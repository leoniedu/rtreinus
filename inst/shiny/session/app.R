## Session analysis cockpit.
##
## The analysis proposes crews, steerers and suspect traces; this app is where
## a human accepts or overrides them. It writes a settings file and renders the
## report from it, so nothing decided here is lost.

library(shiny)
library(rtreinus)
library(dplyr)
library(ggplot2)

`%|_%` <- function(x, y) dplyr::coalesce(unname(x), y)

RECORDS <- getOption("rtreinus.session_records", NULL)
EXERCISES <- getOption("rtreinus.session_exercises", NULL)
SOURCE <- getOption("rtreinus.session_source", NA_character_)

ui <- fluidPage(
  title = "Análise de treino",
  tags$style(HTML("
    body { font-family: system-ui, -apple-system, sans-serif; }
    .step-note { color: #555; margin-bottom: 1rem; }
    .flag-drop { color: #b3261e; font-weight: 600; }
  ")),
  titlePanel("Análise de treino"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("step", NULL,
                   choices = c("1. Sessão" = "session",
                               "2. Relógio" = "clock",
                               "3. Tripulações" = "crews",
                               "4. Bancos" = "seats",
                               "5. Qualidade" = "quality",
                               "6. Relatório" = "report")),
      hr(),
      uiOutput("status")
    ),
    mainPanel(width = 9, uiOutput("body"))
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    raw = RECORDS,
    settings = NULL,
    accepted = character(),  # "athlete|metric" keys ticked for dropping
    quality_touched = FALSE  # once true, proposals stop overriding the choice
  )

  observeEvent(rv$raw, {
    req(rv$raw)
    rv$settings <- treinus_draft_settings(rv$raw, EXERCISES, source = SOURCE)
  }, once = TRUE)

  fixed <- reactive({
    req(rv$raw, rv$settings)
    treinus_fix_clock(rv$raw, local = rv$settings$clock$local,
                      tz = rv$settings$session$tz)
  })

  # Everything the analyst reads is keyed by name; ids are the machine's
  # business, not theirs.
  athletes <- reactive({
    req(rv$raw)
    rv$raw |>
      distinct(id_athlete, fullname_athlete) |>
      arrange(fullname_athlete)
  })
  named_ids <- function(ids) {
    a <- athletes()
    ids <- as.integer(ids)
    stats::setNames(ids, a$fullname_athlete[match(ids, a$id_athlete)])
  }
  who <- function(ids) {
    a <- athletes()
    a$fullname_athlete[match(as.integer(ids), a$id_athlete)]
  }

  detected <- reactive(treinus_detect_crews(fixed()))
  seats <- reactive(treinus_detect_steerer(fixed(), detected()$crews))

  prepared <- reactive({
    req(rv$settings)
    tryCatch(treinus_prepare_records(rv$raw, rv$settings),
             error = function(e) NULL)
  })

  # The same records with no metric blanked. Quality has to be judged on what
  # the devices recorded, not on what is left after the analyst has already
  # thrown some of it away: computing flags from the excluded records makes a
  # flag vanish the moment it is accepted, which un-ticks its own checkbox and
  # then restores the flag. The cost of an exclusion has to be measured here
  # too, for the same reason.
  prepared_clean <- reactive({
    req(rv$settings)
    s <- rv$settings
    s$exclude <- list()
    tryCatch(treinus_prepare_records(rv$raw, s), error = function(e) NULL)
  })

  measures <- reactive({
    rec <- prepared()
    req(rec)
    bs <- treinus_boat_speed(rec, rv$settings)
    list(records = rec, boat_speed = bs,
         pieces = treinus_pieces(bs, rv$settings))
  })

  flags <- reactive({
    rec <- prepared_clean()
    req(rec)
    bs <- treinus_boat_speed(rec, rv$settings)
    treinus_data_quality(rec, bs, treinus_pieces(bs, rv$settings))
  })

  output$status <- renderUI({
    s <- rv$settings
    if (is.null(s)) return(helpText("Sem dados carregados."))
    tagList(
      tags$p(tags$b("Tripulações: "), length(s$crews$include)),
      tags$p(tags$b("Exclusões: "),
             if (is.data.frame(s$exclude)) nrow(s$exclude) else length(s$exclude)),
      tags$p(tags$b("Relógio local: "),
             if (length(s$clock$local)) paste(who(s$clock$local), collapse = ", ")
             else "nenhum")
    )
  })

  # -- 1. session -------------------------------------------------------------
  output$ui_session <- renderUI({
    req(rv$raw)
    n <- rv$raw |>
      summarise(n = n(), .by = c("id_athlete", "id_exercise")) |>
      arrange(id_athlete)
    tagList(
      div(class = "step-note",
          "Registros carregados. Marque exercícios a descartar."),
      selectizeInput("drop_ex", "Exercícios a excluir",
                     choices = sort(unique(n$id_exercise)),
                     selected = rv$settings$exercises$exclude,
                     multiple = TRUE, width = "100%"),
      tableOutput("tbl_session")
    )
  })
  output$tbl_session <- renderTable({
    rv$raw |>
      summarise(registros = n(), .by = c("id_athlete", "id_exercise")) |>
      transmute(atleta = who(id_athlete),
                exercicio = as.integer(id_exercise),
                registros = as.integer(registros)) |>
      arrange(atleta)
  })
  # ignoreInit: these inputs live inside renderUI, so at the first flush they
  # are still NULL. Without it the observer fires immediately and overwrites
  # the detected draft with an empty vector, and every proposal is lost before
  # the analyst sees it.
  observeEvent(input$drop_ex, {
    rv$settings$exercises$exclude <- as.integer(input$drop_ex)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # -- 2. clock ---------------------------------------------------------------
  output$ui_clock <- renderUI({
    tagList(
      div(class = "step-note", paste(
        "Alguns relógios gravam hora local e outros UTC, e o mesmo relógio",
        "pode mudar de uma sessão para outra. Com a tabela de exercícios a",
        "diferença é medida diretamente; sem ela, é um palpite que supõe um",
        "único treino no dia.")),
      selectizeInput("local_ids", "Relógios em hora local",
                     choices = named_ids(sort(unique(rv$raw$id_athlete))),
                     selected = rv$settings$clock$local,
                     multiple = TRUE, width = "100%"),
      tableOutput("tbl_clock")
    )
  })
  output$tbl_clock <- renderTable({
    treinus_detect_clock(rv$raw, EXERCISES) |>
      transmute(atleta = who(id_athlete),
                inicio_bruto = format(first_ts, "%H:%M:%S"),
                ajuste_h = offset_hours,
                hora_local = if_else(is_local, "sim", "não"),
                metodo = method) |>
      arrange(atleta)
  })
  observeEvent(input$local_ids, {
    rv$settings$clock$local <- as.integer(input$local_ids)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # -- 3. crews ---------------------------------------------------------------
  output$ui_crews <- renderUI({
    d <- detected()
    warn <- NULL
    sizes <- d$crews |> summarise(n = n(), .by = "crew")
    if (any(sizes$n > 6)) {
      warn <- div(class = "flag-drop", paste(
        "Uma tripulação tem mais de seis lugares: duas canoas que andaram",
        "juntas podem ter sido fundidas."))
    }
    if (length(d$unassigned)) {
      warn <- tagList(warn, div(class = "flag-drop",
        paste("Sem tripulação:", paste(who(d$unassigned), collapse = ", "))))
    }
    tagList(
      div(class = "step-note",
          "Tripulações deduzidas da proximidade no GPS."),
      warn,
      checkboxGroupInput("include", "Analisar",
                         choices = sort(unique(d$crews$crew)),
                         selected = rv$settings$crews$include,
                         inline = TRUE),
      plotOutput("plot_crews", height = "460px"),
      tableOutput("tbl_crews")
    )
  })
  output$plot_crews <- renderPlot(treinus_plot_crew_map(fixed(), detected()$crews))
  output$tbl_crews <- renderTable({
    detected()$crews |>
      mutate(nome = who(id_athlete)) |>
      arrange(crew, nome) |>
      summarise(integrantes = paste(nome, collapse = ", "), .by = "crew")
  })
  observeEvent(input$include, {
    # Assigning NULL to a list element removes it; an empty selection has to
    # become an empty vector so the settings keep the key.
    rv$settings$crews$include <- as.character(input$include %||% character())
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # -- 4. seats ---------------------------------------------------------------
  output$ui_seats <- renderUI({
    s <- seats()
    pickers <- lapply(rv$settings$crews$include, function(cw) {
      members <- rv$settings$crews$members[[cw]]
      prop <- s$steerer$id_athlete[s$steerer$crew == cw]
      conf <- isTRUE(s$steerer$confident[s$steerer$crew == cw])
      tagList(
        selectInput(paste0("steerer_", make.names(cw)), paste("Leme —", cw),
                    choices = named_ids(members),
                    selected = rv$settings$crews$steerer[[cw]] %||% prop),
        if (!conf) div(class = "flag-drop",
                       "Identificação incerta: confirme com a tripulação.")
      )
    })
    tagList(
      div(class = "step-note", paste(
        "O leme vai na popa, então o remador mais atrás é a proposta. Ela só",
        "é dada como certa quando a ordem se mantém nos dois sentidos de",
        "deslocamento e ao longo de toda a sessão. Só a separação da popa é",
        "confiável: o viés de GPS de cada relógio é da ordem do espaço entre",
        "bancos, então a posição dos demais não diz quem senta onde.")),
      do.call(tagList, pickers),
      uiOutput("steerer_cadence_note"),
      plotOutput("plot_seats", height = "300px"),
      tableOutput("tbl_seats"),
      hr(),
      tags$p(tags$b("Segunda evidência: cadência.")),
      helpText(paste(
        "O leme rema em tempo com a tripulação mas nunca mais rápido, e cada",
        "remada de governo tira uma remada da contagem — então a cadência dele",
        "fica abaixo da do resto. Quem tem o maior déficit é o leme mais",
        "provável. Relógios com cadência suspeita ficam de fora, porque um",
        "aparelho que perde remadas imita exatamente um leme que governa muito.")),
      tableOutput("tbl_steerer_cadence")
    )
  })
  output$plot_seats <- renderPlot(treinus_plot_stern(seats()$offsets, seats()$steerer))
  output$tbl_seats <- renderTable({
    seats()$steerer |>
      transmute(crew, leme = who(id_athlete), margem_m = round(margin_m, 2),
                sentidos_ok = consistent, estavel = stable,
                gravando = as.integer(n_recording), confiavel = confident)
  })
  cadence_evidence <- reactive({
    m <- measures()
    req(m)
    f <- flags()
    bad <- unique(f$id_athlete[f$metric == "cadence"])
    list(
      ranked = treinus_steerer_by_cadence(m$records, m$pieces, rv$settings,
                                          ignore = bad),
      disagreement = treinus_check_steerer(m$records, m$pieces, rv$settings,
                                           ignore = bad)
    )
  })

  output$tbl_steerer_cadence <- renderTable({
    e <- cadence_evidence()$ranked
    if (!nrow(e)) return(NULL)
    e |>
      transmute(crew, atleta = fullname_athlete,
                deficit_spm = round(deficit_spm, 1),
                janelas = as.integer(n),
                marcado = if_else(is_steerer, "leme", ""))
  })

  output$steerer_cadence_note <- renderUI({
    d <- cadence_evidence()$disagreement
    if (!nrow(d)) return(NULL)
    tagList(lapply(seq_len(nrow(d)), function(i) {
      div(class = "flag-drop", paste0(d$crew[i], ": ", d$evidence[i]))
    }))
  })

  observe({
    for (cw in rv$settings$crews$include) {
      val <- input[[paste0("steerer_", make.names(cw))]]
      if (!is.null(val)) rv$settings$crews$steerer[[cw]] <- as.integer(val)
    }
  })

  # -- 5. quality -------------------------------------------------------------
  # The screen states one thing: a ticked box means that metric will be thrown
  # away. The rules that propose a drop arrive ticked; the rest arrive unticked
  # and are there to be read, not acted on.

  flag_key <- function(f) paste(f$id_athlete, f$metric, sep = "|")

  METRIC_PT <- c(heart_rate = "frequência cardíaca", cadence = "cadência")
  RULE_PT <- c(
    hr_flatline = "valor travado",
    hr_never_acquired = "sensor não pegou",
    hr_noise = "leitura instável",
    hr_spike = "pico isolado",
    cadence_dropout = "remadas não detectadas"
  )

  # Proposals are ticked the first time the flags are computed, and the
  # analyst's choice is respected from then on.
  observeEvent(flags(), {
    f <- flags()
    if (!nrow(f) || rv$quality_touched) return()
    rv$accepted <- flag_key(f)[f$action == "drop"]
  })

  output$ui_quality <- renderUI({
    f <- flags()
    if (!nrow(f)) {
      return(div(class = "step-note",
                 "Nenhum sinal de medição ruim nesta sessão."))
    }
    rec <- prepared_clean()
    keys <- flag_key(f)
    custo <- purrr::map2_dbl(f$id_athlete, f$metric, function(a, m) {
      treinus_exclusion_cost(rec, a, m, rv$settings)$valid_minutes
    })
    labels <- sprintf(
      "%s — %s (%s) · descarta %.0f min de leituras%s",
      f$fullname_athlete,
      METRIC_PT[f$metric] %|_% f$metric,
      RULE_PT[f$rule] %|_% f$rule,
      custo,
      if_else(f$action == "drop", " · sugerido", "")
    )
    tagList(
      div(class = "step-note", tags$b("Marque o que deve ser descartado.")),
      helpText(paste(
        "Marcado = a métrica daquele atleta sai da análise e aparece vazia no",
        "relatório; o resto dos dados dele continua. O tempo indicado é de",
        "leituras gravadas, não necessariamente boas. Desmarcado = fica como",
        "está. As linhas marcadas como \u201csugerido\u201d já vêm marcadas",
        "porque a medição é inutilizável; as demais são apenas avisos, e valem",
        "mais lidas do que descartadas.")),
      checkboxGroupInput("exclusions", NULL,
                         choices = stats::setNames(keys, labels),
                         selected = rv$accepted, width = "100%"),
      uiOutput("quality_summary"),
      hr(),
      tags$p(tags$b("O que cada regra encontrou")),
      tableOutput("tbl_quality")
    )
  })

  output$quality_summary <- renderUI({
    n <- length(rv$accepted)
    rec <- prepared_clean()

    # The line has to be checked against every accepted exclusion at once.
    # Taken one at a time, two cadence drops can each look harmless while
    # together they leave the crew without enough paddlers to form a line.
    dropped <- do.call(rbind, lapply(rv$accepted, function(k) {
      p <- strsplit(k, "|", fixed = TRUE)[[1]]
      data.frame(athlete = as.integer(p[1]), metric = p[2])
    }))
    breaks <- character()
    if (!is.null(dropped) && any(dropped$metric == "cadence")) {
      gone <- dropped$athlete[dropped$metric == "cadence"]
      min_line <- rv$settings$analysis$cadence_min_on_line
      left <- rec |>
        filter(!is_steerer, !id_athlete %in% gone) |>
        summarise(n = n_distinct(id_athlete), .by = "crew") |>
        filter(n < min_line)
      breaks <- left$crew
    }
    tagList(
      tags$p(if (n == 0) "Nada será descartado."
             else sprintf("%d métrica%s será%s descartada%s.", n,
                          if (n > 1) "s" else "", if (n > 1) "o" else "",
                          if (n > 1) "s" else "")),
      if (length(breaks)) div(class = "flag-drop", paste0(
        "Atenção: com essas exclusões, ", paste(breaks, collapse = " e "),
        " fica sem remadores suficientes para formar a linha, e a análise de",
        " cadência dessa tripulação desaparece do relatório."))
    )
  })

  output$tbl_quality <- renderTable({
    f <- flags()
    f |>
      transmute(atleta = fullname_athlete,
                metrica = METRIC_PT[metric] %|_% metric,
                regra = RULE_PT[rule] %|_% rule,
                evidencia = evidence,
                sugestao = if_else(action == "drop", "descartar", "só avisar"))
  })

  observeEvent(input$exclusions, {
    # Unticking the last box sends NULL, not an empty vector, and strsplit()
    # errors on it. ignoreNULL = FALSE is what makes that case reachable, and
    # it has to stay: clearing every exclusion is a decision the settings must
    # record.
    chosen <- as.character(input$exclusions %||% character())
    rv$quality_touched <- TRUE
    rv$accepted <- chosen
    rv$settings$exclude <- lapply(
      strsplit(chosen, "|", fixed = TRUE),
      \(p) list(athlete = as.integer(p[1]), metric = p[2])
    )
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # -- 6. report --------------------------------------------------------------
  output$ui_report <- renderUI({
    stem <- format(as.Date(rv$settings$session$date), "%Y%m%d")
    tagList(
      div(class = "step-note", "Grave as decisões e, se quiser, gere o relatório."),
      textInput("out_yml", "Arquivo de ajustes",
                value = sprintf("treino_%s.yml", stem), width = "100%"),
      actionButton("save", "Gravar ajustes", class = "btn-primary"),
      uiOutput("save_msg"),
      hr(),
      textInput("out_html", "Relatório", value = sprintf("treino_%s.html", stem),
                width = "100%"),
      textInput("template", "Modelo (vazio = o do pacote)", value = "",
                width = "100%"),
      actionButton("render", "Gravar e gerar relatório"),
      uiOutput("render_msg"),
      hr(),
      tags$pre(paste(yaml::as.yaml(rv$settings), collapse = "\n"))
    )
  })

  save_settings <- function(path) {
    treinus_write_settings(rv$settings, path)
    normalizePath(path)
  }

  observeEvent(input$save, {
    ok <- tryCatch({ p <- save_settings(input$out_yml)
      output$save_msg <- renderUI(div(paste("Gravado em", p))); TRUE },
      error = function(e) {
        output$save_msg <- renderUI(div(class = "flag-drop",
                                        conditionMessage(e))); FALSE })
    invisible(ok)
  })

  observeEvent(input$render, {
    output$render_msg <- renderUI(div("Gerando..."))
    tryCatch({
      yml <- save_settings(input$out_yml)
      tpl <- if (nzchar(input$template)) input$template else NULL
      withProgress(message = "Renderizando o relatório", value = 0.5, {
        out <- treinus_render_report(yml, template = tpl,
                                     output = input$out_html)
      })
      output$render_msg <- renderUI(
        div(paste("Relatório gerado em", normalizePath(out, mustWork = FALSE))))
    }, error = function(e) {
      output$render_msg <- renderUI(div(class = "flag-drop",
                                        conditionMessage(e)))
    })
  })

  output$body <- renderUI({
    switch(input$step,
           session = uiOutput("ui_session"),
           clock = uiOutput("ui_clock"),
           crews = uiOutput("ui_crews"),
           seats = uiOutput("ui_seats"),
           quality = uiOutput("ui_quality"),
           report = uiOutput("ui_report"))
  })
}

shinyApp(ui, server)
