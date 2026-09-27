#' Colours for crews and for paddlers within a crew
#'
#' Crew colours are assigned in a fixed order. Paddler colours repeat between
#' crews on purpose: the panel says which crew, and every line is labelled, so
#' identity never rests on colour alone.
#'
#' @return A character vector of hex colours.
#' @export
treinus_palette <- function() {
  c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#4a3aa7",
    "#e87ba4", "#008300", "#e34948")
}

#' A plain theme for session figures
#' @param base_size Base font size.
#' @return A ggplot2 theme.
#' @export
theme_treinus <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.25,
                                               colour = "grey88"),
      axis.title = ggplot2::element_text(colour = "grey30"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey35"),
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    )
}

#' @keywords internal
crew_colours <- function(crews) {
  stats::setNames(treinus_palette()[seq_along(crews)], crews)
}

#' Assign a colour to each paddler within their crew, steerer last
#' @keywords internal
athlete_colours <- function(records) {
  pal <- treinus_palette()
  records |>
    dplyr::distinct(.data$crew, .data$id_athlete, .data$fullname_athlete,
                    .data$is_steerer) |>
    dplyr::arrange(.data$crew, .data$is_steerer, .data$fullname_athlete) |>
    dplyr::mutate(
      cor = pal[(dplyr::row_number() - 1L) %% length(pal) + 1L],
      .by = "crew"
    )
}

#' Short label for a line end
#'
#' First name, plus a surname initial when two people in the same crew share it.
#' @keywords internal
short_name <- function(fullname, crew) {
  first <- sub(" .*", "", fullname)
  dup <- stats::ave(seq_along(first), crew, first, FUN = length) > 1
  initial <- substr(sub("^\\S+\\s+", "", fullname), 1, 1)
  dplyr::if_else(dup, paste0(first, " ", initial, "."), first)
}

#' @keywords internal
label_ends <- function(d, time_col, group_cols) {
  lab <- dplyr::slice_max(d, .data[[time_col]], n = 1, by = dplyr::all_of(group_cols))
  dplyr::mutate(lab, nome = short_name(.data$fullname_athlete, .data$crew))
}

#' @keywords internal
end_labels_layer <- function(data, size = 3) {
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      data = data, mapping = ggplot2::aes(label = .data$nome), size = size,
      hjust = 0, direction = "y", nudge_x = 120, segment.size = 0.2,
      segment.colour = "grey70", min.segment.length = 0, box.padding = 0.15,
      show.legend = FALSE
    )
  } else {
    ggplot2::geom_text(
      data = data, mapping = ggplot2::aes(label = .data$nome), size = size,
      hjust = -0.15, show.legend = FALSE
    )
  }
}


#' Speed of each crew over time
#'
#' @param boat_speed Output of [treinus_boat_speed()].
#' @param title,subtitle Plot text.
#' @return A ggplot object.
#' @export
treinus_plot_speed <- function(boat_speed,
                               title = "Velocidade das canoas",
                               subtitle = paste("Mediana da tripula\u00e7\u00e3o,",
                                                "m\u00e9dia m\u00f3vel de 60 s")) {
  cols <- crew_colours(sort(unique(boat_speed$crew)))
  ends <- dplyr::slice_max(boat_speed, .data$tgrid, n = 1, by = "crew")

  ggplot2::ggplot(boat_speed,
                  ggplot2::aes(.data$tgrid, .data$kmh_60s, colour = .data$crew)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_text(data = ends, ggplot2::aes(label = .data$crew),
                       hjust = -0.1, size = 3.2, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min",
                              expand = ggplot2::expansion(mult = c(0.01, 0.12))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "km/h") +
    theme_treinus()
}


#' Signed speed difference between two crews
#'
#' @param gap Output of [treinus_crew_gap()].
#' @param a,b Crew names, as passed to [treinus_crew_gap()].
#' @return A ggplot object.
#' @export
treinus_plot_gap <- function(gap, a, b) {
  cols <- crew_colours(c(a, b))

  ggplot2::ggplot(gap, ggplot2::aes(.data$tgrid, .data$dif)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_area(data = ~ transform(.x, dif = pmax(.x$dif, 0)),
                       fill = cols[[a]], alpha = 0.25) +
    ggplot2::geom_area(data = ~ transform(.x, dif = pmin(.x$dif, 0)),
                       fill = cols[[b]], alpha = 0.25) +
    ggplot2::geom_line(linewidth = 0.5, colour = "grey25") +
    ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min") +
    ggplot2::labs(
      title = paste(a, "menos", b),
      subtitle = paste0("Acima de zero a ", a, " est\u00e1 mais r\u00e1pida; abaixo, a ", b),
      x = NULL, y = "diferen\u00e7a de velocidade (km/h)"
    ) +
    theme_treinus()
}


#' Heart rate of each paddler, one panel per crew
#'
#' @param records Prepared records.
#' @param smooth_s Centred smoothing window in seconds. Default 30.
#' @return A ggplot object.
#' @export
treinus_plot_hr <- function(records, smooth_s = 30) {
  d <- records |>
    dplyr::filter(!is.na(.data$heart_rate)) |>
    dplyr::arrange(.data$id_athlete, .data$ts) |>
    dplyr::mutate(
      fc = slider::slide_index_dbl(
        .data$heart_rate, .data$ts, mean, na.rm = TRUE,
        .before = lubridate::dseconds(smooth_s / 2),
        .after = lubridate::dseconds(smooth_s / 2)),
      .by = "id_athlete"
    ) |>
    dplyr::inner_join(athlete_colours(records),
                      by = c("crew", "id_athlete", "fullname_athlete",
                             "is_steerer"))

  ggplot2::ggplot(d, ggplot2::aes(.data$ts, .data$fc, colour = .data$cor,
                                  group = .data$id_athlete)) +
    ggplot2::geom_line(linewidth = 0.5) +
    end_labels_layer(label_ends(d, "ts", c("crew", "fullname_athlete"))) +
    ggplot2::facet_wrap(~crew, ncol = 1) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min",
                              expand = ggplot2::expansion(mult = c(0.01, 0.13))) +
    ggplot2::labs(title = "Frequ\u00eancia card\u00edaca por remador",
                  subtitle = paste0("M\u00e9dia m\u00f3vel de ", smooth_s,
                                    " s centrada"),
                  x = NULL, y = "bpm") +
    theme_treinus() +
    ggplot2::theme(legend.position = "none")
}


#' Cadence of the steerer against the crew's stroke line
#'
#' @param cadence_line Output of [treinus_cadence_line()].
#' @return A ggplot object.
#' @export
treinus_plot_steerer_cadence <- function(cadence_line) {
  d <- dplyr::filter(cadence_line, .data$is_steerer)
  if (!nrow(d)) {
    cli::cli_warn("No steerer cadence to plot.")
    return(ggplot2::ggplot() + theme_treinus())
  }
  cols <- crew_colours(sort(unique(d$crew)))
  ends <- label_ends(d, "tgrid", c("crew", "fullname_athlete"))

  ggplot2::ggplot(d, ggplot2::aes(.data$tgrid, .data$desvio_suave,
                                  colour = .data$crew)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_line(ggplot2::aes(group = interaction(.data$crew,
                                                        .data$bloco)),
                       linewidth = 0.7) +
    end_labels_layer(ends, size = 3.2) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min",
                              expand = ggplot2::expansion(mult = c(0.01, 0.12))) +
    ggplot2::labs(
      title = "Cad\u00eancia do leme em rela\u00e7\u00e3o \u00e0 linha da canoa",
      subtitle = "Leme menos a mediana de quem est\u00e1 na remada, durante os blocos",
      x = NULL, y = "desvio do leme (spm)"
    ) +
    theme_treinus()
}


#' Cadence deviation of every paddler, one panel per crew
#'
#' @param cadence_line Output of [treinus_cadence_line()].
#' @param clip_spm Vertical limits. Deviations outside are drawn at the edge.
#' @return A ggplot object.
#' @export
treinus_plot_cadence <- function(cadence_line, clip_spm = c(-16, 16)) {
  d <- cadence_line |>
    dplyr::mutate(cor = NULL) |>
    dplyr::inner_join(
      athlete_colours(cadence_line),
      by = c("crew", "id_athlete", "fullname_athlete", "is_steerer")
    )
  ends <- label_ends(d, "tgrid", c("crew", "fullname_athlete"))

  ggplot2::ggplot(d, ggplot2::aes(.data$tgrid, .data$desvio_suave,
                                  colour = .data$cor,
                                  group = interaction(.data$id_athlete,
                                                      .data$bloco))) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.45) +
    end_labels_layer(ends) +
    ggplot2::facet_wrap(~crew, ncol = 1) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(ylim = clip_spm) +
    ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min",
                              expand = ggplot2::expansion(mult = c(0.01, 0.13))) +
    ggplot2::labs(title = "Cad\u00eancia em rela\u00e7\u00e3o \u00e0 linha da canoa",
                  subtitle = "Zero \u00e9 a remada da tripula\u00e7\u00e3o",
                  x = NULL, y = "desvio (spm)") +
    theme_treinus() +
    ggplot2::theme(legend.position = "none")
}


#' Tracks coloured by detected crew
#'
#' The map the analyst confirms crew membership against.
#'
#' @param records Records with a corrected `ts`.
#' @param crews Tibble of `id_athlete` and `crew`.
#' @return A ggplot object.
#' @export
treinus_plot_crew_map <- function(records, crews) {
  d <- treinus_position_grid(records) |>
    dplyr::left_join(crews, by = "id_athlete") |>
    dplyr::mutate(crew = dplyr::coalesce(.data$crew, "sem tripula\u00e7\u00e3o"))
  cols <- crew_colours(sort(unique(d$crew)))

  ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y, colour = .data$crew,
                                  group = .data$id_athlete)) +
    ggplot2::geom_path(linewidth = 0.4, alpha = 0.8) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Trajetos por tripula\u00e7\u00e3o", x = NULL, y = NULL) +
    theme_treinus() +
    ggplot2::theme(axis.text = ggplot2::element_blank())
}


#' How far the steerer sits behind the rest of the crew
#'
#' Deliberately not a seating diagram. Per-device GPS bias is comparable to the
#' spacing between seats, so only the stern separates reliably from the pack;
#' see [treinus_detect_steerer()]. The chart shows that separation and nothing
#' finer.
#'
#' @param offsets The `offsets` element of [treinus_detect_steerer()].
#' @param steerer The `steerer` element, used to mark the proposal and whether
#'   it is confident.
#' @return A ggplot object.
#' @export
treinus_plot_stern <- function(offsets, steerer = NULL) {
  d <- offsets
  d$papel <- dplyr::if_else(d$is_stern, "popa (leme proposto)", "linha")

  note <- paste0(
    "Apenas a separa\u00e7\u00e3o da popa \u00e9 confi\u00e1vel.\n",
    "O vi\u00e9s de GPS de cada rel\u00f3gio \u00e9 da ordem do espa\u00e7o ",
    "entre bancos, ent\u00e3o a posi\u00e7\u00e3o dos demais n\u00e3o diz ",
    "quem senta onde.")

  if (!is.null(steerer)) {
    lab <- steerer |>
      dplyr::mutate(txt = sprintf("%+.1f m \u00e0 frente do 2\u00ba; %s",
                                  .data$margin_m,
                                  dplyr::if_else(.data$confident,
                                                 "confi\u00e1vel",
                                                 "incerto")))
    d <- dplyr::left_join(d, dplyr::select(lab, "crew", "txt"), by = "crew")
  }

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$along_m, .data$crew,
                                       colour = .data$papel)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$crew), colour = "grey85",
                       linewidth = 1.2) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_colour_manual(values = c("popa (leme proposto)" = "#eb6834",
                                            "linha" = "#2a78d6")) +
    ggplot2::labs(title = "Quem est\u00e1 na popa",
                  subtitle = note,
                  x = "metros ao longo do casco (menor = mais atr\u00e1s)",
                  y = NULL) +
    theme_treinus()

  if (!is.null(steerer)) {
    p <- p + ggplot2::geom_text(
      data = dplyr::distinct(d, .data$crew, .data$txt),
      ggplot2::aes(x = -Inf, y = .data$crew, label = .data$txt),
      hjust = -0.05, vjust = -1.2, size = 3, colour = "grey35",
      inherit.aes = FALSE
    )
  }
  p
}
