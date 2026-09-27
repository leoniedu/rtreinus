#' Quality-check thresholds
#'
#' Each was set from a fault seen in real data and then checked against every
#' trace in that session, so that no rule fires on a healthy one. A rule that
#' flags good data is worse than no rule.
#'
#' Two of these look surprising and are deliberate:
#'
#' * There is no "share of repeated samples" rule. It reads 93% on a device
#'   stuck at one value, but healthy 1 Hz traces reach 70%, which is too close
#'   to separate. The run-length rule alone does the job.
#' * `hr_noise_bpm_s` is per **second**, not per sample. Sampling intervals
#'   differ sixfold between devices, so a per-sample threshold fires on every
#'   slow logger.
#'
#' There is also no "cadence ceiling" rule, though a device that saturates at a
#' low stroke rate is a real fault. Every formulation tried against the
#' reference session flagged most of the crew: comparing a maximum against the
#' crew's p90 makes almost everyone a positive by construction, because p90 of
#' five values sits just under the largest. The one genuine case is caught by
#' the dropout rule anyway.
#'
#' @return A named list of thresholds.
#' @export
treinus_quality_thresholds <- function() {
  list(
    hr_flat_run_s = 300,      # longest run of one value
    hr_low_bpm = 100,         # "never acquired": implausibly low while working
    hr_low_kmh = 8,           # boat speed that makes it implausible
    hr_low_min = 10,          # for at least this long
    hr_noise_bpm_s = 3,       # per second, not per sample
    hr_noise_pct = 10,
    hr_spike_gap = 20,        # max - p99
    cad_low_spm = 20,
    cad_low_pct = 10,         # measured in-block only
    steerer_gap_spm = 2.0,    # how much better a rival candidate must look
    steerer_min_bins = 20,
    aborted_s = 300
  )
}


#' Flag badly measured traces
#'
#' Two rules need context beyond the records: "heart rate never acquired"
#' compares against boat speed, and cadence dropout is only meaningful inside
#' the blocks. Those inputs are passed explicitly rather than recomputed, so
#' this stays a plain function of its arguments.
#'
#' @param records Prepared records.
#' @param boat_speed Output of [treinus_boat_speed()], or `NULL` to skip the
#'   rules that need it.
#' @param pieces Output of [treinus_pieces()], or `NULL`.
#' @param thresholds See [treinus_quality_thresholds()].
#'
#' @return One row per flag raised: athlete, metric, rule, evidence, and
#'   `action`, which is `"drop"` for faults that invalidate a metric and
#'   `"flag"` for ones that only change how it should be read.
#' @export
treinus_data_quality <- function(records,
                                 boat_speed = NULL,
                                 pieces = NULL,
                                 thresholds = treinus_quality_thresholds()) {
  th <- thresholds
  who <- dplyr::distinct(records, .data$id_athlete, .data$fullname_athlete,
                         .data$crew, .data$is_steerer)

  out <- list()

  # --- heart rate ------------------------------------------------------------
  hr <- records |>
    dplyr::filter(!is.na(.data$heart_rate)) |>
    dplyr::summarise(
      flat_run_s = longest_flat_run(.data$heart_rate, .data$ts),
      noise_pct = 100 * mean(
        abs(diff(.data$heart_rate)) / pmax(diff(as.numeric(.data$ts)), 1) >
          th$hr_noise_bpm_s, na.rm = TRUE),
      spike_gap = safe_max(.data$heart_rate) -
        safe_quantile(.data$heart_rate, 0.99),
      .by = c("id_athlete")
    )

  out$flat <- hr |>
    dplyr::filter(.data$flat_run_s >= th$hr_flat_run_s) |>
    dplyr::transmute(.data$id_athlete, metric = "heart_rate",
                     rule = "hr_flatline", action = "drop",
                     evidence = sprintf("%.0f s travados num \u00fanico valor",
                                        .data$flat_run_s))

  out$noise <- hr |>
    dplyr::filter(.data$noise_pct > th$hr_noise_pct) |>
    dplyr::transmute(.data$id_athlete, metric = "heart_rate",
                     rule = "hr_noise", action = "flag",
                     evidence = sprintf("%.1f%% das amostras variam >%d bpm/s",
                                        .data$noise_pct, th$hr_noise_bpm_s))

  out$spike <- hr |>
    dplyr::filter(.data$spike_gap > th$hr_spike_gap) |>
    dplyr::transmute(.data$id_athlete, metric = "heart_rate",
                     rule = "hr_spike", action = "flag",
                     evidence = sprintf("m\u00e1ximo %.0f bpm acima do p99",
                                        .data$spike_gap))

  if (!is.null(boat_speed)) {
    out$never <- hr_never_acquired(records, boat_speed, th)
  }

  # --- cadence ---------------------------------------------------------------
  if (!is.null(pieces)) {
    in_block <- records |>
      dplyr::filter(!is.na(.data$cadence)) |>
      dplyr::mutate(bloco = which_piece(.data$crew, .data$ts, pieces)) |>
      dplyr::filter(!is.na(.data$bloco))

    if (nrow(in_block)) {
      cad <- in_block |>
        dplyr::summarise(
          low_pct = 100 * mean(.data$cadence <= th$cad_low_spm),
          cad_max = safe_max(.data$cadence),
          .by = c("crew", "id_athlete", "is_steerer")
        )

      # Steerers are exempt: they stop paddling to steer, so a low reading is
      # their job, not a fault.
      out$dropout <- cad |>
        dplyr::filter(!.data$is_steerer, .data$low_pct > th$cad_low_pct) |>
        dplyr::transmute(.data$id_athlete, metric = "cadence",
                         rule = "cadence_dropout", action = "flag",
                         evidence = sprintf(
                           "%.1f%% das leituras no bloco <=%d spm",
                           .data$low_pct, th$cad_low_spm))

    }
  }

  res <- purrr::list_rbind(out)
  if (!nrow(res)) return(empty_quality())

  res |>
    dplyr::left_join(who, by = "id_athlete") |>
    dplyr::select("crew", "id_athlete", "fullname_athlete", "metric", "rule",
                  "action", "evidence") |>
    dplyr::arrange(dplyr::desc(.data$action == "drop"), .data$crew,
                   .data$id_athlete)
}


#' Recordings too short or too implausible to analyse
#'
#' @param exercises Exercises tibble, as [treinus_get_exercises_db()] returns.
#' @param thresholds See [treinus_quality_thresholds()].
#' @return Exercises that should be dropped, with the reason.
#' @export
treinus_aborted_exercises <- function(exercises,
                                      thresholds = treinus_quality_thresholds()) {
  exercises |>
    dplyr::mutate(
      implausible_kmh = .data$distance / (.data$total_time / 3600),
      reason = dplyr::case_when(
        .data$total_time < thresholds$aborted_s ~
          sprintf("apenas %.0f s de grava\u00e7\u00e3o", .data$total_time),
        .data$implausible_kmh > 40 ~
          sprintf("%.0f km em %.0f s", .data$distance, .data$total_time),
        is.na(.data$speed) ~ "sem velocidade registrada",
        .default = NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$reason)) |>
    dplyr::select(dplyr::any_of(c("id_athlete", "id_exercise", "total_time",
                                  "distance", "reason")))
}


# -- helpers ------------------------------------------------------------------

#' Longest stretch, in seconds, over which a value never changed
#' @keywords internal
longest_flat_run <- function(x, ts) {
  if (length(x) < 2) return(0)
  t <- as.numeric(ts)
  rl <- rle(x)
  end <- cumsum(rl$lengths)
  start <- end - rl$lengths + 1L
  max(t[end] - t[start])
}

#' Heart rate implausibly low while the boat is working
#' @keywords internal
hr_never_acquired <- function(records, boat_speed, th) {
  fast <- boat_speed |>
    dplyr::filter(.data$kmh_60s > th$hr_low_kmh) |>
    dplyr::select("crew", "tgrid")

  if (!nrow(fast)) return(NULL)
  bin_s <- as.numeric(stats::median(diff(sort(unique(boat_speed$tgrid)))))
  if (!is.finite(bin_s) || bin_s <= 0) bin_s <- 10
  tz <- attr(records$ts, "tzone") %||% "UTC"

  records |>
    dplyr::filter(!is.na(.data$heart_rate)) |>
    dplyr::mutate(tgrid = bin_time(.data$ts, bin_s, tz)) |>
    dplyr::inner_join(fast, by = c("crew", "tgrid")) |>
    dplyr::summarise(
      low_min = sum(.data$dt[.data$heart_rate < th$hr_low_bpm]) / 60,
      med_bpm = safe_median(.data$heart_rate[.data$heart_rate < th$hr_low_bpm]),
      .by = "id_athlete"
    ) |>
    dplyr::filter(.data$low_min > th$hr_low_min) |>
    dplyr::transmute(.data$id_athlete, metric = "heart_rate",
                     rule = "hr_never_acquired", action = "drop",
                     evidence = sprintf(
                       "%.0f min abaixo de %d bpm (mediana %.0f) com a canoa acima de %d km/h",
                       .data$low_min, th$hr_low_bpm, .data$med_bpm,
                       th$hr_low_kmh))
}

empty_quality <- function() {
  tibble::tibble(crew = character(), id_athlete = integer(),
                 fullname_athlete = character(), metric = character(),
                 rule = character(), action = character(),
                 evidence = character())
}


#' Does the cadence evidence agree with who is marked as steerer?
#'
#' A steerer stays in time with the crew and never paddles at a higher rate, but
#' pokes remove strokes from their count, so their measured cadence sits below
#' the crew's. Steve West, *Outrigger Canoeing - The Art and Skill of Steering*
#' (Kanu Culture, 7th ed. 2014), pp. 22, 31, 86, 89.
#'
#' The paddler with the largest shortfall against the rest of the crew is
#' therefore the likeliest steerer. When that is not the paddler on the label,
#' something is wrong: the label, or that person's cadence trace.
#'
#' A threshold on the labelled steerer's own deviation was tried first and
#' abandoned. Where a crew paddles tightly together, every candidate sits within
#' a stroke of the others, so a wrong label shifts the line by half a stroke and
#' no threshold separates it from noise. Comparing candidates against each other
#' has the power that comparing one against an absolute value does not.
#'
#' @param records Prepared records.
#' @param pieces Output of [treinus_pieces()].
#' @param settings Settings list.
#' @param ignore Athletes whose cadence was flagged; a device that under-counts
#'   strokes imitates heavy poking exactly, so leaving one in produces a
#'   confident wrong answer.
#' @param thresholds See [treinus_quality_thresholds()].
#'
#' @return One row per crew where the evidence disagrees with the label, naming
#'   both candidates. Empty when they agree, or when there is too little clean
#'   cadence to judge.
#' @export
treinus_check_steerer <- function(records, pieces, settings,
                                  ignore = integer(),
                                  thresholds = treinus_quality_thresholds()) {
  th <- thresholds
  ranked <- treinus_steerer_by_cadence(records, pieces, settings,
                                       ignore = ignore)
  if (!nrow(ranked)) return(empty_steerer_check())

  best <- ranked |>
    dplyr::filter(.data$rank == 1L, .data$n >= th$steerer_min_bins)
  labelled <- ranked |>
    dplyr::filter(.data$is_steerer, .data$n >= th$steerer_min_bins)

  dplyr::inner_join(
    dplyr::select(best, "crew", best_id = "id_athlete",
                  best_name = "fullname_athlete", best_spm = "deficit_spm"),
    dplyr::select(labelled, "crew", helm_id = "id_athlete",
                  helm_name = "fullname_athlete", helm_spm = "deficit_spm"),
    by = "crew"
  ) |>
    dplyr::filter(.data$best_id != .data$helm_id,
                  .data$helm_spm - .data$best_spm > th$steerer_gap_spm) |>
    dplyr::transmute(
      .data$crew, .data$helm_id, .data$helm_name, .data$best_id,
      .data$best_name,
      gap_spm = round(.data$helm_spm - .data$best_spm, 1),
      evidence = sprintf(
        "%s fica %.1f spm abaixo da tripula\u00e7\u00e3o; %s fica %+.1f: a cad\u00eancia aponta outro leme",
        .data$best_name, -.data$best_spm, .data$helm_name, .data$helm_spm)
    )
}

empty_steerer_check <- function() {
  tibble::tibble(crew = character(), helm_id = integer(),
                 helm_name = character(), best_id = integer(),
                 best_name = character(), gap_spm = numeric(),
                 evidence = character())
}
