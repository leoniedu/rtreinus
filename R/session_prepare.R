#' Apply settings to raw records
#'
#' Fixes clocks, drops excluded exercises and crews, labels each row with its
#' crew and role, computes the capped inter-sample interval, and blanks excluded
#' metrics.
#'
#' Exclusion is implemented by setting the metric to `NA`, never by dropping
#' rows. Every measure carries `na.rm`, so nothing downstream needs to know that
#' exclusions exist, and the `n` in each table shrinks on its own. Dropping rows
#' would corrupt `dt`, cumulative distance and the speed traces, all of which
#' depend on consecutive samples.
#'
#' @param records Raw records tibble.
#' @param settings A settings list from [treinus_read_settings()].
#'
#' @return A tibble with `ts`, `dt`, `crew`, `is_steerer` and excluded metrics
#'   set to `NA`.
#' @export
treinus_prepare_records <- function(records, settings) {
  settings <- treinus_validate_settings(settings)
  a <- settings$analysis

  crews <- crew_table(settings)
  if (!nrow(crews)) {
    cli::cli_abort("No crews to analyse; check {.field crews$include}.")
  }

  out <- records |>
    dplyr::filter(!.data$id_exercise %in% settings$exercises$exclude) |>
    treinus_fix_clock(local = settings$clock$local, tz = settings$session$tz) |>
    dplyr::inner_join(crews, by = "id_athlete")

  if (!nrow(out)) {
    cli::cli_abort("No records left after applying crews and exclusions.")
  }

  # dt is the honest inter-sample interval, capped so that auto-pause gaps are
  # not charged to the session.
  out <- out |>
    dplyr::mutate(
      dt = pmin(c(0, diff(as.numeric(.data$ts))), a$max_sample_gap_s),
      .by = c("id_athlete", "id_exercise")
    )

  for (i in seq_len(nrow(settings$exclude))) {
    who <- settings$exclude$athlete[i]
    what <- settings$exclude$metric[i]
    out[[what]][out$id_athlete == who] <- NA
  }

  out
}


#' Crew and role table implied by settings
#' @keywords internal
crew_table <- function(settings) {
  include <- settings$crews$include
  members <- settings$crews$members[include]
  steerer <- settings$crews$steerer

  purrr::imap(members, function(ids, crew_name) {
    ids <- as.integer(ids)
    helm <- as.integer(steerer[[crew_name]] %||% integer())
    tibble::tibble(
      id_athlete = ids,
      crew = crew_name,
      is_steerer = ids %in% helm
    )
  }) |>
    purrr::list_rbind()
}


#' What an exclusion would cost
#'
#' Reports how much valid data a proposed exclusion discards, and whether it
#' would knock a crew below the minimum number of paddlers needed for a stroke
#' line. That second consequence is easy to miss: accepting both cadence flags
#' on a five-person crew can remove its cadence analysis entirely.
#'
#' @param records Prepared records, before the exclusion is applied.
#' @param athlete Athlete id.
#' @param metric Metric name.
#' @param settings Settings list, used for the minimum line size.
#'
#' @return A one-row tibble: `valid_minutes` discarded and `breaks_line`.
#' @export
treinus_exclusion_cost <- function(records, athlete, metric, settings) {
  settings <- treinus_validate_settings(settings)
  min_line <- settings$analysis$cadence_min_on_line

  their <- records[records$id_athlete == athlete, ]
  valid_s <- sum(their$dt[!is.na(their[[metric]])], na.rm = TRUE)

  breaks_line <- FALSE
  if (identical(metric, "cadence") && nrow(their)) {
    crew <- their$crew[1]
    in_crew <- records[records$crew == crew, ]
    on_line <- unique(in_crew$id_athlete[!in_crew$is_steerer])
    already_gone <- vapply(on_line, function(id) {
      all(is.na(in_crew$cadence[in_crew$id_athlete == id]))
    }, logical(1))
    remaining <- sum(!already_gone) - as.integer(athlete %in% on_line)
    breaks_line <- remaining < min_line
  }

  tibble::tibble(
    athlete = as.integer(athlete),
    metric = metric,
    valid_minutes = round(valid_s / 60, 1),
    breaks_line = breaks_line
  )
}
