# Device clocks.
#
# Garmin watches record either UTC or local time depending on how they were
# set up, and a squad's devices disagree. Everything downstream - which
# exercises belong to one session, who was on the water at the same moment -
# depends on getting them onto one clock first, so this is the only part of
# the session analysis that stayed here: the analysis itself lives in
# yachtvaa, but reading the API means fixing its clocks.

#' Detect which devices store local time
#'
#' Devices disagree about whether they write UTC or local time, and the
#' difference is a whole number of hours.
#'
#' Given the exercises table this is exact: `start_time_as_string` is the local
#' start, so comparing it against the first raw record measures each device's
#' offset directly. Without it the function falls back on clustering the first
#' record of each athlete, which assumes the whole squad trained together.
#' **That assumption fails on any day with more than one training**: a morning
#' crew and a midday crew look exactly like two clocks three hours apart. The
#' fallback warns, and its answer should be checked rather than trusted.
#'
#' @param records Records tibble with `id_athlete` and a character or POSIXct
#'   `timestamp`.
#' @param exercises Optional exercises table, as [treinus_get_exercises_db()]
#'   returns. When supplied, detection is exact.
#' @param tz_offset_hours Hours to add to UTC to get local time. Negative for
#'   the Americas. Default -3 (America/Bahia).
#' @param tolerance_min Used only by the fallback: athletes starting within this
#'   many minutes of each other are treated as sharing a clock. Default 45.
#'
#' @return A tibble with one row per athlete: `id_athlete`, `first_ts`,
#'   `offset_hours` (hours to add to their raw timestamps), `is_local` and
#'   `method`, which is `"exercises"` or `"clustered"`.
#'
#' @examples
#' \dontrun{
#' treinus_detect_clock(records, treinus_get_exercises_db())
#' }
#' @export
treinus_detect_clock <- function(records,
                                 exercises = NULL,
                                 tz_offset_hours = -3,
                                 tolerance_min = 45) {
  first <- records |>
    dplyr::mutate(.ts = as_treinus_time(.data$timestamp)) |>
    dplyr::summarise(first_ts = min(.data$.ts, na.rm = TRUE),
                     .by = c("id_athlete", "id_exercise"))

  if (!is.null(exercises)) {
    out <- clock_from_exercises(first, exercises, tz_offset_hours)
    if (!is.null(out)) return(out)
    cli::cli_warn(c(
      "The exercises table could not settle the clocks for every athlete.",
      "i" = "Either no exercise matched, or a recording does not begin at its
             reported start, which makes the difference unusable as an offset."
    ))
  }

  # Always warn: the fallback is wrong on any day with more than one training,
  # and the caller has no other signal that it was used. On a day with a
  # morning crew and a midday crew it reported nine local-time devices where
  # there were two.
  cli::cli_warn(c(
    "Guessing the clock offsets by clustering start times.",
    "i" = "This assumes the whole squad trained together; a second training
           three hours later is indistinguishable from a second clock.",
    "i" = "Pass {.arg exercises} to measure the offsets instead."
  ))
  clock_by_clustering(first, tz_offset_hours, tolerance_min)
}


#' Exact offsets, by comparing raw records against the reported local start
#' @keywords internal
clock_from_exercises <- function(first, exercises, tz_offset_hours) {
  need <- c("id_exercise", "start_time_as_string")
  if (!all(need %in% names(exercises))) return(NULL)

  ref <- exercises |>
    dplyr::distinct(.data$id_exercise, .data$id_athlete,
                    .data$start_time_as_string)

  joined <- dplyr::inner_join(first, ref, by = c("id_athlete", "id_exercise"))
  if (!nrow(joined)) return(NULL)

  out <- joined |>
    dplyr::mutate(
      local_start = as.numeric(hms_seconds(.data$start_time_as_string)),
      raw_start = as.numeric(format(.data$first_ts, "%H")) * 3600 +
        as.numeric(format(.data$first_ts, "%M")) * 60,
      # Offset is a whole number of hours, and the day may wrap.
      raw_h = ((.data$raw_start - .data$local_start) / 3600 + 12) %% 24 - 12,
      diff_h = round(.data$raw_h),
      # The first record should land on the reported start. When it does not -
      # a trimmed snapshot, or a watch started late - the difference is not a
      # clock offset and rounding it invents one: 32 minutes late becomes an
      # hour. Only near-whole-hour differences are evidence.
      trusted = abs(.data$raw_h - .data$diff_h) < 0.25
    ) |>
    dplyr::filter(.data$trusted) |>
    dplyr::summarise(
      first_ts = min(.data$first_ts),
      diff_h = stats::median(.data$diff_h),
      .by = "id_athlete"
    ) |>
    dplyr::mutate(
      is_local = .data$diff_h == 0,
      offset_hours = dplyr::if_else(.data$is_local, 0, tz_offset_hours),
      method = "exercises"
    ) |>
    dplyr::select("id_athlete", "first_ts", "offset_hours", "is_local",
                  "method")

  # Every athlete has to be judged, or the caller gets a silent gap.
  if (!nrow(out) || !setequal(out$id_athlete, unique(first$id_athlete))) {
    return(NULL)
  }
  out
}


#' Fallback: cluster the first record of each athlete
#' @keywords internal
clock_by_clustering <- function(first, tz_offset_hours, tolerance_min) {
  first <- dplyr::summarise(first, first_ts = min(.data$first_ts),
                            .by = "id_athlete")

  ref <- stats::median(as.numeric(first$first_ts))
  hours <- round((as.numeric(first$first_ts) - ref) / 3600)
  within <- abs(as.numeric(first$first_ts) - ref) <= tolerance_min * 60
  hours[within] <- 0
  majority_is_utc <- sum(within) >= sum(!within)

  first |>
    dplyr::mutate(
      .lag = hours,
      is_local = if (majority_is_utc) .data$.lag != 0 else .data$.lag == 0,
      offset_hours = dplyr::if_else(.data$is_local, 0, tz_offset_hours),
      method = "clustered"
    ) |>
    dplyr::select("id_athlete", "first_ts", "offset_hours", "is_local",
                  "method")
}


#' Seconds since midnight from an "HH:MM:SS" string
#' @keywords internal
hms_seconds <- function(x) {
  parts <- strsplit(as.character(x), ":", fixed = TRUE)
  vapply(parts, function(p) {
    p <- suppressWarnings(as.numeric(p))
    if (!length(p) || anyNA(p[1:2])) return(NA_real_)
    p[1] * 3600 + p[2] * 60 + (if (length(p) > 2 && !is.na(p[3])) p[3] else 0)
  }, numeric(1))
}


#' Put every device on the same clock
#'
#' @param records Records tibble.
#' @param local Integer vector of athlete ids whose devices store local time.
#'   The rest are assumed to store UTC and are shifted by `tz_offset_hours`.
#' @param tz Time zone name for the result. Default "America/Bahia".
#' @param tz_offset_hours Hours to add to UTC to get local time. Default -3.
#'
#' @return `records` with a POSIXct `ts` column in `tz`.
#' @export
treinus_fix_clock <- function(records,
                              local = integer(),
                              tz = "America/Bahia",
                              tz_offset_hours = -3) {
  records |>
    dplyr::mutate(
      ts = as_treinus_time(.data$timestamp),
      ts = dplyr::if_else(.data$id_athlete %in% local,
                          .data$ts,
                          .data$ts + tz_offset_hours * 3600),
      ts = lubridate::force_tz(.data$ts, tz)
    ) |>
    dplyr::arrange(.data$id_athlete, .data$id_exercise, .data$ts)
}


#' Parse Treinus record timestamps
#' @keywords internal
as_treinus_time <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  as.POSIXct(x, tz = "UTC")
}
