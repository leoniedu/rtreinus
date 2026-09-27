#' Per-athlete session summary
#'
#' @param records Prepared records from [treinus_prepare_records()].
#' @param settings Settings list.
#' @return One row per athlete and exercise.
#' @export
treinus_athlete_summary <- function(records, settings) {
  a <- treinus_validate_settings(settings)$analysis

  records |>
    dplyr::summarise(
      start = min(.data$ts), end = max(.data$ts),
      elapsed_min = as.numeric(difftime(max(.data$ts), min(.data$ts),
                                        units = "mins")),
      km = safe_max(.data$distance) / 1000,
      moving_min = sum(.data$dt[!is.na(.data$speed) &
                                  .data$speed > a$moving_ms]) / 60,
      kmh_max30 = safe_max(slider::slide_index_dbl(
        .data$speed, .data$ts, mean, na.rm = TRUE,
        .before = lubridate::dseconds(30))) * 3.6,
      fc_avg = safe_mean(.data$heart_rate),
      fc_p99 = safe_quantile(.data$heart_rate, 0.99),
      fc_max = safe_max(.data$heart_rate),
      spm_med = safe_median(.data$cadence[.data$cadence > 20]),
      .by = c("crew", "id_athlete", "fullname_athlete", "is_steerer",
              "id_exercise")
    ) |>
    dplyr::mutate(
      kmh_avg = .data$km / (.data$elapsed_min / 60),
      kmh_moving = dplyr::if_else(.data$moving_min > 0,
                                  .data$km / (.data$moving_min / 60),
                                  NA_real_)
    ) |>
    dplyr::arrange(.data$crew, dplyr::desc(.data$km))
}


#' Time spent in each heart-rate zone
#'
#' @param records Prepared records.
#' @param settings Settings list.
#' @return One row per athlete and zone, with minutes and share.
#' @export
treinus_hr_zones <- function(records, settings) {
  breaks <- treinus_validate_settings(settings)$analysis$hr_zones
  cuts <- c(-Inf, breaks, Inf)
  labels <- zone_labels(breaks)

  # Everyone in the crew appears, including those whose heart rate was
  # excluded. Filtering first would drop them from the table with no marker,
  # so the reader could not tell "no data" from "never in this zone".
  roster <- dplyr::distinct(records, .data$crew, .data$id_athlete,
                            .data$fullname_athlete)

  measured <- records |>
    dplyr::filter(!is.na(.data$heart_rate)) |>
    dplyr::mutate(zone = cut(.data$heart_rate, cuts, labels = labels)) |>
    dplyr::summarise(minutes = sum(.data$dt) / 60,
                     .by = c("crew", "id_athlete", "fullname_athlete", "zone")) |>
    dplyr::mutate(pct = 100 * .data$minutes / sum(.data$minutes),
                  .by = "id_athlete")

  dplyr::left_join(
    tidyr::expand_grid(roster, zone = factor(labels, levels = labels)),
    measured, by = c("crew", "id_athlete", "fullname_athlete", "zone")
  ) |>
    dplyr::mutate(no_data = all(is.na(.data$minutes)), .by = "id_athlete")
}

zone_labels <- function(breaks) {
  n <- length(breaks)
  c(paste0("Z1 <", breaks[1]),
    if (n > 1) paste0("Z", seq_len(n - 1) + 1, " ",
                      breaks[-n], "-", breaks[-1]),
    paste0("Z", n + 1, " ", breaks[n], "+"))
}


#' Speed of each crew over time
#'
#' One hull has one speed. Averaging within athlete before taking the median
#' across the crew matters: sampling rates differ sixfold between devices, so a
#' plain median over raw rows is effectively the fastest logger's trace.
#'
#' @param records Prepared records.
#' @param settings Settings list.
#' @return Crew, bin, `kmh` and a smoothed `kmh_60s`.
#' @export
treinus_boat_speed <- function(records, settings) {
  a <- treinus_validate_settings(settings)$analysis
  tz <- attr(records$ts, "tzone") %||% "UTC"

  records |>
    dplyr::filter(!is.na(.data$speed)) |>
    dplyr::mutate(tgrid = bin_time(.data$ts, a$bin_s, tz)) |>
    dplyr::summarise(kmh = mean(.data$speed) * 3.6,
                     .by = c("crew", "id_athlete", "tgrid")) |>
    dplyr::summarise(kmh = stats::median(.data$kmh), n_crew = dplyr::n(),
                     .by = c("crew", "tgrid")) |>
    # A bin where only one member is recording cannot distinguish boat from
    # beach.
    dplyr::filter(.data$n_crew >= 2) |>
    dplyr::arrange(.data$crew, .data$tgrid) |>
    dplyr::mutate(
      kmh_60s = slider::slide_index_dbl(
        .data$kmh, .data$tgrid, mean, na.rm = TRUE,
        .before = lubridate::dseconds(30), .after = lubridate::dseconds(30)),
      .by = "crew"
    )
}


#' Detect the blocks a crew rowed
#'
#' A block is a stretch held at `piece_frac` of that crew's own cruising speed.
#' The threshold has to be relative: an absolute one sits on the cruising speed
#' of the slowest boat, so ordinary variation crosses it repeatedly and splits
#' steady paddling into spurious fragments.
#'
#' @param boat_speed Output of [treinus_boat_speed()].
#' @param settings Settings list.
#' @return One row per block.
#' @export
treinus_pieces <- function(boat_speed, settings) {
  a <- treinus_validate_settings(settings)$analysis

  cruise <- boat_speed |>
    dplyr::filter(.data$kmh_60s > a$piece_cruise_floor_kmh) |>
    dplyr::summarise(cruise = stats::median(.data$kmh_60s), .by = "crew")

  boat_speed |>
    dplyr::inner_join(cruise, by = "crew") |>
    dplyr::arrange(.data$crew, .data$tgrid) |>
    dplyr::mutate(
      on = .data$kmh_60s >= a$piece_frac * .data$cruise,
      # A recording gap breaks the run even when both sides are "on".
      brk = .data$on != dplyr::lag(.data$on, default = dplyr::first(.data$on)) |
        c(FALSE, diff(as.numeric(.data$tgrid)) > a$piece_min_s),
      run = cumsum(.data$brk),
      .by = "crew"
    ) |>
    dplyr::filter(.data$on) |>
    dplyr::summarise(
      start = min(.data$tgrid), end = max(.data$tgrid),
      dur_min = as.numeric(difftime(max(.data$tgrid), min(.data$tgrid),
                                    units = "mins")),
      kmh = mean(.data$kmh_60s), kmh_max = max(.data$kmh_60s),
      .by = c("crew", "run")
    ) |>
    dplyr::filter(.data$dur_min >= a$piece_min_s / 60) |>
    dplyr::mutate(piece = dplyr::row_number(), .by = "crew") |>
    dplyr::select(-"run")
}


#' Kilometre splits
#'
#' Times are interpolated at each whole-kilometre crossing. Bucketing by
#' `floor(distance/1000)` instead charges a partial last bucket and swallows
#' recording gaps.
#'
#' @param records Prepared records.
#' @return One row per athlete and kilometre.
#' @export
treinus_km_splits <- function(records) {
  records |>
    dplyr::reframe(km_crossings(.data$distance, .data$ts, start = TRUE),
                   .by = c("crew", "id_athlete", "fullname_athlete",
                           "id_exercise")) |>
    dplyr::mutate(split_min = (.data$t - dplyr::lag(.data$t)) / 60,
                  .by = c("id_athlete", "id_exercise")) |>
    # The seeded km 0 row exists only to give km 1 something to subtract from.
    dplyr::filter(.data$km_mark > 0, !is.na(.data$split_min))
}

#' @param start Seed a km 0 row at the first recorded position, so that the
#'   first kilometre gets a split like every other one.
#' @keywords internal
km_crossings <- function(distance, ts, start = FALSE) {
  ok <- !is.na(distance) & !is.na(ts)
  distance <- distance[ok]
  ts <- as.numeric(ts)[ok]
  empty <- tibble::tibble(km_mark = integer(), t = numeric())
  if (length(distance) < 2) return(empty)
  # While the boat is stopped the odometer repeats; the first time a distance
  # is reached is the crossing.
  keep <- !duplicated(distance)
  distance <- distance[keep]
  ts <- ts[keep]
  if (length(distance) < 2) return(empty)
  marks <- seq_len(floor(max(distance) / 1000))
  if (!length(marks)) return(empty)
  out <- tibble::tibble(km_mark = marks,
                        t = stats::approx(distance, ts, xout = marks * 1000)$y)
  if (start) {
    out <- dplyr::bind_rows(
      tibble::tibble(km_mark = 0L, t = ts[which.min(distance)]), out)
  }
  out
}


#' Where the time went, to a common reference distance
#'
#' Compares crews over the same odometer distance rather than over whatever each
#' watch happened to record. Note this is the same distance, not the same water:
#' crews may start recording minutes apart.
#'
#' @param records Prepared records.
#' @param settings Settings list.
#' @return One row per athlete who covered the reference distance.
#' @export
treinus_pace_to_distance <- function(records, settings) {
  a <- treinus_validate_settings(settings)$analysis
  ref <- a$reference_distance_m

  records |>
    dplyr::filter(!is.na(.data$distance), .data$distance <= ref) |>
    dplyr::filter(safe_max(.data$distance) >= ref * 0.995,
                  .by = c("crew", "id_athlete")) |>
    dplyr::summarise(
      elapsed_min = as.numeric(difftime(max(.data$ts), min(.data$ts),
                                        units = "mins")),
      moving_min = sum(.data$dt[!is.na(.data$speed) &
                                  .data$speed > a$moving_ms]) / 60,
      stopped_min = sum(.data$dt[!is.na(.data$speed) &
                                   .data$speed <= a$moving_ms]) / 60,
      gap_min = as.numeric(difftime(max(.data$ts), min(.data$ts),
                                    units = "mins")) - sum(.data$dt) / 60,
      .by = c("crew", "id_athlete", "fullname_athlete")
    ) |>
    dplyr::mutate(kmh_moving = (ref / 1000) / (.data$moving_min / 60)) |>
    dplyr::arrange(.data$crew, dplyr::desc(.data$kmh_moving))
}


#' Signed speed difference between two crews
#'
#' @param boat_speed Output of [treinus_boat_speed()].
#' @param a,b Crew names. Positive `dif` means `a` is faster.
#' @return Bin, both speeds and their difference.
#' @export
treinus_crew_gap <- function(boat_speed, a, b) {
  boat_speed |>
    dplyr::filter(.data$crew %in% c(a, b)) |>
    dplyr::select("crew", "tgrid", "kmh_60s") |>
    tidyr::pivot_wider(names_from = "crew", values_from = "kmh_60s") |>
    dplyr::filter(!is.na(.data[[a]]), !is.na(.data[[b]])) |>
    dplyr::mutate(dif = .data[[a]] - .data[[b]])
}


#' Cadence of each paddler relative to the crew's stroke line
#'
#' The line is the median cadence of the paddlers who are on the stroke.
#' Steerers are excluded from it, since they do not follow the rhythm, and the
#' comparison runs only inside detected blocks: during warm-up, turns and stops
#' there is no common rhythm to deviate from, and deviations there reach tens of
#' strokes per minute without meaning anything.
#'
#' @param records Prepared records.
#' @param pieces Output of [treinus_pieces()].
#' @param settings Settings list.
#' @return Per athlete and bin: own rate, the line, raw and smoothed deviation.
#' @export
treinus_cadence_line <- function(records, pieces, settings) {
  a <- treinus_validate_settings(settings)$analysis
  tz <- attr(records$ts, "tzone") %||% "UTC"

  binned <- records |>
    dplyr::filter(!is.na(.data$cadence), .data$cadence > 20) |>
    dplyr::mutate(tgrid = bin_time(.data$ts, a$cadence_bin_s, tz)) |>
    dplyr::summarise(spm = stats::median(.data$cadence),
                     .by = c("crew", "id_athlete", "fullname_athlete",
                             "is_steerer", "tgrid"))

  if (!nrow(binned) || !nrow(pieces)) return(empty_cadence_line())

  binned |>
    dplyr::mutate(bloco = which_piece(.data$crew, .data$tgrid, pieces)) |>
    dplyr::filter(!is.na(.data$bloco)) |>
    # With fewer than three paddlers the median is pulled by any one of them.
    dplyr::filter(sum(!.data$is_steerer) >= a$cadence_min_on_line,
                  .by = c("crew", "tgrid")) |>
    dplyr::mutate(linha = stats::median(.data$spm[!.data$is_steerer]),
                  .by = c("crew", "tgrid")) |>
    dplyr::mutate(desvio = .data$spm - .data$linha) |>
    dplyr::arrange(.data$id_athlete, .data$tgrid) |>
    dplyr::mutate(
      desvio_suave = slider::slide_index_dbl(
        .data$desvio, .data$tgrid, mean, na.rm = TRUE,
        .before = lubridate::dseconds(a$cadence_smooth_s / 2),
        .after = lubridate::dseconds(a$cadence_smooth_s / 2)),
      .by = "id_athlete"
    )
}

empty_cadence_line <- function() {
  tibble::tibble(crew = character(), id_athlete = integer(),
                 fullname_athlete = character(), is_steerer = logical(),
                 tgrid = as.POSIXct(character()), spm = numeric(),
                 bloco = integer(), linha = numeric(), desvio = numeric(),
                 desvio_suave = numeric())
}


#' Summarise how closely each paddler held the line
#'
#' @param cadence_line Output of [treinus_cadence_line()].
#' @param within_spm Tolerance counted as "on the line". Default 1.
#' @return One row per athlete.
#' @export
treinus_cadence_vs_line <- function(cadence_line, within_spm = 1) {
  cadence_line |>
    dplyr::summarise(
      desvio_med = stats::median(.data$desvio),
      desvio_abs = stats::median(abs(.data$desvio)),
      pct_na_linha = 100 * mean(abs(.data$desvio) <= within_spm),
      n = dplyr::n(),
      .by = c("crew", "id_athlete", "fullname_athlete", "is_steerer")
    ) |>
    dplyr::arrange(.data$crew, .data$is_steerer, .data$desvio_abs)
}


#' Fastest straight run of a given distance, per athlete
#'
#' Thin wrapper over the internal `fastest_straight_distance()` that carries
#' crew labels.
#'
#' @param records Prepared records.
#' @param distance_m Distance to search for. Default 1000.
#' @param crs Projected CRS, or `NULL` to derive one.
#' @return One row per athlete, with their crew.
#' @export
treinus_fastest_straight <- function(records, distance_m = 1000, crs = NULL) {
  pts <- records |>
    dplyr::filter(!is.na(.data$position_long)) |>
    dplyr::mutate(lat = .data$position_lat * 180 / 2^31,
                  lon = .data$position_long * 180 / 2^31,
                  timestamp = .data$ts)
  if (is.null(crs)) crs <- utm_crs(stats::median(pts$lon), stats::median(pts$lat))

  sf_pts <- pts |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) |>
    sf::st_transform(crs)

  fastest_straight_distance(
    sf_points = sf_pts, athlete_col = "id_athlete",
    time_col = "timestamp", distance_m = distance_m
  ) |>
    dplyr::left_join(
      dplyr::distinct(sf::st_drop_geometry(sf_pts),
                      .data$id_athlete, .data$crew, .data$fullname_athlete),
      by = "id_athlete"
    )
}


# -- helpers ------------------------------------------------------------------

#' @keywords internal
bin_time <- function(ts, bin_s, tz) {
  as.POSIXct(round(as.numeric(ts) / bin_s) * bin_s,
             origin = "1970-01-01", tz = tz)
}

#' Which block each bin falls in, or NA between blocks
#' @keywords internal
which_piece <- function(crew, tgrid, pieces) {
  purrr::map2_int(crew, tgrid, function(cw, t) {
    p <- pieces[pieces$crew == cw, ]
    hit <- which(t >= p$start & t <= p$end)
    if (length(hit)) p$piece[hit[1]] else NA_integer_
  })
}

# Excluding a metric makes every value NA. Base R answers that with NaN, -Inf
# or a warning; these answer with NA, which is what the tables should print.
#' @keywords internal
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
#' @keywords internal
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
#' @keywords internal
safe_median <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
#' @keywords internal
safe_quantile <- function(x, p) {
  if (all(is.na(x))) NA_real_ else unname(stats::quantile(x, p, na.rm = TRUE))
}


#' Rank paddlers by how far their stroke rate sits below the rest of the crew
#'
#' An independent line on who steered, usable when the GPS seat order is
#' ambiguous.
#'
#' A steerer stays in time with the crew and never paddles at a higher rate:
#' Steve West, *Outrigger Canoeing - The Art and Skill of Steering* (Kanu
#' Culture, 7th ed. 2014) is explicit about it, since a stroke out of time
#' breaks the crew's rhythm. But a steerer also pokes, and every poke is time
#' not spent stroking, so their *counted* cadence falls below the crew's while
#' still being in time with it. The size of the shortfall tracks the paddle-to
#' -poke split, which the same source puts at roughly 75/25 on flat water and
#' 50/50 in a moderate sea.
#'
#' Each paddler is compared against the median of **the others**, so nobody is
#' measured against a line they helped define, and only long blocks count.
#'
#' Cadence faults defeat this: a device that under-counts strokes looks exactly
#' like heavy poking. Pass the athletes flagged by [treinus_data_quality()] for
#' `cadence` to `ignore` and treat the result as corroboration, never proof.
#'
#' @param records Prepared records.
#' @param pieces Output of [treinus_pieces()].
#' @param settings Settings list.
#' @param ignore Athlete ids whose cadence is not trustworthy.
#' @param min_block_min Only blocks at least this long are used. Default 20.
#' @param min_others Minimum crewmates needed to form a comparison. Default 2.
#'
#' @return One row per athlete: `deficit_spm` against the others, `n` bins, and
#'   `rank` within the crew. The likeliest steerer is rank 1; ties share it, and
#'   a shared first place means the cadence cannot choose between them.
#' @export
treinus_steerer_by_cadence <- function(records, pieces, settings,
                                       ignore = integer(),
                                       min_block_min = 20,
                                       min_others = 2) {
  a <- treinus_validate_settings(settings)$analysis
  tz <- attr(records$ts, "tzone") %||% "UTC"

  long <- pieces |>
    dplyr::filter(.data$dur_min >= min_block_min) |>
    dplyr::select("crew", bloco = "piece")

  if (!nrow(long)) return(empty_steerer_cadence())

  binned <- records |>
    dplyr::filter(!.data$id_athlete %in% ignore,
                  !is.na(.data$cadence), .data$cadence > 20) |>
    dplyr::mutate(tgrid = bin_time(.data$ts, a$cadence_bin_s, tz)) |>
    dplyr::summarise(spm = stats::median(.data$cadence),
                     .by = c("crew", "id_athlete", "fullname_athlete",
                             "is_steerer", "tgrid")) |>
    dplyr::mutate(bloco = which_piece(.data$crew, .data$tgrid, pieces)) |>
    dplyr::filter(!is.na(.data$bloco)) |>
    dplyr::inner_join(long, by = c("crew", "bloco"))

  if (!nrow(binned)) return(empty_steerer_cadence())

  binned |>
    dplyr::filter(dplyr::n() > min_others, .by = c("crew", "tgrid")) |>
    dplyr::mutate(
      # Leave-one-out median: the rest of the crew's stroke.
      vs_others = .data$spm - vapply(seq_along(.data$spm),
                                     \(i) stats::median(.data$spm[-i]),
                                     numeric(1)),
      .by = c("crew", "tgrid")
    ) |>
    dplyr::summarise(deficit_spm = stats::median(.data$vs_others),
                     n = dplyr::n(),
                     .by = c("crew", "id_athlete", "fullname_athlete",
                             "is_steerer")) |>
    dplyr::arrange(.data$crew, .data$deficit_spm) |>
    # min_rank, not row_number: two paddlers can sit at exactly the same
    # deficit, and row_number would silently award first place to whichever
    # happened to be read first.
    dplyr::mutate(rank = dplyr::min_rank(.data$deficit_spm), .by = "crew")
}

empty_steerer_cadence <- function() {
  tibble::tibble(crew = character(), id_athlete = integer(),
                 fullname_athlete = character(), is_steerer = logical(),
                 deficit_spm = numeric(), n = integer(), rank = integer())
}
