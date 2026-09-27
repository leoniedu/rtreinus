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


#' Detect which athletes shared a boat
#'
#' Athletes in the same hull stay within a few metres of each other for the
#' whole session. Crews are the connected components of the graph joining pairs
#' that did.
#'
#' The radius has to cover a bow-to-stern pair in a roughly 13 m hull plus a few
#' metres of per-device GPS error, which is why it is far larger than the
#' typical separation. On the reference session the components are identical for
#' any radius from 15 to 30 m.
#'
#' @param records Records with a clock-corrected `ts` (see [treinus_fix_clock()]).
#' @param near_m Radius in metres. Default 25.
#' @param near_pct Share of shared bins a pair must spend within `near_m`.
#'   Default 90.
#' @param min_bins Minimum shared bins for a pair to be considered at all.
#'   Default 60.
#' @param bin_s Grid resolution in seconds. Default 10.
#' @param crs Projected CRS for distance computation. `NULL` derives a UTM zone
#'   from the median longitude.
#'
#' @return A list with `crews` (tibble of `id_athlete`, `crew`), `pairs` (every
#'   pair with `n`, `median_m`, `pct_near`) and `unassigned` (athlete ids with
#'   no qualifying pair).
#' @export
treinus_detect_crews <- function(records,
                                 near_m = 25,
                                 near_pct = 90,
                                 min_bins = 60,
                                 bin_s = 10,
                                 crs = NULL) {
  grid <- treinus_position_grid(records, bin_s = bin_s, crs = crs)

  pairs <- grid |>
    dplyr::inner_join(grid, by = "tgrid", relationship = "many-to-many",
                      suffix = c("", "_j")) |>
    dplyr::filter(.data$id_athlete < .data$id_athlete_j) |>
    dplyr::mutate(m = sqrt((.data$x - .data$x_j)^2 + (.data$y - .data$y_j)^2)) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_m = stats::median(.data$m),
      pct_near = 100 * mean(.data$m < near_m),
      .by = c("id_athlete", "id_athlete_j")
    ) |>
    dplyr::arrange(.data$median_m)

  edges <- pairs |>
    dplyr::filter(.data$n >= min_bins, .data$pct_near > near_pct)

  members <- connected_components(edges$id_athlete, edges$id_athlete_j)

  all_ids <- sort(unique(records$id_athlete))
  list(
    crews = members,
    pairs = pairs,
    unassigned = setdiff(all_ids, members$id_athlete)
  )
}


#' Identify the steerer from position along the hull
#'
#' Projects each crew member onto the boat's heading. The rearmost is proposed
#' as the steerer, which is where the steerer sits.
#'
#' **This does not recover a seating order, and must not be read as one.**
#' Per-device GPS bias is of the same order as the spacing between seats. Across
#' the validated sessions the projected offsets span 16 to 20 m for crews in a
#' hull of roughly 12 to 13 m, and gaps between adjacent paddlers range from
#' 0.09 m to 10.4 m where every one of them should be near 2 m. The middle of
#' the boat is noise.
#'
#' What usually survives that noise is the stern. In five of the six crews
#' checked the steerer came out 4 to 9.4 m behind the next paddler, and a gap
#' that size is robust to metre-scale bias. In the sixth the gap was 0.09 m and
#' the identification was a coin toss - correct, as it happens, but `confident`
#' is `FALSE` there and should be believed. So the function reports who is at
#' the back, how far clear they are, and how much to trust it; it says nothing
#' about who sits where in front of them.
#'
#' Two details make this robust. Positions are compared **pairwise** rather than
#' against the crew centroid, because the centroid moves whenever a member's
#' watch drops out. And the ordering must hold in **both directions of travel**:
#' a wrist GPS carries a quasi-fixed positional bias of a few metres, which
#' projects onto the heading with opposite signs outbound and inbound, so a
#' pooled median can hide a disagreement that is really there.
#'
#' Note this identifies the rearmost *recording device*. That is the steerer
#' only when the steerer is wearing a watch, so `n_recording` is reported and
#' should be weighed before trusting the result.
#'
#' @param records Records with a clock-corrected `ts`.
#' @param crews Tibble of `id_athlete` and `crew`, from [treinus_detect_crews()].
#' @param bin_s Grid resolution in seconds. Default 10.
#' @param min_move_m Minimum centroid displacement per bin for the heading to
#'   be meaningful. Default 15 (about 0.75 m/s).
#' @param min_margin_m How far clear of the next paddler the stern athlete must
#'   sit before the identification counts as confident. Default 1.5.
#' @param n_segments Number of equal time segments the session is split into to
#'   check that the ordering is stable over time. Default 4.
#' @param crs Projected CRS. `NULL` derives a UTM zone from longitude.
#'
#' @return A list with `offsets` (per athlete: `along_m` along the heading, and
#'   `is_stern`), `pairs` (pairwise separations with per-sector medians and
#'   `consistent`),
#'   and `steerer` (per crew: the proposal, its `margin_m` over the next seat,
#'   whether the ordering is sector-`consistent`, whether it is `stable` across
#'   segments of the session, `n_recording`, and `confident`, which requires
#'   all three).
#' @export
treinus_detect_steerer <- function(records, crews,
                               bin_s = 10,
                               min_move_m = 15,
                               min_margin_m = 1.5,
                               n_segments = 4,
                               crs = NULL) {
  grid <- treinus_position_grid(records, bin_s = bin_s, crs = crs) |>
    dplyr::inner_join(crews, by = "id_athlete")

  heading <- grid |>
    dplyr::summarise(cx = mean(.data$x), cy = mean(.data$y),
                     .by = c("crew", "tgrid")) |>
    dplyr::arrange(.data$crew, .data$tgrid) |>
    dplyr::mutate(
      hx = dplyr::lead(.data$cx) - dplyr::lag(.data$cx),
      hy = dplyr::lead(.data$cy) - dplyr::lag(.data$cy),
      h = sqrt(.data$hx^2 + .data$hy^2),
      .by = "crew"
    ) |>
    dplyr::filter(!is.na(.data$h), .data$h > min_move_m)

  along <- grid |>
    dplyr::inner_join(heading, by = c("crew", "tgrid")) |>
    dplyr::mutate(
      along = ((.data$x - .data$cx) * .data$hx +
                 (.data$y - .data$cy) * .data$hy) / .data$h,
      # Heading sector: which way the boat was pointing in this bin.
      sector = dplyr::if_else(atan2(.data$hy, .data$hx) > 0, "N", "S")
    ) |>
    dplyr::mutate(
      segment = as.integer(cut(as.numeric(.data$tgrid), n_segments,
                               labels = FALSE)),
      .by = "crew"
    )

  pairs <- along |>
    dplyr::select("crew", "tgrid", "id_athlete", "along", "sector") |>
    dplyr::inner_join(
      dplyr::select(along, "crew", "tgrid",
                    id_athlete_j = "id_athlete", along_j = "along"),
      by = c("crew", "tgrid"), relationship = "many-to-many"
    ) |>
    dplyr::filter(.data$id_athlete < .data$id_athlete_j) |>
    dplyr::summarise(
      n = dplyr::n(),
      d_all = stats::median(.data$along - .data$along_j),
      d_n = stats::median((.data$along - .data$along_j)[.data$sector == "N"]),
      d_s = stats::median((.data$along - .data$along_j)[.data$sector == "S"]),
      .by = c("crew", "id_athlete", "id_athlete_j")
    ) |>
    dplyr::mutate(consistent = !is.na(.data$d_n) & !is.na(.data$d_s) &
                    sign(.data$d_n) == sign(.data$d_s))

  # Is the stern athlete rearmost in every segment of the session? Two paddlers
  # sitting within a metre of each other can swap places between blocks, and a
  # single window then reports a clean, consistent, entirely wrong answer. Only
  # looking at the session in pieces exposes that.
  by_segment <- along |>
    dplyr::summarise(along_m = stats::median(.data$along),
                     .by = c("crew", "segment", "id_athlete")) |>
    dplyr::slice_min(.data$along_m, n = 1, by = c("crew", "segment")) |>
    dplyr::summarise(rearmost = list(unique(.data$id_athlete)), .by = "crew")

  # Rank seats by mean pairwise offset against everyone else in the crew.
  offsets <- dplyr::bind_rows(
    dplyr::select(pairs, "crew", "id_athlete", d = "d_all"),
    dplyr::transmute(pairs, crew = .data$crew,
                     id_athlete = .data$id_athlete_j, d = -.data$d_all)
  )

  # Only the extreme is reported. Ranking the rest would imply a seating order
  # the measurement cannot support.
  seats <- offsets |>
    dplyr::summarise(along_m = mean(.data$d), .by = c("crew", "id_athlete")) |>
    dplyr::arrange(.data$crew, .data$along_m) |>
    dplyr::mutate(is_stern = .data$along_m == min(.data$along_m), .by = "crew")

  # How far clear of the next paddler the stern athlete sits. This is the one
  # distance the projection measures well, because it is large.
  margins <- seats |>
    dplyr::arrange(.data$crew, .data$along_m) |>
    dplyr::summarise(
      margin_m = if (dplyr::n() > 1) .data$along_m[2] - .data$along_m[1] else NA_real_,
      .by = "crew"
    )

  steerer <- seats |>
    dplyr::filter(.data$is_stern) |>
    dplyr::select("crew", "id_athlete", "along_m") |>
    dplyr::left_join(
      crews |> dplyr::summarise(n_recording = dplyr::n(), .by = "crew"),
      by = "crew"
    ) |>
    dplyr::left_join(margins, by = "crew")

  # Does the stern athlete sit behind every crewmate in both directions?
  stern_consistent <- pairs |>
    dplyr::inner_join(dplyr::select(steerer, "crew", stern = "id_athlete"),
                      by = "crew") |>
    dplyr::filter(.data$id_athlete == .data$stern |
                    .data$id_athlete_j == .data$stern) |>
    dplyr::summarise(consistent = all(.data$consistent), .by = "crew")

  # Confidence needs both. Sector consistency alone can look settled on a short
  # window while the true ordering flips elsewhere in the session: two paddlers
  # half a metre apart swap places between blocks, and every pair in a single
  # window then agrees on the wrong order. The margin is what catches that.
  steerer <- steerer |>
    dplyr::left_join(stern_consistent, by = "crew") |>
    dplyr::left_join(by_segment, by = "crew") |>
    dplyr::mutate(
      stable = purrr::map2_lgl(.data$rearmost, .data$id_athlete,
                               \(r, id) length(r) == 1 && identical(r[[1]], id)),
      confident = .data$consistent & .data$stable &
        !is.na(.data$margin_m) & .data$margin_m >= min_margin_m
    ) |>
    dplyr::select(-"rearmost")

  list(offsets = seats, pairs = pairs, steerer = steerer)
}


#' Positions on a shared time grid, projected
#'
#' @param records Records with a clock-corrected `ts`.
#' @param bin_s Grid resolution in seconds.
#' @param crs Projected CRS, or `NULL` to derive a UTM zone.
#' @return Tibble of `id_athlete`, `tgrid`, `x`, `y` in projected metres.
#' @keywords internal
#' @export
treinus_position_grid <- function(records, bin_s = 10, crs = NULL) {
  pts <- records |>
    dplyr::filter(!is.na(.data$position_long), !is.na(.data$position_lat)) |>
    dplyr::mutate(
      lat = .data$position_lat * 180 / 2^31,
      lon = .data$position_long * 180 / 2^31
    )

  if (nrow(pts) == 0) {
    cli::cli_abort("No positions in {.arg records}; cannot build a grid.")
  }
  if (is.null(crs)) crs <- utm_crs(stats::median(pts$lon), stats::median(pts$lat))

  tz <- attr(records$ts, "tzone") %||% "UTC"

  pts |>
    dplyr::mutate(
      tgrid = as.POSIXct(round(as.numeric(.data$ts) / bin_s) * bin_s,
                         origin = "1970-01-01", tz = tz)
    ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs) |>
    (\(d) dplyr::mutate(d,
                        x = sf::st_coordinates(d)[, 1],
                        y = sf::st_coordinates(d)[, 2]))() |>
    sf::st_drop_geometry() |>
    dplyr::summarise(x = mean(.data$x), y = mean(.data$y),
                     .by = c("id_athlete", "tgrid")) |>
    tibble::as_tibble()
}


#' EPSG code for the UTM zone containing a point
#' @keywords internal
utm_crs <- function(lon, lat) {
  zone <- floor((lon + 180) / 6) + 1
  # SIRGAS 2000 / UTM south zones are 31954 + (zone - 1); north uses WGS84.
  if (lat < 0) 32700 + zone else 32600 + zone
}


#' Parse Treinus record timestamps
#' @keywords internal
as_treinus_time <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  as.POSIXct(x, tz = "UTC")
}


#' Connected components of an undirected graph given as edge endpoints
#' @keywords internal
connected_components <- function(from, to) {
  ids <- sort(unique(c(from, to)))
  if (!length(ids)) {
    return(tibble::tibble(id_athlete = integer(), crew = character()))
  }
  comp <- stats::setNames(seq_along(ids), ids)
  repeat {
    before <- comp
    for (i in seq_along(from)) {
      a <- as.character(from[i])
      b <- as.character(to[i])
      k <- min(comp[[a]], comp[[b]])
      comp[[a]] <- k
      comp[[b]] <- k
    }
    if (identical(before, comp)) break
  }
  tibble::tibble(
    id_athlete = as.integer(names(comp)),
    crew = paste("Canoa", as.integer(factor(comp)))
  ) |>
    dplyr::arrange(.data$crew, .data$id_athlete)
}
