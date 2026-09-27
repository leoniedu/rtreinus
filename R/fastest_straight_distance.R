# ============================================================
# Fastest straight-line distance analysis for GPS tracks
# ============================================================
# Finds, for each athlete, the fastest predicted time to cover
# a given straight-line distance (default 500 m), using:
#
# 1) First point >= distance from each start
# 2) Average speed between start and that point
# 3) Predicted time for exactly `distance`
#
# Tracks are automatically split when consecutive timestamps
# differ by more than `max_gap` seconds (default = 20).
#
# No interpolation of geometry or time.
# ============================================================

# data.table NSE columns
utils::globalVariables(c(
  ".", "athlete", "avg_speed_kmh", "avg_speed_mps", "predicted_time_sec",
  "segment_id", "time_num", "x", "y"
))

# ------------------------------------------------------------
# INTERNAL: fastest predicted distance for one continuous track
# ------------------------------------------------------------

.fastest_distance_vectorized <- function(
  x,
  y,
  time,
  distance_m,
  min_time = NULL,
  max_speed = NULL
) {
  n <- length(x)
  if (n < 2) {
    return(NULL)
  }

  # Optimization 1: Pre-calculate constants and convert time once
  time_numeric <- as.numeric(time)
  dist_sq_threshold <- distance_m^2
  best_t_pred <- Inf
  best_i <- NA_integer_
  best_j <- NA_integer_

  for (i in seq_len(n - 1)) {
    # Optimization 2: Create a vector of all indices ahead of i
    j_indices <- (i + 1):n

    # Optimization 3: Vectorized subtraction and squaring
    # This happens in C-speed, calculating all dx and dy at once
    dx <- x[j_indices] - x[i]
    dy <- y[j_indices] - y[i]
    d_sq <- dx^2 + dy^2

    # Optimization 4: Find the first index that exceeds our distance
    # which() is highly optimized. [1] ensures we only take the first match.
    match_idx <- which(d_sq >= dist_sq_threshold)[1]

    if (!is.na(match_idx)) {
      # Map back to the actual index in the original vector
      real_j <- j_indices[match_idx]

      d <- sqrt(d_sq[match_idx])
      dt_ij <- time_numeric[real_j] - time_numeric[i]

      # Optional plausibility filters
      if (!is.null(min_time) && dt_ij < min_time) {
        next
      }
      if (!is.null(max_speed) && (d / dt_ij) > max_speed) {
        next
      }

      # Optimization 5: Simplified algebra
      # t_pred = distance / (d / dt_ij) -> (distance * dt_ij) / d
      t_pred <- (distance_m * dt_ij) / d

      if (t_pred < best_t_pred) {
        best_t_pred <- t_pred
        best_i <- i
        best_j <- real_j
      }
    }
  }

  if (is.infinite(best_t_pred)) {
    return(NULL)
  }

  list(
    t_pred_sec = best_t_pred,
    start_index = best_i,
    end_index = best_j
  )
}

# ------------------------------------------------------------
# PUBLIC: fastest straight-line distance per athlete
# ------------------------------------------------------------
fastest_straight_distance <- function(
  sf_points,
  athlete_col,
  time_col,
  distance_m = 500,
  max_gap = 20, # seconds (DEFAULT)
  min_time = NULL,
  max_speed_kmh = Inf,
  f_distance = .fastest_distance_vectorized
) {
  stopifnot(inherits(sf_points, "sf"))
  stopifnot(inherits(sf_points[[time_col]], "POSIXct"))
  if (sf::st_crs(sf_points)$units_gdal != "metre") {
    stop("CRS is geographic (degrees). Project to a planar CRS first.")
  }

  # Extract coordinates (must already be projected in meters)
  coords <- sf::st_coordinates(sf_points)

  ## max speed is in meters per second
  max_speed <- max_speed_kmh * 1000 / 60 / 60

  dt <- data.table::as.data.table(sf_points)
  data.table::setDT(dt)
  dt[, `:=`(
    x = coords[, 1],
    y = coords[, 2],
    time_num = as.numeric(get(time_col))
  )]
  data.table::setorderv(dt, cols = c(athlete_col, "time_num"))

  # ----------------------------------------------------------
  # Split tracks by time gaps
  # ----------------------------------------------------------
  dt[, segment_id := 1L, by = athlete_col]

  if (!is.null(max_gap)) {
    dt[,
      segment_id := cumsum(
        c(TRUE, diff(time_num) > max_gap)
      ),
      by = athlete_col
    ]
  }

  # ----------------------------------------------------------
  # Compute fastest distance per (athlete, segment)
  # ----------------------------------------------------------
  seg_res <- dt[,
    {
      out <- f_distance(
        x = x,
        y = y,
        time = get(time_col),
        distance_m = distance_m,
        min_time = min_time,
        max_speed = max_speed
      )

      if (is.null(out)) {
        NULL
      } else {
        i <- out$start_index
        j <- out$end_index

        d_end <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
        dt_ij <- as.numeric(get(time_col)[j] - get(time_col)[i])

        # cumulative distance along the track
        cum_dist <- sum(
          sqrt(
            diff(x[i:j])^2 +
              diff(y[i:j])^2
          )
        )

        sinuosity <- cum_dist / d_end

        .(
          distance_m = distance_m,
          predicted_time_sec = out$t_pred_sec,
          avg_speed_mps = d_end / dt_ij,
          start_time = get(time_col)[i],
          end_time = get(time_col)[j],
          straight_distance_m = d_end,
          cumulative_distance_m = cum_dist,
          sinuosity = cum_dist / d_end,
          start_x = x[i],
          start_y = y[i],
          end_x = x[j],
          end_y = y[j]
        )
      }
    },
    by = .(athlete = get(athlete_col), segment_id)
  ]

  # Keep only valid segment results
  if (nrow(seg_res) == 0) {
    return("None found")
  }
  seg_res <- seg_res[!is.na(predicted_time_sec)]
  # ----------------------------------------------------------
  # Keep best segment per athlete
  # ----------------------------------------------------------
  final <- seg_res[,
    .SD[which.min(predicted_time_sec)],
    by = athlete
  ]

  data.table::setnames(final, "athlete", athlete_col)
  final[, avg_speed_kmh := avg_speed_mps * 60 * 60 / 1000]
  final[,]
}

# ------------------------------------------------------------
# OPTIONAL: build sf LINESTRING geometry for results
# ------------------------------------------------------------
fastest_straight_geometry <- function(result_dt, crs) {
  lines <- mapply(
    function(x1, y1, x2, y2) {
      sf::st_linestring(
        matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE)
      )
    },
    result_dt$start_x,
    result_dt$start_y,
    result_dt$end_x,
    result_dt$end_y,
    SIMPLIFY = FALSE
  )

  sf::st_as_sf(
    result_dt,
    geometry = sf::st_sfc(lines, crs = crs)
  )
}
