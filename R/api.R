#' Get last exercises for an athlete
#'
#' Retrieves the most recent exercises/workouts done by an athlete.
#' Results are memoised to disk and persist across R sessions.
#' Use [treinus_clear_memoise()] to force a refresh.
#'
#' @param athlete_id Integer ID of the athlete. If NULL, uses `TREINUS_ATHLETE_ID`
#'   environment variable. Can be a vector of athlete IDs.
#' @param team_id Integer ID of the team. If NULL, uses `TREINUS_TEAM_ID`
#'   environment variable or team from session.
#' @param session A treinus_session object from [treinus_auth()]. Optional when
#'   using cached data with `use_db = TRUE`.
#' @param use_db Logical. Store results in local SQLite database? Default TRUE.
#' @param overwrite_db Logical. If TRUE, overwrite existing records in database.
#'   If FALSE, skip duplicates. Default FALSE.
#' @param .progress Show progress bar? Default TRUE.
#' @param .delay Numeric. Seconds to wait between API requests. Default 0.5.
#'   Only applies to actual API calls, not cache hits.
#'
#' @return A tibble with exercise data including columns like `id_exercise`,
#'   `genre_name`, `start`, `distance`, `total_elapsed_time`, etc.
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' # Single athlete
#' exercises <- treinus_get_exercises(athlete_id = 50, session = session)
#'
#' # Multiple athletes
#' exercises <- treinus_get_exercises(athlete_id = c(50, 51, 52), session = session)
#'
#' # Using cached data without a session
#' exercises <- treinus_get_exercises(athlete_id = 50, use_db = TRUE)
#' }
#'
#' @seealso [treinus_clear_memoise()], [treinus_get_exercises_db()]
#' @export
treinus_get_exercises <- function(
  athlete_id = NULL,
  team_id = NULL,
  session = NULL,
  use_db = TRUE,
  overwrite_db = FALSE,
  .progress = TRUE,
  .delay = 0.5
) {
  if (!is.null(session) && !inherits(session, "treinus_session")) {
    cli::cli_abort(
      "{.arg session} must be a treinus_session object from {.fn treinus_auth} or NULL."
    )
  }

  # Handle single vs multiple athlete IDs
  athlete_ids <- resolve_athlete_id(athlete_id)
  team_id <- resolve_team_id(team_id, session)

  if (is.null(session)) {
    if (!use_db) {
      cli::cli_abort(
        "Either {.arg session} or {.code use_db = TRUE} is required to fetch exercises."
      )
    }
    cli::cli_alert_info("No session provided, returning cached data from database.")
    return(treinus_get_exercises_db())
  }

  # Always use the vectorized function regardless of single or multiple athletes
  result <- get_exercises_vectorized(
    athlete_ids,
    team_id,
    session = session,
    .progress = .progress,
    .delay = .delay
  )

  # Store in database if requested
  if (use_db && nrow(result) > 0) {
    for (aid in unique(result$id_athlete)) {
      athlete_data <- result[result$id_athlete == aid, ]
      inserted <- store_exercises_in_db(
        athlete_data,
        team_id,
        aid
      )
      cli::cli_alert_success(
        "Stored {inserted} exercise{?s} in database for athlete {aid}"
      )
    }
  }

  return(result)
}


#' Get detailed exercise analysis
#'
#' Retrieves detailed analysis data for one or more exercises, including
#' GPS records, heart rate, and other metrics.
#'
#' @param exercise_id Integer ID(s) of the exercise(s). Can be a vector.
#' @param athlete_id Integer ID of the athlete (single value). If NULL, uses
#'   `TREINUS_ATHLETE_ID` environment variable.
#' @param team_id Integer ID of the team (single value). If NULL, uses the
#'   team_id from session (if set during auth) or `TREINUS_TEAM_ID` environment variable.
#' @param session A treinus_session object from [treinus_auth()]. Optional when
#'   all requested exercises are already cached.
#' @param cache Logical. Cache results locally? Default TRUE.
#'   Cached data is stored in the user cache directory (see [treinus_cache_dir()]).
#' @param .progress Logical. Show progress bar for multiple exercises? Default TRUE.
#' @param .delay Numeric. Seconds to wait between API requests (for multiple exercises).
#'   Default 0.5. Set to 0 for no delay. Only applies to non-cached requests.
#'
#' @return For a single exercise_id, a list with exercise analysis data.
#'   For multiple exercise_ids, a named list of analysis results.
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#'
#' # Single exercise
#' analysis <- treinus_get_exercise_analysis(
#'   exercise_id = 57,
#'   athlete_id = 50,
#'   team_id = 2994,
#'   session = session
#' )
#' records <- analysis$data$Analysis$Records
#'
#' # Multiple exercises
#' analyses <- treinus_get_exercise_analysis(
#'   exercise_id = c(57, 58, 59),
#'   session = session
#' )
#' analyses[["57"]]$data$Analysis$Records
#' }
#'
#' @seealso [treinus_cache_dir()], [treinus_clear_cache()]
#' @export
treinus_get_exercise_analysis <- function(
  exercise_id,
  athlete_id = NULL,
  team_id = NULL,
  session = NULL,
  cache = TRUE,
  .progress = TRUE,
  .delay = 0.5
) {
  if (!is.null(session) && !inherits(session, "treinus_session")) {
    cli::cli_abort(
      "{.arg session} must be a treinus_session object from {.fn treinus_auth} or NULL."
    )
  }

  athlete_id <- resolve_athlete_id(athlete_id)
  team_id <- resolve_team_id(team_id, session)

  if (length(athlete_id) != 1) {
    cli::cli_abort("{.arg athlete_id} must be a single value, not a vector.")
  }
  if (length(team_id) != 1) {
    cli::cli_abort("{.arg team_id} must be a single value, not a vector.")
  }

  # Single exercise - return directly
  if (length(exercise_id) == 1) {
    return(fetch_single_analysis(
      session,
      exercise_id,
      athlete_id,
      team_id,
      cache
    ))
  }

  # Multiple exercises - return named list
  n <- length(exercise_id)
  results <- vector("list", n)
  names(results) <- as.character(exercise_id)

  if (.progress) {
    cli::cli_progress_bar("Fetching exercises", total = n)
  }

  last_was_api_call <- FALSE

  for (i in seq_len(n)) {
    ex_id <- exercise_id[i]

    # Rate limit: delay after previous API call
    if (last_was_api_call && .delay > 0) {
      Sys.sleep(.delay)
    }

    # Check cache first
    if (cache) {
      cache_file <- treinus_cache_path(team_id, athlete_id, ex_id)
      if (file.exists(cache_file)) {
        cached <- safe_cache_read(cache_file, exercise_id = ex_id)
        if (!is.null(cached)) {
          results[[i]] <- cached
          last_was_api_call <- FALSE
          if (.progress) {
            cli::cli_progress_update()
          }
          next
        }
        # Cache was corrupt — fall through to API fetch
      }
    }

    # Fetch from API
    last_was_api_call <- TRUE
    results[[i]] <- tryCatch(
      fetch_single_analysis(session, ex_id, athlete_id, team_id, cache),
      error = function(e) {
        cli::cli_alert_warning("Failed exercise {ex_id}: {conditionMessage(e)}")
        NULL
      }
    )

    if (.progress) cli::cli_progress_update()
  }

  if (.progress) {
    cli::cli_progress_done()
  }

  results
}


#' Fetch a single exercise analysis (internal)
#' @keywords internal
fetch_single_analysis <- function(
  session,
  exercise_id,
  athlete_id,
  team_id,
  cache
) {
  # Check cache first
  if (cache) {
    cache_file <- treinus_cache_path(team_id, athlete_id, exercise_id)
    if (file.exists(cache_file)) {
      cached <- safe_cache_read(cache_file, exercise_id = exercise_id)
      if (!is.null(cached)) return(cached)
      # Cache was corrupt — fall through to API fetch
    }
  }

  if (is.null(session)) {
    cli::cli_abort(
      "{.arg session} is required to fetch exercise {exercise_id} (not in cache)."
    )
  }

  base_url <- attr(session, "base_url")
  url <- paste0(base_url, "/Athlete/ExerciseSheet/Done/ExerciseAnalysis")

  resp <- session |>
    httr2::req_url(url) |>
    httr2::req_url_query(
      idTeam = team_id,
      idAthlete = athlete_id,
      idExerciseDone = exercise_id
    ) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)

  # Check for API error
  if (isFALSE(result$success)) {
    cli::cli_abort(c(
      "x" = "Failed to get exercise analysis.",
      "i" = "API message: {result$message}",
      "i" = "exercise_id={exercise_id}, athlete_id={athlete_id}, team_id={team_id}"
    ))
  }

  # Save to cache only on success
  if (cache) {
    cache_file <- treinus_cache_path(team_id, athlete_id, exercise_id)
    safe_cache_write(result, cache_file)
  }

  result
}


#' Extract records from exercise analysis
#'
#' Extracts time-series records from exercise analysis. Includes unit metadata
#' as attributes and optionally standardizes values to common units.
#'
#' @param analysis Analysis data from [treinus_get_exercise_analysis()]
#' @param standardize Logical. Apply unit standardization? Default FALSE.
#' @param tz Timezone for timestamp conversion (used when `standardize=TRUE`).
#'   Default "America/Sao_Paulo".
#'
#' @return A tibble with time-series records. Unit metadata from the analysis
#'   is attached as attributes (e.g., `attr(result, "DistanceUnit")`).
#'
#'   If `standardize=TRUE`, additional columns are added:
#'   \itemize{
#'     \item `timestamp`: converted to POSIXct datetime
#'     \item `lat`, `lon`: coordinates in degrees (converted from semicircles)
#'     \item `distance_km`: distance in kilometers
#'     \item `speed_kmh`: speed in km/h
#'   }
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' analysis <- treinus_get_exercise_analysis(session, 57, 50, 2994)
#'
#' # Raw records with unit attributes
#' records <- treinus_extract_records(analysis)
#' attr(records, "DistanceUnit")
#'
#' # Standardized records
#' records <- treinus_extract_records(analysis, standardize = TRUE)
#' }
#'
#' @export
treinus_extract_records <- function(
  analysis,
  standardize = FALSE,
  tz = "America/Sao_Paulo"
) {
  records <- analysis$data$Analysis$Records
  if (is.null(records) || length(records) == 0) {
    return(tibble::tibble())
  }

  df <- dplyr::bind_rows(records) |>
    janitor::clean_names()

  # Extract all unit metadata

  analysis_data <- analysis$data$Analysis
  unit_names <- grep("Unit$", names(analysis_data), value = TRUE)
  units <- stats::setNames(
    lapply(unit_names, function(u) analysis_data[[u]]),
    unit_names
  )

  # Attach units as attributes
  for (unit_name in names(units)) {
    attr(df, unit_name) <- units[[unit_name]]
  }

  if (!standardize) {
    return(df)
  }

  # Validate required units for standardization
  distance_unit <- units$DistanceUnit
  speed_unit <- units$SpeedUnit

  if (is.null(distance_unit)) {
    cli::cli_abort(
      "Cannot standardize: {.field DistanceUnit} not found in analysis metadata"
    )
  }
  if (is.null(speed_unit)) {
    cli::cli_abort(
      "Cannot standardize: {.field SpeedUnit} not found in analysis metadata"
    )
  }

  # Conversion factors
  semicircles_to_deg <- 180 / 2^31

  dist_to_km <- switch(
    distance_unit,
    "m" = 1 / 1000,
    "km" = 1,
    cli::cli_abort("Unknown {.field DistanceUnit}: {.val {distance_unit}}")
  )

  speed_to_kmh <- switch(
    speed_unit,
    "m/s" = 3.6,
    "km/h" = 1,
    cli::cli_abort("Unknown {.field SpeedUnit}: {.val {speed_unit}}")
  )

  df |>
    dplyr::mutate(
      timestamp = as.POSIXct(.data$timestamp, tz = tz),
      lat = .data$position_lat * semicircles_to_deg,
      lon = .data$position_long * semicircles_to_deg,
      distance_km = .data$distance * dist_to_km,
      speed_kmh = .data$speed * speed_to_kmh
    )
}


#' Get dashboard data
#'
#' Retrieves dashboard summary data including recent workouts and statistics.
#'
#' @param session A treinus_session object from [treinus_auth()]
#'
#' @return A list with dashboard data
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' dashboard <- treinus_get_dashboard(session)
#' }
#'
#' @export
treinus_get_dashboard <- function(session) {
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort(
      "{.arg session} must be a treinus_session object from {.fn treinus_auth}"
    )
  }

  base_url <- attr(session, "base_url")
  url <- paste0(base_url, "/Global/DashBoard/Index")

  cli::cli_progress_step("Fetching dashboard data...")

  resp <- session |>
    httr2::req_url(url) |>
    httr2::req_perform()

  page <- httr2::resp_body_html(resp)

  # Extract key metrics from dashboard
  dashboard_data <- list(
    title = page |> rvest::html_element("title") |> rvest::html_text2(),
    # Add more extraction logic here based on actual dashboard structure
    status = "success"
  )

  cli::cli_progress_done()

  dashboard_data
}


#' Make a raw API request
#'
#' Low-level function to make authenticated requests to any Treinus endpoint.
#' Useful for exploring the API or accessing endpoints not yet wrapped.
#'
#' @param session A treinus_session object from [treinus_auth()]
#' @param endpoint Character string with the endpoint path (e.g., "/Athlete/Performance/Index")
#' @param method HTTP method ("GET", "POST", etc.)
#' @param body Optional request body for POST requests
#' @param ... Additional arguments passed to [httr2::req_perform()]
#'
#' @return httr2 response object
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#'
#' # Make a custom request
#' resp <- treinus_request(
#'   session,
#'   endpoint = "/Athlete/Performance/Index",
#'   method = "GET"
#' )
#' }
#'
#' @export
treinus_request <- function(
  session,
  endpoint,
  method = "GET",
  body = NULL,
  ...
) {
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort(
      "{.arg session} must be a treinus_session object from {.fn treinus_auth}"
    )
  }

  base_url <- attr(session, "base_url")
  url <- paste0(base_url, endpoint)

  req <- session |>
    httr2::req_url(url) |>
    httr2::req_method(method)

  if (!is.null(body)) {
    req <- req |> httr2::req_body_json(body)
  }

  req |> httr2::req_perform(...)
}


#' Extract data from Treinus HTML tables
#'
#' Helper function to parse data from HTML tables commonly found in Treinus pages.
#'
#' @param html An html_document or html_node from rvest
#' @param selector CSS selector for the table (default: "table")
#'
#' @return A tibble with the table data
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' resp <- treinus_request(session, "/Athlete/Performance/Index")
#' page <- httr2::resp_body_html(resp)
#'
#' # Extract table data
#' data <- treinus_parse_table(page)
#' }
#'
#' @export
treinus_parse_table <- function(html, selector = "table") {
  table <- html |> rvest::html_element(selector)

  if (inherits(table, "xml_missing") || length(table) == 0) {
    cli::cli_warn("No table found with selector: {selector}")
    return(tibble::tibble())
  }

  table |>
    rvest::html_table() |>
    tibble::as_tibble()
}


# ID resolution helpers ---------------------------------------------------

#' Resolve athlete_id from argument or environment
#' @keywords internal
resolve_athlete_id <- function(athlete_id) {
  if (!is.null(athlete_id)) {
    return(as.integer(athlete_id))
  }

  env_id <- Sys.getenv("TREINUS_ATHLETE_ID", unset = "")
  if (env_id != "") {
    return(as.integer(env_id))
  }

  cli::cli_abort(c(
    "x" = "{.arg athlete_id} not provided.",
    "i" = "Set {.envvar TREINUS_ATHLETE_ID} environment variable or pass explicitly."
  ))
}


#' Resolve team_id from argument, session, or environment
#' @keywords internal
resolve_team_id <- function(team_id, session = NULL) {
  if (!is.null(team_id)) {
    return(as.integer(team_id))
  }

  # Try from session
  if (!is.null(session)) {
    session_team <- attr(session, "team_id")
    if (!is.null(session_team)) {
      return(as.integer(session_team))
    }
  }

  # Try from environment
  env_id <- Sys.getenv("TREINUS_TEAM_ID", unset = "")
  if (env_id != "") {
    return(as.integer(env_id))
  }

  cli::cli_abort(c(
    "x" = "{.arg team_id} not provided.",
    "i" = "Set {.envvar TREINUS_TEAM_ID} environment variable, or pass explicitly,",
    "i" = "or authenticate with team selection to store team_id in session."
  ))
}


# Safe cache I/O ----------------------------------------------------------

#' Safe atomic RDS cache write
#'
#' Writes to a temporary file first then renames, so an interrupted write
#' cannot leave a corrupted cache file behind.
#'
#' @param obj Object to write
#' @param cache_file Destination file path
#' @keywords internal
safe_cache_write <- function(obj, cache_file) {
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  temp_file <- tempfile(tmpdir = dirname(cache_file), fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)
  saveRDS(obj, temp_file)
  file.rename(temp_file, cache_file)
}

#' Safe RDS cache read with corruption handling
#'
#' Reads an RDS file, handling corruption gracefully by warning, deleting the
#' corrupt file, and returning NULL to signal a cache miss.
#'
#' @param cache_file Path to the cache file
#' @param exercise_id Optional exercise ID for diagnostic messages
#' @return The cached object, or NULL if the file is corrupt
#' @keywords internal
safe_cache_read <- function(cache_file, exercise_id = NULL) {
  tryCatch(
    readRDS(cache_file),
    error = function(e) {
      if (!is.null(exercise_id)) {
        cli::cli_warn(
          "Cache file corrupted for exercise {exercise_id}, re-fetching from API."
        )
      } else {
        cli::cli_warn(
          "Cache file corrupted at {.path {cache_file}}, re-fetching from API."
        )
      }
      unlink(cache_file)
      NULL
    }
  )
}


# Cache functions ---------------------------------------------------------

#' Get the cache directory path
#'
#' Returns the path to the rtreinus cache directory, following
#' platform-specific conventions via [tools::R_user_dir()].
#'
#' @return Character string with the cache directory path.
#'
#' @examples
#' treinus_cache_dir()
#'
#' @seealso [treinus_clear_cache()], [treinus_cache_info()]
#' @export
treinus_cache_dir <- function() {
  tools::R_user_dir("rtreinus", "cache")
}


#' Get cache file path for an exercise
#'
#' @param team_id Team ID
#' @param athlete_id Athlete ID
#' @param exercise_id Exercise ID
#' @return Character string with cache file path
#' @keywords internal
treinus_cache_path <- function(team_id, athlete_id, exercise_id) {
  file.path(
    treinus_cache_dir(),
    sprintf("%s_%s_%s.rds", team_id, athlete_id, exercise_id)
  )
}


#' Clear the exercise analysis cache
#'
#' Removes cached exercise analysis data. Can clear all cached data or
#' specific exercises.
#'
#' @param team_id Optional team ID to filter
#' @param athlete_id Optional athlete ID to filter
#' @param exercise_id Optional exercise ID to clear specific exercise
#' @param all Logical. If TRUE, clear entire cache. Default FALSE.
#'
#' @return Invisibly returns the number of files deleted.
#'
#' @examples
#' \dontrun{
#' # Clear all cache
#' treinus_clear_cache(all = TRUE)
#'
#' # Clear specific exercise
#' treinus_clear_cache(team_id = 2994, athlete_id = 50, exercise_id = 151)
#'
#' # Clear all exercises for an athlete
#' treinus_clear_cache(team_id = 2994, athlete_id = 50)
#' }
#'
#' @seealso [treinus_cache_dir()], [treinus_cache_info()]
#' @export
treinus_clear_cache <- function(
  team_id = NULL,
  athlete_id = NULL,
  exercise_id = NULL,
  all = FALSE
) {
  cache_dir <- treinus_cache_dir()

  if (!dir.exists(cache_dir)) {
    cli::cli_alert_info("Cache directory does not exist.")
    return(invisible(0L))
  }

  if (all) {
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  } else if (
    !is.null(exercise_id) && !is.null(athlete_id) && !is.null(team_id)
  ) {
    # Specific exercise
    files <- treinus_cache_path(team_id, athlete_id, exercise_id)
    files <- files[file.exists(files)]
  } else {
    # Pattern match
    pattern <- paste0(
      if (!is.null(team_id)) team_id else "\\d+",
      "_",
      if (!is.null(athlete_id)) athlete_id else "\\d+",
      "_",
      if (!is.null(exercise_id)) exercise_id else "\\d+",
      "\\.rds$"
    )
    files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
  }

  if (length(files) == 0) {
    cli::cli_alert_info("No cached files match the criteria.")
    return(invisible(0L))
  }

  unlink(files)
  cli::cli_alert_success("Deleted {length(files)} cached file{?s}.")
  invisible(length(files))
}


#' Get cache information
#'
#' Returns information about cached exercise analysis data.
#'
#' @return A tibble with columns: team_id, athlete_id, exercise_id, size_kb, modified
#'
#' @examples
#' \dontrun{
#' treinus_cache_info()
#' }
#'
#' @seealso [treinus_cache_dir()], [treinus_clear_cache()]
#' @export
treinus_cache_info <- function() {
  cache_dir <- treinus_cache_dir()

  if (!dir.exists(cache_dir)) {
    return(tibble::tibble(
      team_id = integer(),
      athlete_id = integer(),
      exercise_id = integer(),
      size_kb = numeric(),
      modified = as.POSIXct(character())
    ))
  }

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    return(tibble::tibble(
      team_id = integer(),
      athlete_id = integer(),
      exercise_id = integer(),
      size_kb = numeric(),
      modified = as.POSIXct(character())
    ))
  }

  info <- file.info(files)
  basenames <- basename(files)

  # Parse filenames: team_athlete_exercise.rds
  parts <- strsplit(sub("\\.rds$", "", basenames), "_")

  tibble::tibble(
    team_id = as.integer(vapply(parts, `[`, character(1), 1)),
    athlete_id = as.integer(vapply(parts, `[`, character(1), 2)),
    exercise_id = as.integer(vapply(parts, `[`, character(1), 3)),
    size_kb = round(info$size / 1024, 1),
    modified = info$mtime
  )
}


#' Clear memoised exercise download cache
#'
#' Clears the on-disk memoise cache for exercise list downloads.
#' Use this to force re-fetching of exercise lists from the API.
#' The cache is stored under [treinus_cache_dir()]`/memoise/`.
#'
#' @return Invisibly returns NULL.
#'
#' @examples
#' \dontrun{
#' # Fetch exercises (cached on disk)
#' exercises <- treinus_get_exercises(athlete_id = 50, session = session)
#'
#' # Force re-fetch by clearing cache
#' treinus_clear_memoise()
#' exercises <- treinus_get_exercises(athlete_id = 50, session = session)
#' }
#'
#' @seealso [treinus_clear_cache()], [treinus_clean_old_cache()]
#' @export
treinus_clear_memoise <- function() {
  memoise::forget(.memoised_download_exercises)
  cli::cli_alert_success("Cleared memoised exercise list cache.")
  invisible(NULL)
}


#' Clean old cache files
#'
#' Removes cached exercise analysis RDS files older than the specified age.
#'
#' @param max_age_days Maximum age in days. Default 30.
#' @return Invisibly returns the number of files deleted.
#'
#' @examples
#' \dontrun{
#' # Clean cache files older than 30 days
#' treinus_clean_old_cache()
#'
#' # Clean files older than 7 days
#' treinus_clean_old_cache(max_age_days = 7)
#' }
#'
#' @seealso [treinus_cache_info()], [treinus_clear_cache()]
#' @export
treinus_clean_old_cache <- function(max_age_days = 30) {
  cache_dir <- treinus_cache_dir()

  if (!dir.exists(cache_dir)) {
    cli::cli_alert_info("Cache directory does not exist.")
    return(invisible(0L))
  }

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    cli::cli_alert_info("No cached files found.")
    return(invisible(0L))
  }

  cutoff <- Sys.time() - (max_age_days * 86400)
  info <- file.info(files)
  old_files <- files[info$mtime < cutoff]

  if (length(old_files) == 0) {
    cli::cli_alert_info(
      "No cached files older than {max_age_days} day{?s}."
    )
    return(invisible(0L))
  }

  unlink(old_files)
  cli::cli_alert_success(
    "Deleted {length(old_files)} file{?s} older than {max_age_days} day{?s}."
  )
  invisible(length(old_files))
}


# Database functions --------------------------------------------------------

#' Get the database file path
#'
#' Returns the path to the rtreinus database file.
#'
#' @return Character string with the database file path.
#'
#' @examples
#' treinus_db_path()
#'
#' @export
treinus_db_path <- function() {
  file.path(tools::R_user_dir("rtreinus", "data"), "treinus.db")
}


#' Initialize the database
#'
#' Creates the database file and initializes the exercises table if it doesn't exist.
#'
#' @return SQLite connection object
#' @keywords internal
init_db <- function() {
  db_path <- treinus_db_path()
  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  return(con)
}


#' Store exercises in database
#'
#' Stores exercise data in the local SQLite database. Handles schema evolution
#' safely: new columns in the API response are added via ALTER TABLE, and
#' columns missing from the new data are preserved with NA.
#'
#' @param exercises A tibble with exercise data
#' @param team_id Team ID associated with the exercises
#' @param athlete_id Athlete ID associated with the exercises
#'
#' @return Number of rows inserted/updated
#' @keywords internal
store_exercises_in_db <- function(
  exercises,
  team_id,
  athlete_id
) {
  if (nrow(exercises) == 0) return(0L)

  key_cols <- c("id_team", "id_athlete", "id_exercise")
  con <- init_db()
  on.exit(RSQLite::dbDisconnect(con))

  # Write new data to temporary table
  RSQLite::dbWriteTable(
    con, "tmp_exercises",
    value = exercises,
    row.names = FALSE,
    overwrite = TRUE
  )

  if (!RSQLite::dbExistsTable(con, "exercises")) {
    # First time: create from temp table
    RSQLite::dbExecute(
      con,
      "CREATE TABLE exercises AS SELECT * FROM tmp_exercises"
    )
  } else {
    # Schema evolution: handle column differences
    old_cols <- RSQLite::dbListFields(con, "exercises")
    new_cols <- names(exercises)

    # Add new columns to existing table via ALTER TABLE
    missing_in_old <- setdiff(new_cols, old_cols)
    if (length(missing_in_old) > 0) {
      cli::cli_alert_info(
        "Adding {length(missing_in_old)} new column{?s}: {.field {missing_in_old}}"
      )
      for (col in missing_in_old) {
        col_type <- infer_sql_type(exercises[[col]])
        sql <- sprintf('ALTER TABLE exercises ADD COLUMN "%s" %s', col, col_type)
        RSQLite::dbExecute(con, sql)
      }
    }

    # Pad new data with NA for columns in DB but not in new data
    missing_in_new <- setdiff(old_cols, new_cols)
    if (length(missing_in_new) > 0) {
      cli::cli_alert_info(
        "Preserving {length(missing_in_new)} existing column{?s} not in new data: {.field {missing_in_new}}"
      )
      for (col in missing_in_new) {
        exercises[[col]] <- NA
      }
      # Rewrite tmp table with all columns
      RSQLite::dbWriteTable(
        con, "tmp_exercises",
        value = exercises,
        row.names = FALSE,
        overwrite = TRUE
      )
    }

    # Upsert with all columns aligned (exclude key cols from update set)
    all_cols <- union(old_cols, new_cols)
    update_cols <- setdiff(all_cols, key_cols)

    sql <- dbplyr::sql_query_upsert(
      con = con,
      table = "exercises",
      from = "tmp_exercises",
      by = key_cols,
      update_cols = update_cols
    )
    RSQLite::dbExecute(con, sql)
  }

  # Ensure unique index exists
  RSQLite::dbExecute(
    con,
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_unique_exercise ON exercises (id_team, id_athlete, id_exercise)"
  )

  nrow(exercises)
}


#' Infer SQLite column type from an R vector
#' @param x An R vector
#' @return A character string with the SQLite type name
#' @keywords internal
infer_sql_type <- function(x) {
  if (is.integer(x)) return("INTEGER")
  if (is.numeric(x)) return("REAL")
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return("TEXT")
  if (is.logical(x)) return("INTEGER")
  "TEXT"
}


#' Clear exercises from database
#'
#' Removes exercise data from the local SQLite database.
#'
#' @param athlete_ids Vector of athlete IDs to clear (optional, clears all if NULL)
#' @param team_id Team ID to filter by (optional)
#' @param older_than Date to clear exercises older than (optional)
#'
#' @return Invisibly returns the number of rows deleted
#' @keywords internal
clear_exercises_db <- function(
  athlete_ids = NULL,
  team_id = NULL,
  older_than = NULL
) {
  con <- init_db()
  on.exit(RSQLite::dbDisconnect(con))

  query <- "DELETE FROM exercises WHERE 1=1"
  params <- list()

  if (!is.null(athlete_ids)) {
    placeholders <- paste(rep("?", length(athlete_ids)), collapse = ",")
    query <- paste(query, "AND id_athlete IN (", placeholders, ")")
    params <- c(params, as.list(athlete_ids))
  }

  if (!is.null(team_id)) {
    query <- paste(query, "AND id_team = ?")
    params <- c(params, team_id)
  }

  if (!is.null(older_than)) {
    query <- paste(query, "AND start < ?")
    params <- c(params, older_than)
  }

  result <- RSQLite::dbExecute(con, query, unlist(params))
  cli::cli_alert_success("Deleted {result} exercise record{?s} from database.")
  return(invisible(result))
}


#' Get database information
#'
#' Returns information about the exercise database.
#'
#' @return A tibble with database statistics
#'
#' @examples
#' \dontrun{
#' db_info <- treinus_db_info()
#' }
#'
#' @export
treinus_db_info <- function() {
  con <- init_db()
  on.exit(RSQLite::dbDisconnect(con))

  # Get table info
  table_info <- RSQLite::dbGetQuery(
    con,
    "SELECT COUNT(*) as total_records FROM exercises"
  )

  # Get athlete counts
  athlete_counts <- RSQLite::dbGetQuery(
    con,
    "
    SELECT
      id_athlete,
      COUNT(*) as exercise_count,
      MIN(start) as first_exercise,
      MAX(start) as last_exercise
    FROM exercises
    GROUP BY id_athlete
    ORDER BY id_athlete
  "
  )

  # Get team counts
  team_counts <- RSQLite::dbGetQuery(
    con,
    "
    SELECT
      id_team,
      COUNT(*) as exercise_count
    FROM exercises
    GROUP BY id_team
    ORDER BY id_team
  "
  )

  list(
    total_records = table_info$total_records[1],
    athlete_summary = athlete_counts,
    team_summary = team_counts
  )
}


#' Get exercises database table (lazy)
#'
#' @return exercises
#'
#' @export
treinus_get_exercises_db <- function() {
  con <- init_db()
  on.exit(RSQLite::dbDisconnect(con))
  # Return a lazy dplyr table using dbplyr
  # The connection will remain open for the lifetime of the returned object
  # and will be closed when the object is garbage collected
  res <- dplyr::tbl(con, "exercises") |> dplyr::collect()
  res
}


# Memoized helper function --------------------------------------------------

#' Helper function to download exercises for a single athlete
#'
#' Downloads exercises for a single athlete from the API.
#'
#' @param session A treinus_session object
#' @param athlete_id Integer ID of the athlete
#' @param team_id Integer ID of the team
#'
#' @return A tibble with exercise data for the athlete
#' @keywords internal
download_single_exercise_internal <- function(session, athlete_id, team_id) {
  base_url <- attr(session, "base_url")
  url <- paste0(base_url, "/Athlete/Exercise/GetLastExerciseDone")

  resp <- session |>
    httr2::req_url(url) |>
    httr2::req_url_query(idAthlete = athlete_id, idTeam = team_id) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform()

  json <- httr2::resp_body_json(resp)

  if (is.null(json$data) || length(json$data) == 0) {
    return(tibble::tibble())
  }

  result <- dplyr::bind_rows(json$data) |>
    janitor::clean_names()

  # Add athlete and team IDs to the result
  result <- result |>
    dplyr::mutate(
      id_athlete = as.integer(athlete_id),
      id_team = as.integer(team_id)
    )

  return(result)
}


#' Vectorized function to get exercises for multiple athletes
#'
#' Always uses the memoised version. Rate limiting only applies to actual API
#' calls (memoise cache hits skip the delay).
#'
#' @param athlete_ids Vector of athlete IDs
#' @param team_id Integer ID of the team
#' @param session A treinus_session object
#' @param .progress Show progress bar? Default TRUE.
#' @param .delay Numeric. Seconds to wait between API requests. Default 0.5.
#'
#' @return A tibble with exercise data for all athletes
#' @keywords internal
get_exercises_vectorized <- function(
  athlete_ids,
  team_id,
  session = NULL,
  .progress = TRUE,
  .delay = 0.5
) {
  # Store session in package env so the memoised function can access it
  # without including it in the cache key
  .rtreinus_env$session <- session

  # Smart rate limiting: only delay on actual API calls, not cache hits
  fetch_one <- function(athlete_id) {
    is_cached <- memoise::has_cache(.memoised_download_exercises)(
      athlete_id, team_id
    )

    result <- tryCatch(
      .memoised_download_exercises(athlete_id, team_id),
      error = function(e) {
        cli::cli_alert_warning(
          "Failed to fetch exercises for athlete {athlete_id}: {conditionMessage(e)}"
        )
        tibble::tibble()
      }
    )

    # Delay only after fresh API calls
    if (!is_cached && .delay > 0) {
      Sys.sleep(.delay)
    }

    result
  }

  results <- purrr::map(
    athlete_ids,
    fetch_one,
    .progress = .progress
  )

  dplyr::bind_rows(results)
}
