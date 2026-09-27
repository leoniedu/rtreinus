#' Default analysis parameters
#'
#' The tuning constants a session analysis needs. Every one of these was once a
#' hardcoded literal in a vignette script.
#'
#' @return A named list.
#' @export
treinus_analysis_defaults <- function() {
  list(
    moving_ms = 0.5,
    max_sample_gap_s = 30,
    piece_frac = 0.85,
    piece_min_s = 60,
    piece_cruise_floor_kmh = 3,
    cadence_bin_s = 30,
    cadence_smooth_s = 120,
    cadence_min_on_line = 3,
    reference_distance_m = 14000,
    hr_zones = c(120, 140, 160),
    bin_s = 10
  )
}

SETTINGS_SCHEMA <- 1L

#' Metrics that may be excluded
#'
#' Excluding `speed` or `distance` is not a coherent request: the measures are
#' derived from consecutive samples, so removing them produces `Inf` rather than
#' a missing value.
#' @keywords internal
EXCLUDABLE_METRICS <- c("heart_rate", "cadence")


#' Draft a settings object from detection
#'
#' Runs the detectors and assembles their proposals into a settings object
#' ready to be reviewed, edited and written. Nothing here is a decision: it is
#' what the data suggests, for a human to accept or override.
#'
#' @param records Raw records tibble.
#' @param exercises Optional exercises table. Supplying it makes clock
#'   detection exact; without it the fallback assumes a single training on the
#'   day, which is wrong whenever two groups went out at different times.
#' @param date Session date, as a string or Date.
#' @param source Path recorded in the settings for provenance.
#' @param tz Time zone name.
#' @param ... Passed to [treinus_detect_crews()].
#'
#' @return A settings list, as [treinus_read_settings()] would return.
#' @export
treinus_draft_settings <- function(records,
                                   exercises = NULL,
                                   date = NULL,
                                   source = NA_character_,
                                   tz = "America/Bahia",
                                   ...) {
  clock <- treinus_detect_clock(records, exercises)
  local <- sort(clock$id_athlete[clock$is_local])

  fixed <- treinus_fix_clock(records, local = local, tz = tz)
  crews <- treinus_detect_crews(fixed, ...)
  seats <- treinus_detect_steerer(fixed, crews$crews)

  if (is.null(date)) date <- as.Date(min(fixed$ts))

  members <- split(crews$crews$id_athlete, crews$crews$crew)
  steerer <- stats::setNames(
    as.list(seats$steerer$id_athlete),
    seats$steerer$crew
  )

  list(
    schema = SETTINGS_SCHEMA,
    session = list(
      date = format(as.Date(date)),
      source = source,
      tz = tz,
      crs = NA_integer_
    ),
    exercises = list(exclude = integer()),
    clock = list(local = as.integer(local)),
    crews = list(
      include = names(members),
      members = lapply(members, as.integer),
      steerer = lapply(steerer, as.integer)
    ),
    exclude = list(),
    analysis = treinus_analysis_defaults()
  )
}


#' Read a session settings file
#'
#' @param path Path to a YAML settings file.
#' @return A normalised settings list.
#' @export
treinus_read_settings <- function(path) {
  rlang::check_installed("yaml", "to read session settings.")
  s <- treinus_validate_settings(yaml::read_yaml(path))

  # A relative source is relative to the settings file, not to whatever
  # directory happens to be current. Quarto renders from the document's
  # directory, so anything else breaks depending on where it is run from.
  src <- s$session$source
  if (!is.null(src) && !is.na(src) && nzchar(src) &&
      !startsWith(src, "/") && !grepl("^[A-Za-z]:", src)) {
    s$session$source <- file.path(dirname(normalizePath(path)), src)
  }
  s
}


#' Write a session settings file
#'
#' @param settings A settings list.
#' @param path Destination path.
#' @return `path`, invisibly.
#' @export
treinus_write_settings <- function(settings, path) {
  rlang::check_installed("yaml", "to write session settings.")
  settings <- treinus_validate_settings(settings)
  # Dates must be written as strings: write_yaml() emits a Date as its
  # underlying day number.
  settings$session$date <- format(as.Date(settings$session$date))
  yaml::write_yaml(settings, path)
  invisible(path)
}


#' Validate and normalise a settings list
#'
#' `yaml::write_yaml()` collapses length-one vectors to scalars, so a crew with
#' one member reads back as a bare integer rather than a list. Normalising here
#' means no downstream function has to care.
#'
#' @param settings A settings list.
#' @return The normalised list.
#' @export
treinus_validate_settings <- function(settings) {
  schema <- settings$schema %||% NA_integer_
  if (!identical(as.integer(schema), SETTINGS_SCHEMA)) {
    cli::cli_abort(c(
      "Settings schema {.val {schema}} is not supported.",
      "i" = "This version of rtreinus reads schema {.val {SETTINGS_SCHEMA}}."
    ))
  }

  settings$clock$local <- as_int(settings$clock$local)
  settings$exercises$exclude <- as_int(settings$exercises$exclude)
  settings$crews$include <- as_chr(settings$crews$include)
  settings$crews$members <- lapply(settings$crews$members, as_int)
  settings$crews$steerer <- lapply(settings$crews$steerer, as_int)

  settings$analysis <- utils::modifyList(treinus_analysis_defaults(),
                                         settings$analysis %||% list())

  # An empty exclusion list survives a YAML round trip in several shapes -
  # absent, an empty list, or a zero-column frame - so normalise them all to
  # the same two-column tibble.
  ex <- settings$exclude %||% list()
  ex <- if (is.data.frame(ex)) ex else if (length(ex)) dplyr::bind_rows(ex) else NULL
  if (is.null(ex) || !nrow(ex)) {
    ex <- tibble::tibble(athlete = integer(), metric = character())
  }
  if (nrow(ex)) {
    bad <- setdiff(ex$metric, EXCLUDABLE_METRICS)
    if (length(bad)) {
      cli::cli_abort(c(
        "Cannot exclude metric{?s} {.val {bad}}.",
        "i" = "Only {.val {EXCLUDABLE_METRICS}} may be excluded; the others are
               derived from consecutive samples."
      ))
    }
    ex$athlete <- as.integer(ex$athlete)
  }
  settings$exclude <- ex

  unknown <- setdiff(settings$crews$include, names(settings$crews$members))
  if (length(unknown)) {
    cli::cli_abort("Crew{?s} {.val {unknown}} listed in {.field include} but
                    not in {.field members}.")
  }

  settings
}

as_int <- function(x) if (is.null(x)) integer() else as.integer(unlist(x, use.names = FALSE))
as_chr <- function(x) if (is.null(x)) character() else as.character(unlist(x, use.names = FALSE))
