# Package startup and shutdown functions

# Package-level environment for session state
.rtreinus_env <- new.env(parent = emptyenv())
.rtreinus_env$session <- NULL

# Global variable for memoised function
.memoised_download_exercises <- NULL

#' @importFrom memoise memoise
.onLoad <- function(libname, pkgname) {
  cache_dir <- file.path(tools::R_user_dir("rtreinus", "cache"), "memoise")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Memoise with disk cache — keyed only on (athlete_id, team_id),
  # not session, so the cache survives across R sessions.
  .memoised_download_exercises <<- memoise::memoise(
    .download_exercises_cacheable,
    cache = cachem::cache_disk(cache_dir, max_age = 1800)
  )
}

#' Download exercises using the current session from package env
#'
#' This thin wrapper exists so that memoise caches on (athlete_id, team_id)
#' only, not on the session object (which changes every authentication).
#'
#' @keywords internal
.download_exercises_cacheable <- function(athlete_id, team_id) {
  session <- .rtreinus_env$session
  if (is.null(session)) {
    cli::cli_abort("No active session stored. Pass {.arg session} explicitly.")
  }
  download_single_exercise_internal(session, athlete_id, team_id)
}
