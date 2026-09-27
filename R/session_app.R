#' Launch the session analysis app
#'
#' Opens the cockpit where the crews, steerers and suspect traces proposed by
#' the detectors are confirmed or overridden, and writes the resulting settings
#' file. The analysis itself lives in ordinary functions; this is only where the
#' decisions get made.
#'
#' @param records A records tibble, or a path to a saved `.rds` of one. Records
#'   come from [treinus_get_exercise_analysis()] via [treinus_extract_records()].
#' @param exercises Optional exercises table. Supplying it makes clock
#'   detection exact rather than heuristic; the app reads it from the local
#'   database by default.
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Invisibly, the result of [shiny::runApp()].
#'
#' @examples
#' \dontrun{
#' treinus_session_app("vignettes/data/treino_20260926.rds")
#' }
#' @export
treinus_session_app <- function(records, exercises = NULL, ...) {
  rlang::check_installed(c("shiny", "yaml"), "to run the session app.")

  source_path <- NA_character_
  if (is.character(records) && length(records) == 1) {
    source_path <- records
    if (!file.exists(source_path)) {
      cli::cli_abort("No such file: {.path {source_path}}")
    }
    records <- readRDS(source_path)
  }

  required <- c("id_athlete", "id_exercise", "timestamp",
                "position_lat", "position_long")
  missing <- setdiff(required, names(records))
  if (length(missing)) {
    cli::cli_abort(c(
      "{.arg records} is missing column{?s} {.field {missing}}.",
      "i" = "Records come from {.fn treinus_extract_records}."
    ))
  }

  app_dir <- system.file("shiny", "session", package = "rtreinus")
  if (!nzchar(app_dir)) {
    cli::cli_abort("App files not found; is rtreinus installed correctly?")
  }

  if (is.null(exercises)) {
    exercises <- tryCatch(treinus_get_exercises_db(), error = function(e) NULL)
  }

  withr::with_options(
    list(rtreinus.session_records = records,
         rtreinus.session_exercises = exercises,
         rtreinus.session_source = source_path),
    shiny::runApp(app_dir, ...)
  )
}


#' Render a session report from a settings file
#'
#' @param settings_path Path to a settings YAML.
#' @param template Path to the Quarto template. Defaults to the one shipped
#'   with the package.
#' @param output Output path for the rendered report.
#'
#' @return The path to the rendered report, invisibly.
#' @export
treinus_render_report <- function(settings_path,
                                  template = NULL,
                                  output = NULL) {
  rlang::check_installed("quarto", "to render session reports.")
  if (!nzchar(Sys.which("quarto"))) {
    cli::cli_abort(c(
      "The {.strong quarto} command-line tool was not found.",
      "i" = "Install it from {.url https://quarto.org/docs/get-started/}."
    ))
  }
  settings <- treinus_read_settings(settings_path)

  if (is.null(template)) {
    template <- system.file("templates", "session_report.qmd",
                            package = "rtreinus")
  }
  if (!file.exists(template)) {
    cli::cli_abort("Template not found: {.path {template}}")
  }

  if (is.null(output)) {
    output <- sprintf("treino_%s.html",
                      format(as.Date(settings$session$date), "%Y%m%d"))
  }

  dest <- file.path(dirname(output), basename(template))
  if (!identical(normalizePath(template, mustWork = FALSE),
                 normalizePath(dest, mustWork = FALSE))) {
    file.copy(template, dest, overwrite = TRUE)
  }

  quarto::quarto_render(
    dest,
    execute_params = list(settings = normalizePath(settings_path)),
    output_file = basename(output)
  )
  invisible(output)
}
