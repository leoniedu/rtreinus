#' Set Treinus credentials
#'
#' Interactively set or update your Treinus credentials in .Renviron.
#' This is the recommended way to store credentials securely.
#'
#' @param email Optional email to set. If NULL, will prompt interactively.
#' @param password Optional password to set. If NULL, will prompt interactively.
#' @param team_id Optional integer team ID for users with multiple teams.
#' @param athlete_id Optional integer athlete ID.
#' @param overwrite Logical. Overwrite existing credentials? Default FALSE.
#'
#' @return Invisibly returns TRUE if credentials were set successfully
#'
#' @examples
#' \dontrun{
#' # Interactive setup
#' treinus_set_credentials()
#'
#' # Or provide directly
#' treinus_set_credentials(
#'   email = "your.email@example.com",
#'   password = "your-password",
#'   team_id = 2994,
#'   athlete_id = 50
#' )
#' }
#'
#' @export
treinus_set_credentials <- function(email = NULL, password = NULL, team_id = NULL,
                                    athlete_id = NULL, overwrite = FALSE) {
  
  # Get .Renviron path
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
  
  # Check if credentials already exist
  existing_email <- Sys.getenv("TREINUS_EMAIL", unset = "")
  existing_password <- Sys.getenv("TREINUS_PASSWORD", unset = "")
  
  if (existing_email != "" && !overwrite) {
    cli::cli_alert_info("Credentials already set.")
    cli::cli_alert_info("Use {.code overwrite = TRUE} to replace them.")
    return(invisible(FALSE))
  }
  
  # Interactive prompts if not provided
  if (is.null(email)) {
    email <- readline("Treinus email: ")
  }
  
  if (is.null(password)) {
    if (requireNamespace("getPass", quietly = TRUE)) {
      password <- getPass::getPass("Treinus password: ")
    } else {
      cli::cli_alert_warning("Install {.pkg getPass} for secure password input.")
      password <- readline("Treinus password: ")
    }
  }
  
  # Validate inputs
  if (email == "" || password == "") {
    cli::cli_abort("Both email and password are required.")
  }
  
  # Read existing .Renviron
  if (file.exists(renviron_path)) {
    renviron_lines <- readLines(renviron_path)
    # Remove old Treinus credentials
    renviron_lines <- renviron_lines[
      !grepl("^TREINUS_EMAIL=", renviron_lines) &
      !grepl("^TREINUS_PASSWORD=", renviron_lines) &
      !grepl("^TREINUS_TEAM_ID=", renviron_lines) &
      !grepl("^TREINUS_TEAM=", renviron_lines) &
      !grepl("^TREINUS_ATHLETE_ID=", renviron_lines)
    ]
  } else {
    renviron_lines <- character(0)
  }
  

  # Escape any double quotes in the values

  email_escaped <- gsub('"', '\\"', email, fixed = TRUE)
  password_escaped <- gsub('"', '\\"', password, fixed = TRUE)

  # Add new credentials (quoted to handle special characters)
  new_lines <- c(
    renviron_lines,
    sprintf('TREINUS_EMAIL="%s"', email_escaped),
    sprintf('TREINUS_PASSWORD="%s"', password_escaped)
  )

  # Add team_id if specified
  if (!is.null(team_id)) {
    new_lines <- c(new_lines, sprintf('TREINUS_TEAM_ID="%s"', as.integer(team_id)))
  }

  # Add athlete_id if specified
  if (!is.null(athlete_id)) {
    new_lines <- c(new_lines, sprintf('TREINUS_ATHLETE_ID="%s"', as.integer(athlete_id)))
  }

  # Write back to .Renviron
  writeLines(new_lines, renviron_path)
  
  cli::cli_alert_success("Credentials saved to {.file {renviron_path}}")
  cli::cli_alert_info("Restart R for changes to take effect, or run:")
  cli::cli_code("readRenviron('~/.Renviron')")
  
  invisible(TRUE)
}


#' Check if Treinus credentials are configured
#'
#' @return Logical indicating if credentials are set
#'
#' @examples
#' treinus_has_credentials()
#'
#' @export
treinus_has_credentials <- function() {
  email <- Sys.getenv("TREINUS_EMAIL", unset = "")
  password <- Sys.getenv("TREINUS_PASSWORD", unset = "")
  
  email != "" && password != ""
}


#' Get Treinus configuration
#'
#' Returns current configuration including credential status.
#'
#' @return A list with configuration details
#'
#' @examples
#' treinus_config()
#'
#' @export
treinus_config <- function() {
  has_creds <- treinus_has_credentials()
  team <- Sys.getenv("TREINUS_TEAM", unset = "")

  config <- list(
    credentials_configured = has_creds,
    email_set = Sys.getenv("TREINUS_EMAIL", unset = "") != "",
    password_set = Sys.getenv("TREINUS_PASSWORD", unset = "") != "",
    team_set = team != "",
    user_agent = "rtreinus R package (httr2)"
  )

  if (has_creds) {
    config$email <- Sys.getenv("TREINUS_EMAIL")
  }
  if (team != "") {
    config$team <- team
  }

  class(config) <- c("treinus_config", "list")
  config
}


#' @export
print.treinus_config <- function(x, ...) {
  cli::cli_h1("Treinus Configuration")

  if (x$credentials_configured) {
    cli::cli_alert_success("Credentials configured")
    cli::cli_alert_info("Email: {x$email}")
    if (x$team_set) {
      cli::cli_alert_info("Team: {x$team}")
    }
  } else {
    cli::cli_alert_danger("Credentials not configured")
    cli::cli_alert_info("Run {.run treinus_set_credentials()} to set up")
  }

  invisible(x)
}
