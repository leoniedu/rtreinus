#' Authenticate with Treinus
#'
#' Creates an authenticated session with the Treinus webapp. This function
#' handles the ASP.NET WebForms authentication flow, including extracting
#' ViewState tokens and managing session cookies.
#'
#' For Shiny apps or other cases where you need to present team selection UI,
#' use [treinus_login()] and [treinus_select_team()] instead.
#'
#' @param email Character string with your Treinus email/username.
#'   Can also be set via `TREINUS_EMAIL` environment variable.
#' @param password Character string with your Treinus password.
#'   Can also be set via `TREINUS_PASSWORD` environment variable.
#' @param team_id Integer team ID to select if the user belongs to multiple teams.
#'   Can also be set via `TREINUS_TEAM_ID` environment variable.
#'   If NULL and user has multiple teams, uses the first (default) team.
#' @param remember_me Logical. Keep session alive for future requests.
#'   Default is TRUE.
#' @param base_url Character string with the base URL for Treinus.
#'   Default is "https://webapp.treinus.com.br".
#'
#' @return A `treinus_session` object that can be used for API calls.
#'   The session stores `team_id` as an attribute for use in subsequent calls.
#'
#' @examples
#' \dontrun{
#' # Authenticate using environment variables (recommended)
#' # Set in .Renviron: TREINUS_EMAIL, TREINUS_PASSWORD, TREINUS_TEAM_ID
#' session <- treinus_auth()
#'
#' # Or provide credentials directly
#' session <- treinus_auth(
#'   email = "your.email@example.com",
#'   password = "your-password",
#'   team_id = 2994
#' )
#' }
#'
#' @seealso [treinus_login()], [treinus_select_team()] for Shiny apps
#' @export
treinus_auth <- function(
    email = NULL,
    password = NULL,
    team_id = NULL,
    remember_me = TRUE,
    base_url = "https://webapp.treinus.com.br") {
  # Get team_id from environment if not provided
  team_id <- team_id %||% Sys.getenv("TREINUS_TEAM_ID", unset = NA)

  # Do initial login
  login_result <- treinus_login(
    email = email,
    password = password,
    remember_me = remember_me,
    base_url = base_url
  )

  # If already a complete session, return it
  if (inherits(login_result, "treinus_session")) {
    return(login_result)
  }

  # Otherwise, need to select a team
  treinus_select_team(login_result, team = team_id)
}


#' Login to Treinus (step 1 of authentication)
#'
#' Performs the initial login to Treinus. If the user belongs to multiple teams,
#' returns a pending login object that can be used with [treinus_select_team()].
#' This allows Shiny apps to present a team selection UI.
#'
#' @inheritParams treinus_auth
#'
#' @return Either:
#' - A `treinus_session` object if no team selection is needed
#' - A `treinus_pending_login` object with `$teams` tibble if team selection is needed
#'
#' @examples
#' \dontrun{
#' # For Shiny apps - login first, then check if team selection needed
#' login_result <- treinus_login(email, password)
#'
#' if (inherits(login_result, "treinus_pending_login")) {
#'   # Show team selection UI using login_result$teams
#'   print(login_result$teams)
#'   # Then complete with treinus_select_team()
#' } else {
#'   # Already authenticated, use login_result as session
#' }
#' }
#'
#' @seealso [treinus_select_team()], [treinus_auth()]
#' @export
treinus_login <- function(
    email = NULL,
    password = NULL,
    remember_me = TRUE,
    base_url = "https://webapp.treinus.com.br") {
  # Get credentials from environment if not provided
  email <- email %||% Sys.getenv("TREINUS_EMAIL")
  password <- password %||% Sys.getenv("TREINUS_PASSWORD")

  # Validate credentials
  if (email == "" || password == "") {
    cli::cli_abort(c(
      "!" = "Credentials not provided.",
      "i" = "Set {.envvar TREINUS_EMAIL} and {.envvar TREINUS_PASSWORD} environment variables,",
      "i" = "or provide {.arg email} and {.arg password} arguments."
    ))
  }

  cli::cli_progress_step("Authenticating with Treinus...")

  # Step 1: Get the login page to extract ViewState tokens
  login_url <- paste0(base_url, "/Default.aspx")

  initial_req <- httr2::request(login_url) |>
    httr2::req_user_agent(treinus_ua())

  initial_resp <- initial_req |>
    httr2::req_perform()

  # Extract ViewState tokens using rvest
  login_page <- httr2::resp_body_html(initial_resp)

  viewstate <- login_page |>
    rvest::html_element("input[name='__VIEWSTATE']") |>
    rvest::html_attr("value")

  viewstate_generator <- login_page |>
    rvest::html_element("input[name='__VIEWSTATEGENERATOR']") |>
    rvest::html_attr("value")

  if (is.na(viewstate) || is.na(viewstate_generator)) {
    cli::cli_abort(c(
      "x" = "Failed to extract ViewState tokens from login page.",
      "i" = "The page structure may have changed."
    ))
  }

  # Step 2: Build the login POST request
  login_body <- list(
    `__EVENTTARGET` = "",
    `__EVENTARGUMENT` = "",
    `__VIEWSTATE` = viewstate,
    `__VIEWSTATEGENERATOR` = viewstate_generator,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$UserName` = email,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$Password` = password,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$RememberMe` = if (remember_me) "C" else "U",
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$LoginButton` = "Entrar"
  )

  # Step 3: Perform login (don't follow redirects to capture cookies)
  login_resp <- httr2::request(login_url) |>
    httr2::req_user_agent(treinus_ua()) |>
    httr2::req_body_form(!!!login_body) |>
    httr2::req_method("POST") |>
    httr2::req_options(followlocation = FALSE) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # Check if login was successful (expect 302 redirect on success)
  status <- httr2::resp_status(login_resp)
  if (status >= 400) {
    cli::cli_abort(c(
      "x" = "Login failed with status {status}.",
      "i" = "Check your credentials."
    ))
  }

  # Small delay to allow redirect headers to be processed
  Sys.sleep(0.5)

  # Extract cookies from the login response (set on 302 redirect)
  set_cookie_headers <- httr2::resp_headers(login_resp, "set-cookie")

  if (is.null(set_cookie_headers) || length(set_cookie_headers) == 0) {
    cli::cli_abort(c(
      "x" = "No session cookies received from login.",
      "i" = "Authentication may have failed silently."
    ))
  }

  # Parse cookies into name=value format for the Cookie header
  cookie_values <- vapply(
    set_cookie_headers,
    function(cookie) {
      strsplit(cookie, ";")[[1]][1]
    },
    character(1),
    USE.NAMES = FALSE
  )
  cookie_string <- paste(cookie_values, collapse = "; ")

  # Get redirect location and follow it
  # Allow for a slight delay in redirect header processing
  redirect_url <- httr2::resp_header(login_resp, "location")
  if (is.null(redirect_url)) {
    # Try again after a small additional delay
    Sys.sleep(1)
    redirect_url <- httr2::resp_header(login_resp, "location")
  }

  if (is.null(redirect_url)) {
    cli::cli_abort("Login did not redirect. Check credentials.")
  }

  # Handle relative URLs
  if (!grepl("^https?://", redirect_url)) {
    redirect_url <- paste0(base_url, "/", sub("^/", "", redirect_url))
  }

  # Follow the redirect with cookies
  redirect_resp <- httr2::request(redirect_url) |>
    httr2::req_user_agent(treinus_ua()) |>
    httr2::req_headers(Cookie = cookie_string) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  final_url <- redirect_resp$url

  # Check if we were redirected to team selection page
  if (grepl("LoginSelectTeam", final_url, ignore.case = TRUE)) {
    cli::cli_progress_done()

    # Extract team information and return pending login
    teams <- extract_teams_from_page(redirect_resp)

    cli::cli_alert_info("Multiple teams available. Use {.fn treinus_select_team} to choose.")

    return(structure(
      list(
        teams = teams,
        cookies = cookie_string,
        base_url = base_url,
        response = redirect_resp
      ),
      class = "treinus_pending_login"
    ))
  }

  # Check if we were redirected to dashboard (successful login)
  if (!grepl("Dashboard|Private", final_url, ignore.case = TRUE)) {
    # Try to extract error message from the response
    error_page <- httr2::resp_body_html(redirect_resp)
    error_msg <- error_page |>
      rvest::html_element(".error, .validation-summary-errors") |>
      rvest::html_text2()

    if (!is.na(error_msg)) {
      cli::cli_abort(c(
        "x" = "Login failed: {error_msg}"
      ))
    } else {
      cli::cli_abort(c(
        "x" = "Login failed.",
        "i" = "Check your credentials."
      ))
    }
  }

  cli::cli_progress_done()
  cli::cli_alert_success("Successfully authenticated with Treinus!")

  # Create completed session
  create_session(base_url, cookie_string, login_resp)
}


#' Select a team (step 2 of authentication)
#'
#' Completes authentication by selecting a team. Use after [treinus_login()]
#' when the user belongs to multiple teams.
#'
#' @param pending_login A `treinus_pending_login` object from [treinus_login()]
#' @param team Team ID (numeric) or team name (character) to select.
#'   If NULL, uses the first (default) team.
#'
#' @return A `treinus_session` object that can be used for API calls.
#'
#' @examples
#' \dontrun{
#' # Login first
#' login_result <- treinus_login(email, password)
#'
#' # Check available teams
#' if (inherits(login_result, "treinus_pending_login")) {
#'   print(login_result$teams)
#'   #   id                    name
#'   #   600                   Team A
#'   #   601                   Team B
#'
#'   # Select by name or ID
#'   session <- treinus_select_team(login_result, team = "Team A")
#'   # or
#'   session <- treinus_select_team(login_result, team = 600)
#' }
#' }
#'
#' @seealso [treinus_login()], [treinus_auth()]
#' @export
treinus_select_team <- function(pending_login, team = NULL) {
  if (!inherits(pending_login, "treinus_pending_login")) {
    cli::cli_abort(c(
      "x" = "{.arg pending_login} must be a {.cls treinus_pending_login} object from {.fn treinus_login}.",
      "i" = "If you have a completed session, no team selection is needed."
    ))
  }

  cli::cli_progress_step("Selecting team...")

  teams <- pending_login$teams
  cookies <- pending_login$cookies
  base_url <- pending_login$base_url
  team_resp <- pending_login$response

  team_page <- httr2::resp_body_html(team_resp)

  # Extract ViewState tokens
  viewstate <- team_page |>
    rvest::html_element("input[name='__VIEWSTATE']") |>
    rvest::html_attr("value")

  viewstate_generator <- team_page |>
    rvest::html_element("input[name='__VIEWSTATEGENERATOR']") |>
    rvest::html_attr("value")

  # Determine which team to select
  selected_team_id <- resolve_team_selection(teams, team)

  selected_name <- teams$name[teams$id == selected_team_id]
  cli::cli_alert_info("Selecting team: {selected_name}")

  # Build team selection POST request
  team_select_url <- paste0(base_url, "/LoginSelectTeam.aspx")

  team_body <- list(
    `__EVENTTARGET` = "",
    `__EVENTARGUMENT` = "",
    `__VIEWSTATE` = viewstate,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$rblTeam` = selected_team_id,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginButton` = "Entrar",
    `__VIEWSTATEGENERATOR` = viewstate_generator
  )

  final_resp <- httr2::request(team_select_url) |>
    httr2::req_user_agent(treinus_ua()) |>
    httr2::req_headers(
      Cookie = cookies,
      Referer = team_select_url,
      Origin = base_url
    ) |>
    httr2::req_body_form(!!!team_body) |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # Merge cookies
  cookies <- merge_cookies(cookies, final_resp)

  cli::cli_progress_done()
  cli::cli_alert_success("Successfully authenticated with Treinus!")

  create_session(base_url, cookies, final_resp, team_id = as.integer(selected_team_id))
}


#' Check if a Treinus session is still valid
#'
#' @param session A treinus_session object from [treinus_auth()]
#'
#' @return Logical indicating if the session is still valid
#' @export
treinus_session_valid <- function(session) {
  if (!inherits(session, "treinus_session")) {
    return(FALSE)
  }

  # Try to access a protected page
  test_url <- paste0(attr(session, "base_url"), "/Global/DashBoard/Index")

  result <- tryCatch(
    {
      resp <- session |>
        httr2::req_url(test_url) |>
        httr2::req_perform()

      # Check if we got redirected to login
      !grepl("Default\\.aspx", resp$url, ignore.case = TRUE)
    },
    error = function(e) FALSE
  )

  result
}


#' Print method for pending login
#'
#' @param x A treinus_pending_login object
#' @param ... Additional arguments (unused)
#' @export
print.treinus_pending_login <- function(x, ...) {
  cli::cli_h1("Treinus Pending Login")
  cli::cli_alert_info("Team selection required. Available teams:")
  print(x$teams)
  cli::cli_text("")
  cli::cli_text("Use {.code treinus_select_team(login, team = ...)} to complete authentication.")
  invisible(x)
}


# Internal helpers --------------------------------------------------------

#' User agent for all Treinus requests
#'
#' The Treinus edge rejects libcurl's default user agent with an empty 404,
#' so every request must set one explicitly.
#' @keywords internal
treinus_ua <- function() {
  "rtreinus R package (httr2)"
}

#' Extract teams from team selection page
#' @keywords internal
extract_teams_from_page <- function(response) {
  team_page <- httr2::resp_body_html(response)

  team_radios <- team_page |>
    rvest::html_elements(
      "input[name='ctl00$ctl00$ctl00$FormContent$FormContent$body$rblTeam']"
    )

  team_ids <- team_radios |> rvest::html_attr("value")
  team_labels <- team_page |>
    rvest::html_elements(
      "label[for^='FormContent_FormContent_body_rblTeam']"
    ) |>
    rvest::html_text2()

  if (length(team_ids) == 0) {
    cli::cli_abort("No teams found on team selection page.")
  }

  tibble::tibble(
    id = team_ids,
    name = team_labels
  )
}


#' Resolve team selection to team ID
#' @keywords internal
resolve_team_selection <- function(teams, team) {
  if (is.null(team) || (is.character(team) && (is.na(team) || team == ""))) {
    # Use the first (default) team
    return(teams$id[1])
  }

  if (is.numeric(team) || grepl("^[0-9]+$", as.character(team))) {
    # Team specified by ID
    team_id_str <- as.character(team)
    if (team_id_str %in% teams$id) {
      return(team_id_str)
    } else {
      cli::cli_abort(c(
        "x" = "Team ID {team} not found.",
        "i" = "Available teams:",
        format_teams_list(teams)
      ))
    }
  }

  # Team specified by name (partial match, case-insensitive)
  matches <- grep(team, teams$name, ignore.case = TRUE)
  if (length(matches) == 1) {
    return(teams$id[matches])
  } else if (length(matches) > 1) {
    cli::cli_abort(c(
      "x" = "Multiple teams match '{team}'.",
      "i" = "Matching teams: {paste(teams$name[matches], collapse = ', ')}",
      "i" = "Use the team ID for exact selection."
    ))
  } else {
    cli::cli_abort(c(
      "x" = "No team matches '{team}'.",
      "i" = "Available teams:",
      format_teams_list(teams)
    ))
  }
}


#' Format teams for error messages
#' @keywords internal
format_teams_list <- function(teams) {
  stats::setNames(
    paste(teams$id, "-", teams$name),
    rep("*", nrow(teams))
  )
}


#' Merge cookies from response
#' @keywords internal
merge_cookies <- function(existing_cookies, response) {
  new_cookie_headers <- httr2::resp_headers(response, "set-cookie")

  if (is.null(new_cookie_headers) || length(new_cookie_headers) == 0) {
    return(existing_cookies)
  }

  # Parse existing cookies into a named list
  existing_pairs <- strsplit(existing_cookies, "; ")[[1]]
  cookie_list <- stats::setNames(
    vapply(existing_pairs, function(p) sub("^[^=]+=", "", p), character(1)),
    vapply(existing_pairs, function(p) sub("=.*", "", p), character(1))
  )

  # Add/update with new cookies
  for (cookie in new_cookie_headers) {
    pair <- strsplit(cookie, ";")[[1]][1]
    name <- sub("=.*", "", pair)
    value <- sub("^[^=]+=", "", pair)
    cookie_list[[name]] <- value
  }

  # Rebuild cookie string
  paste(names(cookie_list), cookie_list, sep = "=", collapse = "; ")
}


#' Create a treinus_session object
#' @keywords internal
create_session <- function(base_url, cookies, response, team_id = NULL) {
  authenticated_req <- httr2::request(base_url) |>
    httr2::req_user_agent(treinus_ua()) |>
    httr2::req_headers(Cookie = cookies)

  structure(
    authenticated_req,
    class = c("treinus_session", class(authenticated_req)),
    base_url = base_url,
    cookies = cookies,
    team_id = team_id
  )
}
