test_that("authentication creates session object", {
  skip_if_not(treinus_has_credentials(), "Credentials not configured")
  
  session <- treinus_auth()
  
  expect_s3_class(session, "treinus_session")
  expect_true(treinus_session_valid(session))
})

test_that("authentication fails with invalid credentials", {
  expect_error(
    treinus_auth(email = "fake@example.com", password = "wrongpassword"),
    "Login|redirect|credentials"
  )
})

test_that("authentication requires credentials", {
  withr::local_envvar(TREINUS_EMAIL = "", TREINUS_PASSWORD = "")
  
  expect_error(
    treinus_auth(),
    "Credentials not provided"
  )
})

test_that("every request built in auth.R sets a user agent", {
  # The Treinus edge answers libcurl's default user agent with an empty 404,
  # so a bare httr2::request() anywhere in the login flow breaks authentication.
  # The skip has to come before the read: under R CMD check the sources are not
  # beside the tests, and readLines() errors rather than returning nothing.
  caminho <- test_path("..", "..", "R", "auth.R")
  skip_if_not(file.exists(caminho), "auth.R not available (installed package)")
  src <- readLines(caminho)
  request_lines <- grep("httr2::request\\(", src)
  skip_if(length(request_lines) == 0, "no requests found")

  missing_ua <- Filter(
    function(i) !any(grepl("req_user_agent", src[i:min(i + 2, length(src))])),
    request_lines
  )

  expect_equal(missing_ua, integer(0))
})
