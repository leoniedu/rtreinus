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
  src <- readLines(test_path("..", "..", "R", "auth.R"))
  request_lines <- grep("httr2::request\\(", src)
  skip_if(length(request_lines) == 0, "auth.R not available (installed package)")

  missing_ua <- Filter(
    function(i) !any(grepl("req_user_agent", src[i:min(i + 2, length(src))])),
    request_lines
  )

  expect_equal(missing_ua, integer(0))
})
