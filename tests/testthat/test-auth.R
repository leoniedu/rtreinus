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
