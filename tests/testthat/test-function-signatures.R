test_that("treinus_get_exercises accepts session as optional named arg", {
  # session should not be the first argument
  args <- formals(treinus_get_exercises)
  arg_names <- names(args)

  expect_equal(arg_names[1], "athlete_id")
  expect_true("session" %in% arg_names)
  # session should default to NULL
  expect_null(args$session)
})

test_that("treinus_get_exercises no longer has use_memoise parameter", {
  args <- names(formals(treinus_get_exercises))
  expect_false("use_memoise" %in% args)
})

test_that("treinus_get_exercise_analysis accepts session as optional named arg", {
  args <- formals(treinus_get_exercise_analysis)
  arg_names <- names(args)

  expect_equal(arg_names[1], "exercise_id")
  expect_true("session" %in% arg_names)
  expect_null(args$session)
})

test_that("treinus_get_exercises errors when session=NULL and use_db=FALSE", {
  withr::local_envvar(
    TREINUS_ATHLETE_ID = "50",
    TREINUS_TEAM_ID = "2994"
  )
  expect_error(
    treinus_get_exercises(athlete_id = 50, session = NULL, use_db = FALSE),
    "session.*use_db"
  )
})

test_that("treinus_get_exercises rejects invalid session objects", {
  withr::local_envvar(
    TREINUS_ATHLETE_ID = "50",
    TREINUS_TEAM_ID = "2994"
  )
  expect_error(
    treinus_get_exercises(athlete_id = 50, session = "not_a_session"),
    "treinus_session"
  )
})

test_that("treinus_get_exercise_analysis rejects invalid session objects", {
  withr::local_envvar(
    TREINUS_ATHLETE_ID = "50",
    TREINUS_TEAM_ID = "2994"
  )
  expect_error(
    treinus_get_exercise_analysis(exercise_id = 1, session = "not_a_session"),
    "treinus_session"
  )
})

test_that("treinus_clear_memoise exists and is exported", {
  expect_true(is.function(treinus_clear_memoise))
})

test_that("treinus_clean_old_cache exists and is exported", {
  expect_true(is.function(treinus_clean_old_cache))
})
