# Helper to seed a temp DB with two athletes
seed_test_db <- function() {
  exercises <- tibble::tibble(
    id_team = c(1L, 1L),
    id_athlete = c(50L, 51L),
    id_exercise = c(100L, 200L),
    start = c("2026-01-01T08:00:00", "2026-08-01T08:00:00"),
    distance = c(5000, 6000)
  )
  store_exercises_in_db(exercises[1, ], team_id = 1, athlete_id = 50)
  store_exercises_in_db(exercises[2, ], team_id = 1, athlete_id = 51)
}

test_that("use_db path without session filters by athlete_id and needs no team_id", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )
    withr::local_envvar(TREINUS_ATHLETE_ID = NA, TREINUS_TEAM_ID = NA)
    seed_test_db()

    result <- suppressMessages(
      treinus_get_exercises(athlete_id = 50, use_db = TRUE)
    )

    expect_equal(nrow(result), 1)
    expect_equal(result$id_athlete, 50L)
  })
})

test_that("use_db path without session or athlete_id returns all rows", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )
    withr::local_envvar(TREINUS_ATHLETE_ID = NA, TREINUS_TEAM_ID = NA)
    seed_test_db()

    result <- suppressMessages(treinus_get_exercises(use_db = TRUE))

    expect_equal(nrow(result), 2)
  })
})

test_that("store_exercises_in_db with overwrite = FALSE keeps existing rows", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )
    row <- tibble::tibble(
      id_team = 1L, id_athlete = 50L, id_exercise = 100L, distance = 5000
    )
    store_exercises_in_db(row, team_id = 1, athlete_id = 50)

    row$distance <- 9999
    n <- store_exercises_in_db(row, team_id = 1, athlete_id = 50, overwrite = FALSE)
    expect_equal(n, 0L)

    con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(getwd(), "test.db"))
    on.exit(RSQLite::dbDisconnect(con))
    expect_equal(
      RSQLite::dbGetQuery(con, "SELECT distance FROM exercises")$distance,
      5000
    )
  })
})

test_that("store_exercises_in_db with overwrite = TRUE updates existing rows", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )
    row <- tibble::tibble(
      id_team = 1L, id_athlete = 50L, id_exercise = 100L, distance = 5000
    )
    store_exercises_in_db(row, team_id = 1, athlete_id = 50)

    row$distance <- 9999
    n <- store_exercises_in_db(row, team_id = 1, athlete_id = 50, overwrite = TRUE)
    expect_equal(n, 1L)

    con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(getwd(), "test.db"))
    on.exit(RSQLite::dbDisconnect(con))
    expect_equal(
      RSQLite::dbGetQuery(con, "SELECT distance FROM exercises")$distance,
      9999
    )
  })
})

test_that("store_exercises_in_db leaves no tmp_exercises table behind", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )
    row <- tibble::tibble(
      id_team = 1L, id_athlete = 50L, id_exercise = 100L, distance = 5000
    )
    store_exercises_in_db(row, team_id = 1, athlete_id = 50)
    store_exercises_in_db(row, team_id = 1, athlete_id = 50)

    con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(getwd(), "test.db"))
    on.exit(RSQLite::dbDisconnect(con))
    expect_false("tmp_exercises" %in% RSQLite::dbListTables(con))
  })
})

test_that("clear_exercises_db older_than works with Date input", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )
    seed_test_db()

    n <- suppressMessages(
      clear_exercises_db(older_than = as.Date("2026-06-01"))
    )
    expect_equal(n, 1L)

    con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(getwd(), "test.db"))
    on.exit(RSQLite::dbDisconnect(con))
    expect_equal(
      RSQLite::dbGetQuery(con, "SELECT id_exercise FROM exercises")$id_exercise,
      200L
    )
  })
})

test_that("treinus_config reads TREINUS_TEAM_ID", {
  withr::local_envvar(
    TREINUS_EMAIL = "a@b.c", TREINUS_PASSWORD = "x",
    TREINUS_TEAM = NA, TREINUS_TEAM_ID = "2994"
  )
  config <- treinus_config()
  expect_true(config$team_set)
  expect_equal(config$team, "2994")
})
