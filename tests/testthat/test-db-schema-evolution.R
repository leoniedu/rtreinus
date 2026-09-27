test_that("store_exercises_in_db creates table on first insert", {
  withr::with_tempdir({
    withr::local_envvar(TREINUSR_DATA_DIR = getwd())
    # Override treinus_db_path to use temp dir
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )

    exercises <- tibble::tibble(
      id_team = 1L,
      id_athlete = 50L,
      id_exercise = 100L,
      distance = 5000,
      calories = 300
    )

    result <- store_exercises_in_db(exercises, team_id = 1, athlete_id = 50)
    expect_equal(result, 1L)

    # Verify table exists with correct data
    con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(getwd(), "test.db"))
    on.exit(RSQLite::dbDisconnect(con))

    expect_true(RSQLite::dbExistsTable(con, "exercises"))
    rows <- RSQLite::dbGetQuery(con, "SELECT * FROM exercises")
    expect_equal(nrow(rows), 1)
    expect_equal(rows$distance, 5000)
  })
})

test_that("store_exercises_in_db adds new columns via ALTER TABLE", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )

    # Initial insert
    initial <- tibble::tibble(
      id_team = 1L,
      id_athlete = 50L,
      id_exercise = 100L,
      distance = 5000,
      calories = 300
    )
    store_exercises_in_db(initial, team_id = 1, athlete_id = 50)

    # Insert with new column
    new_data <- tibble::tibble(
      id_team = 1L,
      id_athlete = 50L,
      id_exercise = 101L,
      distance = 6000,
      calories = 350,
      power_avg = 250
    )

    expect_message(
      store_exercises_in_db(new_data, team_id = 1, athlete_id = 50),
      "Adding.*new column"
    )

    # Verify new column exists
    con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(getwd(), "test.db"))
    on.exit(RSQLite::dbDisconnect(con))

    cols <- RSQLite::dbListFields(con, "exercises")
    expect_true("power_avg" %in% cols)

    # Old row gets NA for new column
    old_row <- RSQLite::dbGetQuery(
      con, "SELECT * FROM exercises WHERE id_exercise = 100"
    )
    expect_equal(old_row$distance, 5000)
    expect_true(is.na(old_row$power_avg))

    # New row has actual value
    new_row <- RSQLite::dbGetQuery(
      con, "SELECT * FROM exercises WHERE id_exercise = 101"
    )
    expect_equal(new_row$power_avg, 250)
  })
})

test_that("store_exercises_in_db preserves columns missing in new data", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )

    # Initial data with extra column
    initial <- tibble::tibble(
      id_team = 1L,
      id_athlete = 50L,
      id_exercise = 100L,
      distance = 5000,
      calories = 300,
      power_avg = 250
    )
    store_exercises_in_db(initial, team_id = 1, athlete_id = 50)

    # New data missing power_avg
    new_data <- tibble::tibble(
      id_team = 1L,
      id_athlete = 50L,
      id_exercise = 101L,
      distance = 6000,
      calories = 350
    )

    expect_message(
      store_exercises_in_db(new_data, team_id = 1, athlete_id = 50),
      "Preserving.*existing column"
    )

    # Verify power_avg column still exists
    con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(getwd(), "test.db"))
    on.exit(RSQLite::dbDisconnect(con))

    cols <- RSQLite::dbListFields(con, "exercises")
    expect_true("power_avg" %in% cols)

    # Old row still has power_avg
    old_row <- RSQLite::dbGetQuery(
      con, "SELECT * FROM exercises WHERE id_exercise = 100"
    )
    expect_equal(old_row$power_avg, 250)

    # New row has NA for power_avg
    new_row <- RSQLite::dbGetQuery(
      con, "SELECT * FROM exercises WHERE id_exercise = 101"
    )
    expect_true(is.na(new_row$power_avg))
  })
})

test_that("store_exercises_in_db upserts existing rows with overwrite = TRUE", {
  withr::with_tempdir({
    local_mocked_bindings(
      treinus_db_path = function() file.path(getwd(), "test.db")
    )

    # Initial insert
    initial <- tibble::tibble(
      id_team = 1L,
      id_athlete = 50L,
      id_exercise = 100L,
      distance = 5000
    )
    store_exercises_in_db(initial, team_id = 1, athlete_id = 50)

    # Update with new value for same key
    updated <- tibble::tibble(
      id_team = 1L,
      id_athlete = 50L,
      id_exercise = 100L,
      distance = 9999
    )
    store_exercises_in_db(updated, team_id = 1, athlete_id = 50, overwrite = TRUE)

    con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(getwd(), "test.db"))
    on.exit(RSQLite::dbDisconnect(con))

    rows <- RSQLite::dbGetQuery(con, "SELECT * FROM exercises")
    expect_equal(nrow(rows), 1)
    expect_equal(rows$distance, 9999)
  })
})

test_that("store_exercises_in_db returns 0 for empty tibble", {
  expect_equal(
    store_exercises_in_db(tibble::tibble(), team_id = 1, athlete_id = 50),
    0L
  )
})

test_that("infer_sql_type returns correct types", {
  expect_equal(infer_sql_type(1:10), "INTEGER")
  expect_equal(infer_sql_type(c(1.5, 2.5)), "REAL")
  expect_equal(infer_sql_type(c(TRUE, FALSE)), "INTEGER")
  expect_equal(infer_sql_type(c("a", "b")), "TEXT")
  expect_equal(infer_sql_type(Sys.Date()), "TEXT")
  expect_equal(infer_sql_type(Sys.time()), "TEXT")
  expect_equal(infer_sql_type(factor("a")), "TEXT")
})
