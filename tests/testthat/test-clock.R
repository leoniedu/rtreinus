# The clock is all that stayed in this package: the session analysis it used
# to serve now lives in yachtvaa.

fixture <- function() readRDS(test_path("fixtures", "session_15min.rds"))
# Athletes 36 and 50 wrote local time; the other eight wrote UTC.
LOCAL <- c(36L, 50L)

test_that("clock detection separates local-time devices from UTC ones", {
  ck <- treinus_detect_clock(fixture())

  expect_setequal(ck$id_athlete[ck$is_local], LOCAL)
  expect_equal(ck$offset_hours[ck$id_athlete == 36], 0)
  expect_equal(ck$offset_hours[ck$id_athlete == 5], -3)
})

test_that("clock detection needs only the records", {
  # A saved snapshot has no exercises table, and replaying a snapshot is the
  # main use case, so this must not depend on start_time_as_string.
  bare <- fixture()[c("id_athlete", "id_exercise", "timestamp",
                      "position_lat", "position_long")]
  expect_setequal(
    with(treinus_detect_clock(bare), id_athlete[is_local]),
    LOCAL
  )
})

test_that("fixing the clock puts every device in the same hour", {
  r <- treinus_fix_clock(fixture(), local = LOCAL)

  expect_s3_class(r$ts, "POSIXct")
  expect_equal(attr(r$ts, "tzone"), "America/Bahia")
  # The fixture spans about 35 minutes; a device still three hours out would
  # stretch the range far beyond that.
  expect_lt(as.numeric(diff(range(r$ts)), units = "mins"), 45)
})

test_that("the clock fallback warns whenever it is used", {
  # Without the exercises table the offsets are a guess that assumes one
  # training on the day. On a day with a morning crew and a midday crew it
  # reported nine local-time devices where there were two, so the caller has
  # to be told which method produced the answer.
  expect_warning(treinus_detect_clock(fixture()), "clustering start times")

  out <- suppressWarnings(treinus_detect_clock(fixture()))
  expect_equal(unique(out$method), "clustered")
})

test_that("with the exercises table the offsets are measured, not guessed", {
  ex <- tryCatch(treinus_get_exercises_db(), error = function(e) NULL)
  skip_if(is.null(ex), "local database not available")
  path <- test_path("..", "..", "vignettes", "data", "treino_20260926.rds")
  skip_if_not(file.exists(path), "reference session not available")

  expect_no_warning(out <- treinus_detect_clock(readRDS(path), ex))
  expect_equal(unique(out$method), "exercises")
  expect_setequal(out$id_athlete[out$is_local], LOCAL)
})

test_that("a trimmed recording is not mistaken for a clock offset", {
  ex <- tryCatch(treinus_get_exercises_db(), error = function(e) NULL)
  skip_if(is.null(ex), "local database not available")

  # The fixture starts 32 minutes into its exercises. Rounding that difference
  # to the nearest hour would call it a one-hour offset and mislabel every
  # local-time device, so the exercises path must decline and say so.
  expect_warning(out <- treinus_detect_clock(fixture(), ex), "clustering")
  expect_equal(unique(out$method), "clustered")
})
