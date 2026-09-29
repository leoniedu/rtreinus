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

test_that("`local` means that athlete is three hours later than the rest", {
  # The sign of this is the contract between two different parameterisations, and
  # getting it backwards would double a clock error instead of removing it.
  #
  # Here the athlete listed in `local` is the one left alone, and everybody else is
  # moved by tz_offset_hours — so the *net* effect is that athlete three hours later
  # than the rest. The iOS port says the same thing as "+3 hours for that athlete",
  # which is only equivalent because of that inversion.
  base <- as.POSIXct("2026-09-26 07:00:00", tz = "UTC")
  records <- data.frame(
    id_athlete = rep(c(1L, 2L), each = 3L),
    id_exercise = rep(c(1L, 2L), each = 3L),
    timestamp = rep(base + c(0, 10, 20), times = 2)
  )

  todos <- treinus_fix_clock(records, local = integer())
  um <- treinus_fix_clock(records, local = 1L)

  desvio <- function(fixed, quem) {
    min(as.numeric(fixed$ts[fixed$id_athlete == quem]))
  }

  expect_equal((desvio(um, 1L) - desvio(todos, 1L)) / 3600, 3)
  expect_equal(desvio(um, 2L) - desvio(todos, 2L), 0)

  # And with nobody listed, the epoch does not move at all. The subtraction of
  # tz_offset_hours and the `force_tz` relabel cancel: the shift moves the instant
  # back three hours and the relabel reads the same wall clock as Bahia, which puts
  # it back. So the whole net effect of this function is to move the listed athletes
  # three hours later, and to leave everyone else exactly as they arrived.
  #
  # Measured rather than reasoned about — the first version of this test predicted
  # -3 and was wrong.
  expect_equal(desvio(todos, 1L) - as.numeric(base), 0)
})

test_that("the exact branch works from the committed fixture alone", {
  # The exact branch is what production uses, and the only test of it needed a local
  # database and an untracked session file — so in a fresh clone the path that matters
  # skipped and the one R warns about was all that ran.
  #
  # The exercises table is built here instead. `start_time_as_string` is the local start
  # a device reported, so for the two devices that wrote local time it is their first
  # sample's clock face, and for the eight that wrote UTC it is three hours earlier.
  # That is the ground truth the fixture was chosen for.
  #
  # Read with "a trimmed recording is not mistaken for a clock offset" above, which
  # feeds the same branch a real exercises table and expects it to decline: this
  # fixture's recordings begin 32 minutes after their reported start, and rounding that
  # to an hour would mislabel every device. So one of the pair covers the arithmetic on
  # recordings that begin when they say they do, and the other covers the gate that
  # refuses when they do not.
  r <- fixture()
  r$.ts <- as_treinus_time(r$timestamp)

  primeiro <- r |>
    dplyr::summarise(inicio = min(.data$.ts), .by = c("id_athlete", "id_exercise"))

  ex <- primeiro |>
    dplyr::mutate(
      local = dplyr::if_else(.data$id_athlete %in% LOCAL,
                             .data$inicio, .data$inicio - 3 * 3600),
      start_time_as_string = format(.data$local, "%H:%M:%S", tz = "UTC")
    ) |>
    dplyr::select("id_exercise", "id_athlete", "start_time_as_string")

  expect_no_warning(out <- treinus_detect_clock(r, ex))
  expect_equal(unique(out$method), "exercises")
  expect_setequal(out$id_athlete[out$is_local], LOCAL)
  # And every athlete is judged, which is what the exact branch refuses to do
  # partially.
  expect_setequal(out$id_athlete, unique(r$id_athlete))
})
