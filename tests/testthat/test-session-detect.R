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

test_that("crew detection recovers the two crews", {
  r <- treinus_fix_clock(fixture(), local = LOCAL)
  crews <- treinus_detect_crews(r)

  expect_length(unique(crews$crews$crew), 2)
  expect_length(crews$unassigned, 0)

  by_crew <- split(crews$crews$id_athlete, crews$crews$crew)
  expect_true(any(vapply(by_crew,
                         \(x) setequal(x, c(5, 7, 8, 37, 53)), logical(1))))
  expect_true(any(vapply(by_crew,
                         \(x) setequal(x, c(36, 38, 48, 50, 54)), logical(1))))
})

test_that("crew detection is stable across the usable radius range", {
  r <- treinus_fix_clock(fixture(), local = LOCAL)
  # The radius must cover a bow-to-stern pair plus GPS error. Anything in this
  # band should give the same answer; if it stops doing so, the default is no
  # longer sitting in a stable region.
  runs <- lapply(c(15, 20, 25, 30), \(m) treinus_detect_crews(r, near_m = m)$crews)
  for (i in seq_along(runs)[-1]) expect_equal(runs[[i]], runs[[1]])
})

test_that("the steerer is found where they sit clear of the pack", {
  r <- treinus_fix_clock(fixture(), local = LOCAL)
  crews <- treinus_detect_crews(r)
  seats <- treinus_detect_steerer(r, crews$crews)

  # Atleta 2 (8) steered her canoe and sits several metres clear of the next
  # seat, so she is identified confidently.
  sandra <- seats$steerer[seats$steerer$id_athlete == 8, ]
  expect_equal(nrow(sandra), 1)
  expect_true(sandra$confident)
  expect_true(all(seats$steerer$n_recording == 5))
})

test_that("no confidence is claimed where the gap is inside the noise", {
  r <- treinus_fix_clock(fixture(), local = LOCAL)
  crews <- treinus_detect_crews(r)
  seats <- treinus_detect_steerer(r, crews$crews)

  # In the other canoe two paddlers sit within half a metre of each other and
  # their order flips between blocks, so whichever is named must not be
  # presented as settled.
  other <- seats$steerer[seats$steerer$id_athlete != 8, ]
  expect_equal(nrow(other), 1)
  expect_false(other$confident)
  # Specifically, the ordering does not hold across segments of the session.
  expect_false(other$stable)
})

test_that("sector consistency is reported alongside the margin", {
  r <- treinus_fix_clock(fixture(), local = LOCAL)
  crews <- treinus_detect_crews(r)
  seats <- treinus_detect_steerer(r, crews$crews)

  expect_true(all(c("consistent", "d_n", "d_s") %in% names(seats$pairs)))
  # Consistency is only claimed where both directions were actually observed.
  both <- !is.na(seats$pairs$d_n) & !is.na(seats$pairs$d_s)
  expect_false(any(seats$pairs$consistent[!both]))
})

test_that("connected components handles an isolated athlete", {
  comp <- connected_components(c(1L, 2L), c(2L, 3L))
  expect_equal(nrow(comp), 3)
  expect_length(unique(comp$crew), 1)
})


test_that("no seating order is emitted, because the data cannot support one", {
  r <- treinus_fix_clock(fixture(), local = LOCAL)
  crews <- treinus_detect_crews(r)
  out <- treinus_detect_steerer(r, crews$crews)

  # Per-device GPS bias is comparable to the spacing between seats, so only the
  # stern separates reliably. A rank column would invite reading a bow-to-stern
  # map that is not there.
  expect_false("seat" %in% names(out$offsets))
  expect_true(all(c("along_m", "is_stern") %in% names(out$offsets)))
  expect_equal(sum(out$offsets$is_stern), length(unique(out$offsets$crew)))
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
