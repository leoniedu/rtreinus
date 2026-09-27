exercises_or_null <- function() {
  tryCatch(treinus_get_exercises_db(), error = function(e) NULL)
}

reference_session <- function() {
  path <- test_path("..", "..", "vignettes", "data", "treino_20260926.rds")
  skip_if_not(file.exists(path), "reference session not available")
  readRDS(path)
}

# The faults in the reference session, established by hand and used here as
# ground truth. A rule that fires on anyone else is a false positive, which is
# worse than no rule at all.
EXPECTED_FLAGS <- tibble::tribble(
  ~id_athlete, ~rule,              ~action,
  36L,         "hr_never_acquired", "drop",
  50L,         "hr_flatline",       "drop",
  37L,         "cadence_dropout",   "flag",
  53L,         "cadence_dropout",   "flag",
  46L,         "hr_noise",          "flag",
  47L,         "hr_spike",          "flag"
)

test_that("quality rules fire on exactly the known faults and nothing else", {
  raw <- reference_session()
  s <- draft_quietly(raw)
  rec <- treinus_prepare_records(raw, s)
  bs <- treinus_boat_speed(rec, s)
  pc <- treinus_pieces(bs, s)

  got <- treinus_data_quality(rec, bs, pc)

  expect_equal(
    dplyr::arrange(got[c("id_athlete", "rule", "action")], id_athlete, rule),
    dplyr::arrange(EXPECTED_FLAGS, id_athlete, rule),
    ignore_attr = TRUE
  )
})

test_that("heart-rate rules that need boat speed are skipped without it", {
  raw <- reference_session()
  s <- draft_quietly(raw)
  rec <- treinus_prepare_records(raw, s)

  got <- treinus_data_quality(rec, boat_speed = NULL, pieces = NULL)

  expect_false("hr_never_acquired" %in% got$rule)
  expect_false("cadence_dropout" %in% got$rule)
  expect_true("hr_flatline" %in% got$rule)
})

test_that("steerers are exempt from the cadence dropout rule", {
  raw <- reference_session()
  s <- draft_quietly(raw)
  rec <- treinus_prepare_records(raw, s)
  bs <- treinus_boat_speed(rec, s)
  pc <- treinus_pieces(bs, s)

  got <- treinus_data_quality(rec, bs, pc)
  steerers <- unique(rec$id_athlete[rec$is_steerer])

  # A steerer stops paddling to steer, so low readings are the job, not a fault.
  expect_length(intersect(got$id_athlete[got$rule == "cadence_dropout"],
                          steerers), 0)
})

test_that("longest_flat_run measures seconds, not samples", {
  ts <- as.POSIXct("2026-09-26 07:00:00", tz = "UTC") + seq(0, 100, by = 10)
  x <- c(rep(120, 6), 121, 122, 123, 124, 125)

  # Six samples ten seconds apart is fifty seconds, not six.
  expect_equal(longest_flat_run(x, ts), 50)
  expect_equal(longest_flat_run(1, ts[1]), 0)
})

test_that("aborted recordings are caught by duration and by implausible speed", {
  ex <- tibble::tibble(
    id_athlete = c(1L, 2L, 3L),
    id_exercise = c(10L, 11L, 12L),
    total_time = c(70, 5400, 5400),
    distance = c(119.3, 14.5, 14.5),
    speed = c(6, 9.4, NA)
  )

  out <- treinus_aborted_exercises(ex)

  # 11 is a healthy session: 14.5 km in 90 minutes.
  expect_setequal(out$id_exercise, c(10L, 12L))
  expect_match(out$reason[out$id_exercise == 10], "70 s")
  expect_match(out$reason[out$id_exercise == 12], "velocidade")
})

test_that("cadence ranks the steerer by their shortfall against the crew", {
  raw <- reference_session()
  s <- draft_quietly(raw, exercises_or_null())
  s$crews$steerer <- list(`Canoa 1` = 8L, `Canoa 2` = 36L, `Canoa 3` = 60L)
  rec <- treinus_prepare_records(raw, s)
  bs <- treinus_boat_speed(rec, s)
  pc <- treinus_pieces(bs, s)
  bad <- with(treinus_data_quality(rec, bs, pc),
              unique(id_athlete[metric == "cadence"]))

  ranked <- treinus_steerer_by_cadence(rec, pc, s, ignore = bad)

  # A steerer pokes, and a poke is a stroke not counted, so their rate sits
  # below the crew's; the crew themselves cluster at zero.
  helm <- ranked[ranked$crew == "Canoa 2" & ranked$is_steerer, ]
  expect_equal(helm$rank, 1L)
  expect_lt(helm$deficit_spm, 0)
})

test_that("the steerer check is silent when the label matches the evidence", {
  raw <- reference_session()
  s <- draft_quietly(raw, exercises_or_null())
  s$crews$include <- c("Canoa 1", "Canoa 2")
  s$crews$steerer <- list(`Canoa 1` = 8L, `Canoa 2` = 36L)
  rec <- treinus_prepare_records(raw, s)
  bs <- treinus_boat_speed(rec, s)
  pc <- treinus_pieces(bs, s)
  bad <- with(treinus_data_quality(rec, bs, pc),
              unique(id_athlete[metric == "cadence"]))

  expect_equal(nrow(treinus_check_steerer(rec, pc, s, ignore = bad)), 0)
})

test_that("a cadence-flagged device is kept out of the steerer evidence", {
  raw <- reference_session()
  s <- draft_quietly(raw, exercises_or_null())
  s$crews$include <- "Canoa 1"
  s$crews$steerer <- list(`Canoa 1` = 8L)
  rec <- treinus_prepare_records(raw, s)
  bs <- treinus_boat_speed(rec, s)
  pc <- treinus_pieces(bs, s)

  # Atleta 4's watch under-counts strokes, which imitates heavy poking exactly.
  # Left in, she outranks the real steerer; excluded, the ranking is right.
  with_her <- treinus_steerer_by_cadence(rec, pc, s)
  without <- treinus_steerer_by_cadence(rec, pc, s, ignore = 53L)

  expect_equal(with_her$id_athlete[with_her$rank == 1L], 53L)
  expect_equal(without$id_athlete[without$rank == 1L], 8L)
})
