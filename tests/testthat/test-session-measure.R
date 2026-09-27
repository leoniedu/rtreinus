fixture <- function() readRDS(test_path("fixtures", "session_15min.rds"))

prepared <- function(exclude = list()) {
  raw <- fixture()
  s <- draft_quietly(raw, source = "fixture")
  s$exclude <- exclude
  list(records = treinus_prepare_records(raw, s), settings = s)
}

test_that("preparing records labels crews and roles and caps dt", {
  p <- prepared()

  expect_true(all(c("ts", "dt", "crew", "is_steerer") %in% names(p$records)))
  # One steerer per crew. Which paddler is named in the second crew is not
  # settled by the data (see test-session-detect.R), so only the count and the
  # unambiguous one are asserted here.
  expect_true(8 %in% p$records$id_athlete[p$records$is_steerer])
  expect_length(unique(p$records$id_athlete[p$records$is_steerer]), 2)
  # Auto-pause gaps must not be charged to the session.
  expect_lte(max(p$records$dt), p$settings$analysis$max_sample_gap_s)
})

test_that("exclusion blanks the metric without dropping rows", {
  full <- prepared()
  cut <- prepared(list(list(athlete = 5L, metric = "heart_rate")))

  expect_equal(nrow(cut$records), nrow(full$records))
  expect_true(all(is.na(cut$records$heart_rate[cut$records$id_athlete == 5])))
  # Everything else survives: dt and distance depend on consecutive samples,
  # so dropping rows instead would corrupt them.
  expect_equal(cut$records$dt, full$records$dt)
  expect_equal(cut$records$distance, full$records$distance)
})

test_that("an all-NA metric yields NA, not NaN or -Inf", {
  p <- prepared(list(list(athlete = 5L, metric = "heart_rate")))
  s <- treinus_athlete_summary(p$records, p$settings)
  row <- s[s$id_athlete == 5, ]

  # mean() gives NaN, max() gives -Inf with a warning. Tables should print NA.
  expect_true(is.na(row$fc_avg))
  expect_false(is.nan(row$fc_avg))
  expect_true(is.na(row$fc_max))
  expect_false(is.infinite(row$fc_max))
  expect_true(is.na(row$fc_p99))
  # Distance is untouched, so the rest of the row still computes.
  expect_gt(row$km, 0)
})

test_that("boat speed is one trace per crew and ignores lone recorders", {
  p <- prepared()
  bs <- treinus_boat_speed(p$records, p$settings)

  expect_setequal(unique(bs$crew), unique(p$records$crew))
  expect_true(all(bs$n_crew >= 2))
  expect_false(any(is.na(bs$kmh_60s)))
})

test_that("km splits are interpolated crossings, not truncated buckets", {
  p <- prepared()
  sp <- treinus_km_splits(p$records)

  expect_true(all(sp$split_min > 0))
  # A bucketed last kilometre shows up as an implausibly fast partial split.
  expect_true(all(sp$split_min > 2))
})

test_that("km_crossings copes with a stopped odometer and a short trace", {
  ts <- as.POSIXct("2026-09-26 07:00:00", tz = "UTC") + seq(0, 600, by = 10)
  # Flat stretch: the boat is stopped and the odometer repeats.
  d <- c(seq(0, 1000, length.out = 31), rep(1000, 30))
  out <- km_crossings(d, ts)
  expect_equal(nrow(out), 1)
  expect_equal(out$km_mark, 1)

  expect_equal(nrow(km_crossings(c(1, 2), ts[1:2])), 0)
  expect_equal(nrow(km_crossings(numeric(0), ts[0])), 0)
})

test_that("the cadence line excludes steerers and needs three paddlers", {
  p <- prepared()
  bs <- treinus_boat_speed(p$records, p$settings)
  pc <- treinus_pieces(bs, p$settings)
  cl <- treinus_cadence_line(p$records, pc, p$settings)

  skip_if(nrow(cl) == 0, "no blocks detected in the fixture window")

  # The line is the non-steerers' median, so a steerer can deviate from it but
  # never defines it.
  per_bin <- dplyr::summarise(cl, on_line = sum(!is_steerer),
                              .by = c("crew", "tgrid"))
  expect_true(all(per_bin$on_line >= p$settings$analysis$cadence_min_on_line))
  expect_true(all(cl$bloco %in% pc$piece))
})

test_that("exclusion cost reports when a crew loses its stroke line", {
  p <- prepared()

  cost <- treinus_exclusion_cost(p$records, 37L, "cadence", p$settings)
  expect_equal(cost$metric, "cadence")
  expect_gt(cost$valid_minutes, 0)
  expect_type(cost$breaks_line, "logical")
})

test_that("pieces use a per-crew relative threshold", {
  p <- prepared()
  bs <- treinus_boat_speed(p$records, p$settings)
  pc <- treinus_pieces(bs, p$settings)

  skip_if(nrow(pc) == 0, "no blocks detected in the fixture window")
  expect_true(all(pc$dur_min >= p$settings$analysis$piece_min_s / 60))
  expect_true(all(pc$end >= pc$start))
})
