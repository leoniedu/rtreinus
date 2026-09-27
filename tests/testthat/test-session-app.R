# The app carries the only logic in the package that is not a plain function,
# so the parts that can break silently are tested here as plain functions.

test_that("an empty checkbox selection produces no exclusions, not an error", {
  # Shiny sends NULL, not an empty vector, when the last box is unticked, and
  # ignoreNULL = FALSE makes that case reachable. strsplit() errors on NULL.
  to_exclusions <- function(x) {
    chosen <- as.character(x %||% character())
    lapply(strsplit(chosen, "|", fixed = TRUE),
           \(p) list(athlete = as.integer(p[1]), metric = p[2]))
  }

  expect_equal(to_exclusions(NULL), list())
  expect_equal(to_exclusions(character()), list())
  expect_equal(
    to_exclusions(c("36|heart_rate", "50|cadence")),
    list(list(athlete = 36L, metric = "heart_rate"),
         list(athlete = 50L, metric = "cadence"))
  )
})

test_that("exclusions built from checkbox keys validate", {
  skip_if_not_installed("yaml")

  s <- draft_quietly(readRDS(test_path("fixtures", "session_15min.rds")))
  s$exclude <- lapply(strsplit(c("5|heart_rate"), "|", fixed = TRUE),
                      \(p) list(athlete = as.integer(p[1]), metric = p[2]))

  out <- treinus_validate_settings(s)
  expect_equal(nrow(out$exclude), 1)
  expect_equal(out$exclude$athlete, 5L)
})

test_that("clearing every crew leaves an empty vector, not a deleted key", {
  # Assigning NULL into a list removes the element; downstream code then sees a
  # settings object with no include key at all.
  s <- draft_quietly(readRDS(test_path("fixtures", "session_15min.rds")))
  s$crews$include <- as.character(NULL %||% character())

  expect_true("include" %in% names(s$crews))
  expect_length(s$crews$include, 0)
  expect_error(treinus_prepare_records(
    readRDS(test_path("fixtures", "session_15min.rds")), s), "crew")
})

test_that("the app source parses", {
  app <- system.file("shiny", "session", "app.R", package = "rtreinus")
  skip_if(!nzchar(app), "package not installed")
  expect_no_error(parse(app))
})


test_that("the line-break warning accounts for all exclusions together", {
  path <- test_path("..", "..", "vignettes", "data", "treino_20260926.rds")
  skip_if_not(file.exists(path), "reference session not available")

  raw <- readRDS(path)
  s <- draft_quietly(raw, tryCatch(treinus_get_exercises_db(),
                                            error = function(e) NULL))
  s$crews$include <- c("Canoa 1", "Canoa 2")
  s$crews$steerer <- list(`Canoa 1` = 8L, `Canoa 2` = 36L)
  rec <- treinus_prepare_records(raw, s)

  still_has_line <- function(gone) {
    rec |>
      dplyr::filter(!is_steerer, !id_athlete %in% gone) |>
      dplyr::summarise(n = dplyr::n_distinct(id_athlete), .by = "crew") |>
      dplyr::filter(n < s$analysis$cadence_min_on_line) |>
      nrow() == 0
  }

  # Checked one at a time, each of these looks harmless. Together they leave
  # Canoa 1 with two paddlers on the line, below the minimum of three, and the
  # crew's cadence analysis disappears.
  expect_true(still_has_line(37L))
  expect_true(still_has_line(53L))
  expect_false(still_has_line(c(37L, 53L)))
})
