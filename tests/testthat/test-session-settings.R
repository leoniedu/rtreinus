fixture <- function() readRDS(test_path("fixtures", "session_15min.rds"))

test_that("settings round-trip through YAML unchanged", {
  skip_if_not_installed("yaml")

  s <- draft_quietly(fixture(), source = "fixture")
  path <- withr::local_tempfile(fileext = ".yml")
  treinus_write_settings(s, path)

  back <- treinus_read_settings(path)
  # The source path is deliberately not round-tripped verbatim: reading
  # resolves it against the settings file's own directory.
  back$session$source <- s$session$source
  expect_equal(back, treinus_validate_settings(s))
})

test_that("a relative source resolves against the settings file", {
  skip_if_not_installed("yaml")

  dir <- withr::local_tempdir()
  s <- draft_quietly(fixture(), source = "data/records.rds")
  path <- file.path(dir, "treino.yml")
  treinus_write_settings(s, path)

  # Quarto renders from the document's directory, so a source resolved against
  # the current working directory would break depending on where it is run.
  expect_equal(treinus_read_settings(path)$session$source,
               file.path(normalizePath(dir), "data/records.rds"))
})

test_that("an absolute source is left alone", {
  skip_if_not_installed("yaml")

  s <- draft_quietly(fixture(), source = "/tmp/records.rds")
  path <- withr::local_tempfile(fileext = ".yml")
  treinus_write_settings(s, path)

  expect_equal(treinus_read_settings(path)$session$source, "/tmp/records.rds")
})

test_that("a crew of one survives YAML's collapsing of length-one vectors", {
  skip_if_not_installed("yaml")

  s <- draft_quietly(fixture(), source = "fixture")
  s$crews$members[["Canoa 1"]] <- 5L
  s$crews$include <- "Canoa 1"
  s$crews$steerer <- list("Canoa 1" = 5L)

  path <- withr::local_tempfile(fileext = ".yml")
  treinus_write_settings(s, path)
  back <- treinus_read_settings(path)

  expect_type(back$crews$members[["Canoa 1"]], "integer")
  expect_equal(back$crews$members[["Canoa 1"]], 5L)
})

test_that("the session date is written as a string, not a day number", {
  skip_if_not_installed("yaml")

  s <- draft_quietly(fixture(), source = "fixture")
  s$session$date <- as.Date("2026-09-26")
  path <- withr::local_tempfile(fileext = ".yml")
  treinus_write_settings(s, path)

  # write_yaml() renders a Date as 20722.0 unless it is formatted first.
  expect_match(paste(readLines(path), collapse = "\n"), "2026-09-26")
  expect_equal(treinus_read_settings(path)$session$date, "2026-09-26")
})

test_that("an unsupported schema fails loudly", {
  s <- draft_quietly(fixture(), source = "fixture")
  s$schema <- 99L
  expect_error(treinus_validate_settings(s), "schema")
})

test_that("only heart rate and cadence may be excluded", {
  s <- draft_quietly(fixture(), source = "fixture")

  s$exclude <- list(list(athlete = 5L, metric = "heart_rate"))
  expect_no_error(treinus_validate_settings(s))

  # Excluding speed or distance is not a coherent request: the measures are
  # built from consecutive samples, so removing them yields Inf, not NA.
  s$exclude <- list(list(athlete = 5L, metric = "speed"))
  expect_error(treinus_validate_settings(s), "speed")
})

test_that("a crew listed for inclusion must have members", {
  s <- draft_quietly(fixture(), source = "fixture")
  s$crews$include <- c(s$crews$include, "Canoa 9")
  expect_error(treinus_validate_settings(s), "Canoa 9")
})

test_that("draft settings propose the detected crews and steerers", {
  s <- draft_quietly(fixture(), source = "fixture")

  expect_equal(s$schema, 1L)
  expect_setequal(s$clock$local, c(36L, 50L))
  expect_length(s$crews$include, 2)
  expect_length(unlist(s$crews$steerer), 2)
  expect_true(8L %in% unlist(s$crews$steerer))
})
