test_that("treinus_clean_old_cache removes old files only", {
  withr::with_tempdir({
    cache_dir <- file.path(getwd(), "test_cache")
    dir.create(cache_dir)

    local_mocked_bindings(
      treinus_cache_dir = function() cache_dir
    )

    # Create two files
    old_file <- file.path(cache_dir, "1_50_100.rds")
    new_file <- file.path(cache_dir, "1_50_101.rds")
    saveRDS(list(data = 1), old_file)
    saveRDS(list(data = 2), new_file)

    # Make old_file 40 days old
    old_time <- Sys.time() - (40 * 86400)
    Sys.setFileTime(old_file, old_time)

    result <- treinus_clean_old_cache(max_age_days = 30)
    expect_equal(result, 1L)
    expect_false(file.exists(old_file))
    expect_true(file.exists(new_file))
  })
})

test_that("treinus_clean_old_cache handles empty cache", {
  withr::with_tempdir({
    cache_dir <- file.path(getwd(), "empty_cache")
    dir.create(cache_dir)

    local_mocked_bindings(
      treinus_cache_dir = function() cache_dir
    )

    expect_message(
      result <- treinus_clean_old_cache(),
      "No cached files found"
    )
    expect_equal(result, 0L)
  })
})

test_that("treinus_clean_old_cache handles nonexistent directory", {
  local_mocked_bindings(
    treinus_cache_dir = function() "/nonexistent/path/12345"
  )

  expect_message(
    result <- treinus_clean_old_cache(),
    "does not exist"
  )
  expect_equal(result, 0L)
})

test_that("treinus_clean_old_cache handles no old files", {
  withr::with_tempdir({
    cache_dir <- file.path(getwd(), "fresh_cache")
    dir.create(cache_dir)

    local_mocked_bindings(
      treinus_cache_dir = function() cache_dir
    )

    # Create a fresh file
    saveRDS(list(data = 1), file.path(cache_dir, "1_50_100.rds"))

    expect_message(
      result <- treinus_clean_old_cache(max_age_days = 30),
      "No cached files older than"
    )
    expect_equal(result, 0L)
  })
})
