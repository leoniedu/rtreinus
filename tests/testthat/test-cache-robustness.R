test_that("safe_cache_write and safe_cache_read round-trip correctly", {
  withr::with_tempdir({
    cache_file <- file.path(getwd(), "test_cache.rds")

    safe_cache_write(list(data = 1:10), cache_file)
    result <- safe_cache_read(cache_file, exercise_id = 123)
    expect_equal(result$data, 1:10)
  })
})

test_that("safe_cache_read handles corrupt files gracefully", {
  withr::with_tempdir({
    cache_file <- file.path(getwd(), "corrupt.rds")
    writeLines("CORRUPTED", cache_file)

    expect_warning(
      result <- safe_cache_read(cache_file, exercise_id = 999),
      "corrupted"
    )
    expect_null(result)
    expect_false(file.exists(cache_file))
  })
})

test_that("safe_cache_read returns NULL for missing exercise_id in message", {
  withr::with_tempdir({
    cache_file <- file.path(getwd(), "corrupt2.rds")
    writeLines("BAD", cache_file)

    expect_warning(
      result <- safe_cache_read(cache_file),
      "corrupted"
    )
    expect_null(result)
  })
})

test_that("safe_cache_write uses temp-then-rename strategy", {
  withr::with_tempdir({
    cache_file <- file.path(getwd(), "test.rds")

    # Write succeeds
    safe_cache_write(list(x = 42), cache_file)
    expect_true(file.exists(cache_file))

    # No leftover temp files in the directory
    all_rds <- list.files(getwd(), pattern = "\\.rds$")
    expect_equal(length(all_rds), 1)
    expect_equal(all_rds, "test.rds")
  })
})

test_that("safe_cache_write creates parent directories", {
  withr::with_tempdir({
    cache_file <- file.path(getwd(), "a", "b", "c", "test.rds")

    safe_cache_write(list(x = 1), cache_file)
    expect_true(file.exists(cache_file))
    expect_equal(safe_cache_read(cache_file)$x, 1)
  })
})
