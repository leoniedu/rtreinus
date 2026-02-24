#!/usr/bin/env Rscript

# Test script to verify the updated init_db function works correctly

library(RSQLite)

# Source the api.R file to get access to the functions
source("R/api.R")

# Test 1: Create a fresh database and check if all columns exist
cat("Test 1: Creating fresh database and checking columns...\n")

# Create a temporary database for testing
temp_db_path <- tempfile(fileext = ".db")
# Temporarily override the treinus_db_path function to use our test database
original_path_func <- treinus_db_path
assignInNamespace("treinus_db_path", function() temp_db_path, ns = "rtreinus")

# Call init_db to create the table
con <- init_db()

# Get the table info to verify all columns exist
table_info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(exercises)")
existing_columns <- table_info$name

# Define the expected columns
expected_columns <- c(
  "id_exercise", "id_team", "id_athlete", "genre_name", "start", "distance", 
  "total_elapsed_time", "total_timer_time", "calories", "max_speed", 
  "avg_speed", "max_heartrate", "avg_heartrate", "max_cadence", "avg_cadence", 
  "max_altitude", "min_altitude", "avg_altitude", "total_ascent", "total_descent", 
  "created_at", "date", "id_macro", "id_micro", "genre", "genre_type", 
  "total_time", "total_time_unit", "total_elapsed_time_unit", "total_movement_time", 
  "total_movement_time_unit", "fc_avg", "fc_max", "distance_unit", "cal_unit", 
  "start_time_as_string", "total_time_as_string", "total_elapsed_time_as_string", 
  "total_movement_time_as_string", "speed", "speed_unit", "pace", "pace_unit", 
  "pace_as_string", "first_name", "last_update", "user_last_update", "id_route", 
  "device_source", "id_exercise_plan", "route_preview_records", "route_session_index", 
  "is_commented", "pace_avg", "pace_avg_unit", "pace_max", "pace_max_unit", 
  "speed_avg", "speed_avg_unit", "speed_max", "speed_max_unit", "elevation_min", 
  "elevation_min_unit", "elevation_max", "elevation_max_unit", "cadence_avg", 
  "cadence_avg_unit", "cadence_max", "cadence_max_unit", "route_preview_url", 
  "title", "training_stress_details", "is_training_metrics_user_inputted", "cal", 
  "elevation_gain", "elevation_gain_unit", "elevation_loss", "elevation_loss_unit", 
  "power_avg", "power_avg_unit", "power_max", "power_max_unit", "normalized_power", 
  "normalized_power_unit", "prediction_status", "tsi", "is", "n_pace", 
  "n_pace_as_string", "n_pace_unit"
)

missing_columns <- setdiff(expected_columns, existing_columns)
if (length(missing_columns) == 0) {
  cat("✓ All expected columns exist in the fresh database\n")
} else {
  cat("✗ Missing columns in fresh database:", paste(missing_columns, collapse = ", "), "\n")
}

# Close connection and clean up
RSQLite::dbDisconnect(con)

# Test 2: Simulate an existing database with fewer columns and verify they get added
cat("\nTest 2: Testing column addition to existing database...\n")

# Create a database with minimal columns
con_test <- RSQLite::dbConnect(RSQLite::SQLite(), temp_db_path)
RSQLite::dbExecute(con_test, "DROP TABLE IF EXISTS exercises")
RSQLite::dbExecute(con_test, "
  CREATE TABLE exercises (
    id_exercise INTEGER,
    id_team INTEGER,
    id_athlete INTEGER,
    genre_name TEXT,
    start DATETIME
  )
")
RSQLite::dbDisconnect(con_test)

# Now call init_db again - it should add missing columns
con_test2 <- init_db()
table_info_after <- RSQLite::dbGetQuery(con_test2, "PRAGMA table_info(exercises)")
existing_columns_after <- table_info_after$name

# Check if all expected columns now exist
missing_after <- setdiff(expected_columns, existing_columns_after)
if (length(missing_after) == 0) {
  cat("✓ All expected columns exist after updating existing database\n")
} else {
  cat("✗ Missing columns after update:", paste(missing_after, collapse = ", "), "\n")
}

# Count how many columns were added
added_count <- length(setdiff(existing_columns_after, c("id_exercise", "id_team", "id_athlete", "genre_name", "start")))
cat("✓ Added", added_count, "columns to existing table\n")

# Clean up
RSQLite::dbDisconnect(con_test2)
unlink(temp_db_path)

cat("\nAll tests completed!\n")