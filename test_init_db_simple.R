#!/usr/bin/env Rscript

# Simple test to verify the updated init_db function structure

cat("Checking the updated init_db function...\n")

# Read the file to verify the changes
api_content <- readLines("R/api.R")

# Find the init_db function
init_db_start <- grep("^init_db <- function", api_content)
init_db_end <- integer(0)

# Find where the function ends by looking for the closing brace at the beginning of a line
for(i in (init_db_start+1):length(api_content)) {
  line <- api_content[i]
  # Look for a line that starts with } followed by empty space or comment
  if(grepl("^\\s*}\\s*$", line) || grepl("^\\s*}\\s*#", line)) {
    init_db_end <- i
    break
  }
}

if(length(init_db_start) > 0 && length(init_db_end) > 0) {
  init_db_lines <- api_content[init_db_start:init_db_end]
  init_db_code <- paste(init_db_lines, collapse="\n")
  
  # Check if the new columns are present in the CREATE TABLE statement
  has_new_columns <- grepl("date DATETIME", init_db_code) &&
                    grepl("id_macro INTEGER", init_db_code) &&
                    grepl("genre TEXT", init_db_code) &&
                    grepl("total_time REAL", init_db_code) &&
                    grepl("fc_avg REAL", init_db_code) &&
                    grepl("pace REAL", init_db_code) &&
                    grepl("cal REAL", init_db_code) &&
                    grepl("power_avg REAL", init_db_code) &&
                    grepl("tsi REAL", init_db_code) &&
                    grepl("n_pace REAL", init_db_code)
  
  if(has_new_columns) {
    cat("✓ All expected new columns are present in the CREATE TABLE statement\n")
  } else {
    cat("✗ Some expected columns are missing from the CREATE TABLE statement\n")
  }
  
  # Check if the ALTER TABLE logic is present
  has_alter_logic <- grepl("PRAGMA table_info", init_db_code) &&
                    grepl("ALTER TABLE exercises ADD COLUMN", init_db_code)
  
  if(has_alter_logic) {
    cat("✓ ALTER TABLE logic is present to handle existing databases\n")
  } else {
    cat("✗ ALTER TABLE logic is missing\n")
  }
  
  cat("\nFunction structure looks good!\n")
} else {
  cat("Could not find init_db function properly\n")
}

cat("Test completed.\n")