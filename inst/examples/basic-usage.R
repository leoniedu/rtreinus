#!/usr/bin/env Rscript
# Example: Using rtreinus to access Treinus workout data

library(rtreinus)
library(tidyverse)

# ── Setup ────────────────────────────────────────────────────────────────────

# First time setup (interactive)
# treinus_set_credentials()

# Check configuration
treinus_config()

# ── Authentication ───────────────────────────────────────────────────────────

# Authenticate with Treinus
session <- treinus_auth()

# Verify session is valid
stopifnot(treinus_session_valid(session))

# ── Get Dashboard Data ───────────────────────────────────────────────────────

# Retrieve dashboard summary
dashboard <- treinus_get_dashboard(session)
print(dashboard)

# ── Get Workout Data ─────────────────────────────────────────────────────────

# Get all workouts
all_workouts <- treinus_get_workouts(session)

# Get workouts for January 2024
jan_workouts <- treinus_get_workouts(
  session,
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)

# ── Analyze Workout Data ─────────────────────────────────────────────────────

# Example analysis with tidyverse
# (This is a placeholder - actual structure depends on parsed data)

# workout_summary <- all_workouts |>
#   mutate(
#     date = as.Date(date),
#     distance_km = distance / 1000,
#     pace = duration_minutes / distance_km
#   ) |>
#   group_by(week = floor_date(date, "week")) |>
#   summarise(
#     workouts = n(),
#     total_distance = sum(distance_km),
#     avg_pace = mean(pace, na.rm = TRUE)
#   )

# ── Custom API Request ───────────────────────────────────────────────────────

# Make a custom request to performance page
perf_resp <- treinus_request(
  session,
  endpoint = "/Athlete/Performance/Index",
  method = "GET"
)

# Parse the response
perf_page <- httr2::resp_body_html(perf_resp)

# Extract specific data
# perf_data <- perf_page |>
#   rvest::html_element("#performance-table") |>
#   rvest::html_table()

# ── Export Data ──────────────────────────────────────────────────────────────

# Save workout data
# write_csv(all_workouts, "treinus_workouts.csv")

# Or save as RDS for later use
# saveRDS(all_workouts, "treinus_workouts.rds")

# ── Session Management ───────────────────────────────────────────────────────

# Sessions can be reused for multiple requests
# No need to re-authenticate unless session expires

# Check if session is still valid before making more requests
if (!treinus_session_valid(session)) {
  message("Session expired, re-authenticating...")
  session <- treinus_auth()
}
