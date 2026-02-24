#!/usr/bin/env Rscript
# Advanced Example: Analyzing Treinus workout data with data.table

library(rtreinus)
library(data.table)

# ── Authentication ───────────────────────────────────────────────────────────

session <- treinus_auth()

# ── Retrieve Workout Data ────────────────────────────────────────────────────

# Get recent workouts (last 6 months)
start_date <- Sys.Date() - 180
workouts_raw <- treinus_get_workouts(
  session,
  start_date = start_date,
  end_date = Sys.Date()
)

# Convert to data.table for efficient operations
dt <- as.data.table(workouts_raw)

# ── Data Cleaning and Transformation ─────────────────────────────────────────

# Add computed columns
dt[, `:=`(
  date = as.IDate(date),
  distance_km = distance / 1000,
  duration_hours = duration_minutes / 60,
  week = week(date),
  month = month(date),
  year = year(date)
)]

# Calculate pace (min/km)
dt[distance_km > 0, pace_min_km := duration_minutes / distance_km]

# ── Summary Statistics by Sport Type ─────────────────────────────────────────

summary_by_sport <- dt[, .(
  total_workouts = .N,
  total_distance = sum(distance_km, na.rm = TRUE),
  total_duration = sum(duration_hours, na.rm = TRUE),
  avg_pace = mean(pace_min_km, na.rm = TRUE),
  max_distance = max(distance_km, na.rm = TRUE)
), by = sport_type][order(-total_workouts)]

print("Summary by Sport Type:")
print(summary_by_sport)

# ── Weekly Training Volume ───────────────────────────────────────────────────

weekly_volume <- dt[, .(
  workouts = .N,
  distance = sum(distance_km, na.rm = TRUE),
  duration = sum(duration_hours, na.rm = TRUE)
), by = .(year, week)][order(year, week)]

print("\nWeekly Training Volume (last 8 weeks):")
print(tail(weekly_volume, 8))

# ── Monthly Trends ───────────────────────────────────────────────────────────

monthly_trends <- dt[, .(
  workouts = .N,
  distance = sum(distance_km, na.rm = TRUE),
  duration = sum(duration_hours, na.rm = TRUE),
  avg_pace = mean(pace_min_km, na.rm = TRUE)
), by = .(year, month)][order(year, month)]

print("\nMonthly Trends:")
print(monthly_trends)

# ── Performance Analysis ─────────────────────────────────────────────────────

# Best performances by distance range
distance_ranges <- dt[distance_km > 0, .(
  distance_range = cut(
    distance_km,
    breaks = c(0, 5, 10, 15, 20, Inf),
    labels = c("0-5km", "5-10km", "10-15km", "15-20km", "20km+")
  ),
  pace_min_km,
  duration_hours,
  date
)]

best_paces <- distance_ranges[!is.na(pace_min_km), .SD[which.min(pace_min_km)], 
                               by = distance_range]

print("\nBest Pace by Distance Range:")
print(best_paces)

# ── Training Consistency ─────────────────────────────────────────────────────

# Calculate days between workouts
setorder(dt, date)
dt[, days_since_last := as.numeric(date - shift(date))]

consistency_metrics <- dt[, .(
  avg_days_between = mean(days_since_last, na.rm = TRUE),
  median_days_between = median(days_since_last, na.rm = TRUE),
  max_gap = max(days_since_last, na.rm = TRUE)
)]

print("\nTraining Consistency:")
print(consistency_metrics)

# ── Export Results ───────────────────────────────────────────────────────────

# Save processed data
fwrite(dt, "treinus_workouts_processed.csv")
fwrite(summary_by_sport, "treinus_summary_by_sport.csv")
fwrite(weekly_volume, "treinus_weekly_volume.csv")

cat("\n✓ Analysis complete! Files saved:\n")
cat("  - treinus_workouts_processed.csv\n")
cat("  - treinus_summary_by_sport.csv\n")
cat("  - treinus_weekly_volume.csv\n")
