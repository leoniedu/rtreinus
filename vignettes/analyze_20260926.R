## Training of 2026-09-26 -- record-level analysis.
##
## Sourced both standalone and from treino_20260926.qmd; the report sets
## IN_REPORT so that the figures at the bottom are not written to disk twice.
##
## The records come from treinus_get_exercise_analysis() over every exercise
## of the day with a speed trace and more than 5 minutes of recording; see
## the fetch block at the bottom of this file.

## The report sets IN_REPORT before sourcing; standalone runs default it off.
if (!exists("IN_REPORT")) IN_REPORT <- FALSE
if (!IN_REPORT) pkgload::load_all(quiet = TRUE)
library(dplyr)
library(sf)

DAY <- as.Date("2026-09-26")
RDS <- if (file.exists("data/treino_20260926.rds")) {
  "data/treino_20260926.rds"           # cwd is vignettes/ (Quarto)
} else {
  "vignettes/data/treino_20260926.rds" # cwd is the package root
}

## Devices differ in what they store: most write UTC, athletes 36 and 50 write
## local time already. Shift the UTC ones so every clock is America/Bahia (-03).
LOCAL_CLOCK <- c(36, 50)

## Athletes 23 and 29 trained elsewhere on this date (land sessions); they are
## not part of the water training.
NOT_IN_TRAINING <- c(23, 29)

## Who steered each canoe on this date (informed by the crew, not derivable
## from the data). A steerer does not follow the crew's stroke, so their
## cadence trace is not comparable with their crewmates'.
LEME <- c(36, 8, 60)   # Atleta 1, Atleta 2, Atleta 3

## A sample separated from the previous one by more than this is a recording
## gap (auto-pause), not elapsed training time.
MAX_SAMPLE_GAP_S <- 30
MOVING_MS <- 0.5      # speed above which the boat counts as under way
NEAR_M <- 25          # crew co-location radius (bow-stern + GPS error)
NEAR_PCT <- 90        # share of the session a pair must stay within NEAR_M

records <- readRDS(RDS) |>
  filter(!id_athlete %in% NOT_IN_TRAINING) |>
  mutate(
    ts = as.POSIXct(timestamp, tz = "UTC"),
    ts = if_else(id_athlete %in% LOCAL_CLOCK, ts, ts - 3 * 3600),
    ts = lubridate::force_tz(ts, "America/Bahia")
  ) |>
  arrange(id_athlete, id_exercise, ts) |>
  ## dt is the honest inter-sample interval, capped so that auto-pause gaps
  ## are not charged to the session.
  group_by(id_athlete, id_exercise) |>
  mutate(dt = pmin(c(0, diff(as.numeric(ts))), MAX_SAMPLE_GAP_S)) |>
  ungroup()

## ---- session summary -------------------------------------------------------

zone <- function(hr) {
  cut(hr, c(-Inf, 120, 140, 160, Inf),
      labels = c("Z1 <120", "Z2 120-140", "Z3 140-160", "Z4 160+"))
}

summary_athlete <- records |>
  group_by(id_athlete, fullname_athlete, id_exercise) |>
  summarise(
    start = min(ts), end = max(ts),
    elapsed_min = as.numeric(difftime(max(ts), min(ts), units = "mins")),
    km = max(distance, na.rm = TRUE) / 1000,
    moving_min = sum(dt[!is.na(speed) & speed > MOVING_MS]) / 60,
    kmh_avg = km / (elapsed_min / 60),
    kmh_moving = km / (moving_min / 60),
    kmh_max30 = max(slider::slide_index_dbl(
      speed, ts, mean, na.rm = TRUE,
      .before = lubridate::dseconds(30)), na.rm = TRUE) * 3.6,
    fc_avg = mean(heart_rate, na.rm = TRUE),
    ## p99 rather than max: a single-sample spike is not a physiological peak.
    fc_p99 = quantile(heart_rate, 0.99, na.rm = TRUE),
    fc_max = max(heart_rate, na.rm = TRUE),
    spm_med = median(cadence[cadence > 20], na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(km))

## ---- heart-rate zones ------------------------------------------------------

zones_athlete <- records |>
  filter(!is.na(heart_rate)) |>
  group_by(id_athlete, fullname_athlete, z = zone(heart_rate)) |>
  summarise(min = sum(dt) / 60, .groups = "drop") |>
  group_by(id_athlete) |>
  mutate(pct = 100 * min / sum(min)) |>
  ungroup()

## ---- km splits -------------------------------------------------------------

## Time at each whole-kilometre crossing, interpolated from the cumulative
## distance trace. Bucketing by floor(distance/1000) charges a partial last
## bucket and swallows recording gaps, so it is not used.

km_crossings <- function(distance, ts) {
  ok <- !is.na(distance) & !is.na(ts)
  distance <- distance[ok]; ts <- as.numeric(ts)[ok]
  ## while the boat is stopped the odometer repeats; the first time a distance
  ## is reached is the crossing, so drop the later duplicates
  keep <- !duplicated(distance)
  distance <- distance[keep]; ts <- ts[keep]
  marks <- seq_len(floor(max(distance) / 1000))
  if (!length(marks)) return(tibble::tibble(km_mark = integer(), t = numeric()))
  tibble::tibble(km_mark = marks,
                 t = stats::approx(distance, ts, xout = marks * 1000)$y)
}

splits <- records |>
  group_by(id_athlete, fullname_athlete, id_exercise) |>
  reframe(km_crossings(distance, ts)) |>
  group_by(id_athlete, id_exercise) |>
  mutate(split_min = (t - lag(t, default = NA)) / 60) |>
  ungroup() |>
  filter(!is.na(split_min))

## ---- crew detection: who shared a canoe ------------------------------------
## Positions on a common 10-second grid; athletes who stayed within NEAR_M of
## each other for most of the session were in the same boat.

grid <- records |>
  filter(!is.na(position_long)) |>
  mutate(
    lat = position_lat * 180 / 2^31,
    lon = position_long * 180 / 2^31,
    tgrid = as.POSIXct(round(as.numeric(ts) / 10) * 10, origin = "1970-01-01", tz = "America/Bahia")
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(31984) |>
  mutate(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  group_by(id_athlete, fullname_athlete, tgrid) |>
  summarise(x = mean(x), y = mean(y), .groups = "drop")

pairs_dist <- grid |>
  inner_join(grid, by = "tgrid", relationship = "many-to-many") |>
  filter(id_athlete.x < id_athlete.y) |>
  mutate(m = sqrt((x.x - x.y)^2 + (y.y - y.x)^2)) |>
  group_by(id_athlete.x, fullname_athlete.x, id_athlete.y, fullname_athlete.y) |>
  summarise(n = n(), median_m = median(m), pct_near = 100 * mean(m < NEAR_M), .groups = "drop") |>
  filter(n >= 60) |>
  arrange(median_m)

## ---- boats ----------------------------------------------------------------
## Connected components of the "within NEAR_M for >NEAR_PCT% of the session"
## graph. The radius has to cover a bow-to-stern pair in a ~13 m hull plus a
## few metres of per-device GPS error; the components are identical for any
## radius from 15 to 30 m, so 25 m sits in the middle of the stable region.

edges <- pairs_dist |> filter(pct_near > NEAR_PCT)
boat_ids <- sort(unique(c(edges$id_athlete.x, edges$id_athlete.y)))
comp <- setNames(seq_along(boat_ids), boat_ids)
repeat {
  old <- comp
  for (i in seq_len(nrow(edges))) {
    k <- min(comp[as.character(edges$id_athlete.x[i])],
             comp[as.character(edges$id_athlete.y[i])])
    comp[as.character(edges$id_athlete.x[i])] <- k
    comp[as.character(edges$id_athlete.y[i])] <- k
  }
  if (identical(old, comp)) break
}
boats_all <- tibble::tibble(
  id_athlete = as.integer(names(comp)),
  boat = paste("Canoa", as.integer(factor(comp)))
)

## Only Canoa 1 and Canoa 2 are analysed. Canoa 3 is detected and reported as
## a crew, but left out of everything downstream.
ANALYZED <- c("Canoa 1", "Canoa 2")

boats <- boats_all |> filter(boat %in% ANALYZED)
records <- semi_join(records, boats, by = "id_athlete")

## ---- boat speed over time --------------------------------------------------
## One hull = one speed: average each crew member over a 10-second grid, take
## the median across the crew, then a 60-second centred mean to kill GPS
## jitter. Bins with only one member recording are dropped -- before the rest
## of the crew started their watches there is no way to tell boat from beach.

boat_speed <- records |>
  inner_join(boats, by = "id_athlete") |>
  filter(!is.na(speed)) |>
  mutate(tgrid = as.POSIXct(round(as.numeric(ts) / 10) * 10,
                            origin = "1970-01-01", tz = "America/Bahia")) |>
  ## Average within each athlete first: sample rates differ by 6x across
  ## devices, so a plain median over raw rows is just the fastest logger.
  summarise(kmh = mean(speed) * 3.6, .by = c(boat, id_athlete, tgrid)) |>
  group_by(boat, tgrid) |>
  summarise(kmh = median(kmh), n_crew = n(), .groups = "drop") |>
  filter(n_crew >= 2) |>
  group_by(boat) |>
  arrange(tgrid, .by_group = TRUE) |>
  mutate(kmh_60s = slider::slide_index_dbl(
    kmh, tgrid, mean, na.rm = TRUE,
    .before = lubridate::dseconds(30), .after = lubridate::dseconds(30))) |>
  ungroup()

## ---- boat km splits --------------------------------------------------------

## Crew members' distance traces drift apart by up to 1.5 km over a session,
## so their km marks fall at different times and must not be pooled. Use one
## reference device per boat: the member with the longest continuous trace.

boat_reference <- summary_athlete |>
  inner_join(boats, by = "id_athlete") |>
  group_by(boat) |>
  slice_max(km, n = 1) |>
  ungroup() |>
  select(boat, id_athlete, fullname_athlete)

boat_splits <- splits |>
  inner_join(boat_reference, by = c("id_athlete", "fullname_athlete")) |>
  select(boat, reference = fullname_athlete, km_mark, split_min)

## ---- fastest continuous 1000 m per boat ------------------------------------

records_sf <- records |>
  inner_join(boats, by = "id_athlete") |>
  filter(!is.na(position_long)) |>
  mutate(lat = position_lat * 180 / 2^31,
         lon = position_long * 180 / 2^31,
         timestamp = ts) |>
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) |>
  st_transform(31984)

fast1000 <- fastest_straight_distance(
  sf_points = records_sf, athlete_col = "id_athlete",
  time_col = "timestamp", distance_m = 1000
) |>
  left_join(boats, by = "id_athlete") |>
  left_join(distinct(st_drop_geometry(records_sf), id_athlete, fullname_athlete),
            by = "id_athlete")

## ---- workout structure -----------------------------------------------------
## A "piece" is a stretch where the boat held at least 85% of its own cruising
## speed for 60 s or more. An absolute threshold (e.g. 8 km/h) sits right on
## the cruising speed of the slowest boat, so ordinary speed variation crosses
## it repeatedly and splits steady paddling into spurious fragments; the
## threshold has to be relative to each hull.

PIECE_FRAC <- 0.85
PIECE_MIN_S <- 60

boat_pace <- boat_speed |>
  filter(kmh_60s > 3) |>
  summarise(cruise = median(kmh_60s), .by = boat)

pieces <- boat_speed |>
  inner_join(boat_pace, by = "boat") |>
  group_by(boat) |>
  arrange(tgrid, .by_group = TRUE) |>
  mutate(
    on = kmh_60s >= PIECE_FRAC * cruise,
    ## a recording gap breaks the run even if both sides of it are "on"
    brk = on != lag(on, default = first(on)) |
      c(FALSE, diff(as.numeric(tgrid)) > PIECE_MIN_S),
    run = cumsum(brk)
  ) |>
  filter(on) |>
  group_by(boat, run) |>
  summarise(start = min(tgrid), end = max(tgrid),
            dur_min = as.numeric(difftime(max(tgrid), min(tgrid), units = "mins")),
            kmh = mean(kmh_60s), kmh_max = max(kmh_60s), .groups = "drop") |>
  filter(dur_min >= PIECE_MIN_S / 60) |>
  group_by(boat) |>
  mutate(piece = row_number()) |>
  ungroup()

## ---- where the time went ---------------------------------------------------
## Moving, stopped and gap time to a common reference distance, so the boats
## are compared over the same course rather than over whatever each crew
## happened to record.

to_14km <- records |>
  inner_join(boats, by = "id_athlete") |>
  filter(!is.na(distance), distance <= 14000) |>
  group_by(boat, id_athlete, fullname_athlete) |>
  filter(max(distance) >= 13950) |>
  summarise(
    elapsed_min = as.numeric(difftime(max(ts), min(ts), units = "mins")),
    moving_min = sum(dt[!is.na(speed) & speed > MOVING_MS]) / 60,
    stopped_min = sum(dt[!is.na(speed) & speed <= MOVING_MS]) / 60,
    gap_min = as.numeric(difftime(max(ts), min(ts), units = "mins")) - sum(dt) / 60,
    kmh_moving = 14 / (moving_min / 60),
    .groups = "drop"
  ) |>
  arrange(boat, desc(kmh_moving))

## ---- cadence comparability -------------------------------------------------
## Stroke rate is identical for everyone in one hull who is ON the stroke, so
## among those paddlers a device whose median over a steady piece disagrees
## with its crewmates is missing strokes. Steerers are excluded from the
## baseline: they do not follow the stroke, so a low or ragged trace is
## expected of them and says nothing about the device.
## Compared over a window all three boats spent paddling steadily.

STEADY <- as.POSIXct(c("2026-09-26 07:55:00", "2026-09-26 08:15:00"),
                     tz = "America/Bahia")

cadence_check <- records |>
  inner_join(boats, by = "id_athlete") |>
  filter(ts >= STEADY[1], ts <= STEADY[2], cadence > 20) |>
  group_by(boat, id_athlete, fullname_athlete) |>
  summarise(spm_med = median(cadence), q10 = quantile(cadence, 0.1),
            q90 = quantile(cadence, 0.9), .groups = "drop") |>
  mutate(leme = id_athlete %in% LEME) |>
  group_by(boat) |>
  ## the baseline is the stroke itself, so steerers do not set it
  mutate(vs_crew = spm_med - median(spm_med[!leme])) |>
  ungroup() |>
  arrange(boat, leme, desc(spm_med))

## ---- cadence relative to the stroke line -----------------------------------
## The "line" is the crew's own stroke at each moment: the median cadence of
## the paddlers who are on it (steerers excluded, since they do not follow it).
## A paddler's deviation from that line says whether they sat on the stroke or
## drifted off it, and when.

CAD_BIN_S <- 30

cadence_line <- records |>
  inner_join(boats, by = "id_athlete") |>
  filter(cadence > 20, !is.na(cadence)) |>
  mutate(
    leme = id_athlete %in% LEME,
    tgrid = as.POSIXct(round(as.numeric(ts) / CAD_BIN_S) * CAD_BIN_S,
                       origin = "1970-01-01", tz = "America/Bahia")
  ) |>
  summarise(spm = median(cadence),
            .by = c(boat, id_athlete, fullname_athlete, leme, tgrid)) |>
  ## A stroke line only exists while the crew is rowing: during warm-up,
  ## turns and stops there is no common rhythm to deviate from, and the
  ## deviations there run to +60 spm without meaning anything.
  ## Which block each bin belongs to; NA means between blocks, and those
  ## bins are dropped. Carrying the id keeps the lines from being drawn
  ## across the gaps between blocks.
  mutate(bloco = purrr::map2_int(boat, tgrid, \(b, t) {
    pc <- pieces[pieces$boat == b, ]
    hit <- which(t >= pc$start & t <= pc$end)
    if (length(hit)) pc$piece[hit[1]] else NA_integer_
  })) |>
  filter(!is.na(bloco)) |>
  group_by(boat, tgrid) |>
  ## With only two paddlers the median is their mean, so each is forced to
  ## mirror the other; three is the smallest honest line.
  filter(sum(!leme) >= 3) |>
  mutate(linha = median(spm[!leme])) |>
  ungroup() |>
  mutate(desvio = spm - linha)

## Smoothed over two minutes: a single 30-s bin swings several spm on stroke
## detection alone, and the question is whether someone sits off the line, not
## what happened in one window.
CAD_SMOOTH_S <- 120

cadence_line <- cadence_line |>
  group_by(id_athlete) |>
  arrange(tgrid, .by_group = TRUE) |>
  mutate(desvio_suave = slider::slide_index_dbl(
    desvio, tgrid, mean, na.rm = TRUE,
    .before = lubridate::dseconds(CAD_SMOOTH_S / 2),
    .after = lubridate::dseconds(CAD_SMOOTH_S / 2))) |>
  ungroup()

## The headline comparison: how far the steerer's own stroke rate sits from
## the rate the crew is pulling.
leme_vs_linha <- cadence_line |>
  filter(leme) |>
  select(boat, fullname_athlete, bloco, tgrid, spm, linha, desvio, desvio_suave)

cadence_vs_line <- cadence_line |>
  summarise(
    desvio_med = median(desvio),
    desvio_abs = median(abs(desvio)),
    ## share of the session sitting within one stroke per minute of the line
    pct_na_linha = 100 * mean(abs(desvio) <= 1),
    n = n(),
    .by = c(boat, id_athlete, fullname_athlete, leme)
  ) |>
  arrange(boat, leme, desvio_abs)

## ---- figures ---------------------------------------------------------------
## Skipped when the report is driving: it draws its own.

library(ggplot2)
BOAT_COLS <- c("Canoa 1" = "#2a78d6", "Canoa 2" = "#eb6834", "Canoa 3" = "#1baf7a")

## Within a canoe each athlete gets one of these, in a fixed order. The same
## colours are reused in the other canoe's panel: the facet says which crew,
## and every line is labelled with its own name, so identity never rests on
## colour alone.
ATHLETE_COLS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#4a3aa7")

## Short label for a line end: first name, plus a surname initial when two
## people in the same canoe share it (there are two paddlers sharing a first name in Canoa 2).
short_name <- function(fullname, boat) {
  first <- sub(" .*", "", fullname)
  dup <- ave(seq_along(first), boat, first, FUN = length) > 1
  ## second word, not the last: "Atleta 10" -> "Atleta 11 V.",
  ## which is how the report names him
  initial <- substr(sub("^\\S+\\s+", "", fullname), 1, 1)
  if_else(dup, paste0(first, " ", initial, "."), first)
}

## Assign the slots inside each boat, steerer last.
athlete_slot <- function(d) {
  d |>
    distinct(boat, id_athlete, fullname_athlete) |>
    mutate(leme = id_athlete %in% LEME) |>
    arrange(boat, leme, fullname_athlete) |>
    mutate(cor = ATHLETE_COLS[(row_number() - 1) %% length(ATHLETE_COLS) + 1],
           .by = boat)
}
FIG <- "vignettes/data"

theme_tr <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey88"),
      axis.title = element_text(colour = "grey30"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey35"),
      legend.position = "top", legend.title = element_blank()
    )
}

labels_end <- boat_speed |> group_by(boat) |> slice_max(tgrid, n = 1) |> ungroup()

p_speed <- ggplot(boat_speed, aes(tgrid, kmh_60s, colour = boat)) +
  geom_line(linewidth = 0.7) +
  geom_text(data = labels_end, aes(label = boat), hjust = -0.1, size = 3.2,
            show.legend = FALSE) +
  scale_colour_manual(values = BOAT_COLS) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min",
                   expand = expansion(mult = c(0.01, 0.12))) +
  labs(title = "Velocidade das canoas, treino de 26/09/2026",
       subtitle = "Mediana da tripulação, média móvel de 60 s · hora local (-03)",
       x = NULL, y = "km/h") +
  theme_tr()

if (!IN_REPORT) {
  ggsave(file.path(FIG, "fig_20260926_velocidade.png"), p_speed,
         width = 9, height = 4.5, dpi = 150)
}

## Canoa 1 against Canoa 2, on the common 10-second grid. Positive means
## Canoa 1 is the faster of the two at that moment.

gap_c1_c2 <- boat_speed |>
  select(boat, tgrid, kmh_60s) |>
  tidyr::pivot_wider(names_from = boat, values_from = kmh_60s) |>
  rename(c1 = `Canoa 1`, c2 = `Canoa 2`) |>
  filter(!is.na(c1), !is.na(c2)) |>
  mutate(dif = c1 - c2)

p_gap <- ggplot(gap_c1_c2, aes(tgrid, dif)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_area(data = ~ transform(.x, dif = pmax(dif, 0)),
            fill = BOAT_COLS[["Canoa 1"]], alpha = 0.25) +
  geom_area(data = ~ transform(.x, dif = pmin(dif, 0)),
            fill = BOAT_COLS[["Canoa 2"]], alpha = 0.25) +
  geom_line(linewidth = 0.5, colour = "grey25") +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min") +
  labs(title = "Canoa 1 menos Canoa 2",
       subtitle = "Acima de zero a Canoa 1 está mais rápida; abaixo, a Canoa 2",
       x = NULL, y = "diferença de velocidade (km/h)") +
  theme_tr()

if (!IN_REPORT) {
  ggsave(file.path(FIG, "fig_20260926_diferenca.png"), p_gap,
         width = 9, height = 3.5, dpi = 150)
}

hr <- records |>
  inner_join(boats, by = "id_athlete") |>
  filter(!is.na(heart_rate)) |>
  group_by(id_athlete) |>
  arrange(ts, .by_group = TRUE) |>
  mutate(fc_30s = slider::slide_index_dbl(
    heart_rate, ts, mean, na.rm = TRUE,
    .before = lubridate::dseconds(15), .after = lubridate::dseconds(15))) |>
  ungroup() |>
  inner_join(athlete_slot(records |> inner_join(boats, by = "id_athlete")),
             by = c("boat", "id_athlete", "fullname_athlete"))

hr_labels <- hr |>
  slice_max(ts, n = 1, by = c(boat, fullname_athlete)) |>
  mutate(nome = short_name(fullname_athlete, boat))

p_hr <- ggplot(hr, aes(ts, fc_30s, colour = cor, group = id_athlete)) +
  geom_line(linewidth = 0.5) +
  ggrepel::geom_text_repel(
    data = hr_labels, aes(label = nome), size = 3, hjust = 0,
    direction = "y", nudge_x = 120, segment.size = 0.2,
    segment.colour = "grey70", min.segment.length = 0, box.padding = 0.15,
    show.legend = FALSE) +
  facet_wrap(~boat, ncol = 1) +
  scale_colour_identity() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min",
                   expand = expansion(mult = c(0.01, 0.13))) +
  labs(title = "Frequência cardíaca por remador, treino de 26/09/2026",
       subtitle = "Média móvel de 30 s centrada · hora local (-03)",
       x = NULL, y = "bpm") +
  theme_tr() +
  theme(legend.position = "none")

## ---- cadence: steerer against the crew's stroke line -----------------------

leme_labels <- leme_vs_linha |>
  slice_max(tgrid, n = 1, by = boat) |>
  mutate(nome = sub(" .*", "", fullname_athlete))

p_cad <- ggplot(leme_vs_linha, aes(tgrid, desvio_suave, colour = boat)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_line(aes(group = interaction(boat, bloco)), linewidth = 0.7) +
  ggrepel::geom_text_repel(
    data = leme_labels, aes(label = nome), size = 3.2, hjust = 0,
    direction = "y", nudge_x = 120, segment.size = 0.2,
    segment.colour = "grey70", min.segment.length = 0, box.padding = 0.15,
    show.legend = FALSE) +
  scale_colour_manual(values = BOAT_COLS) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min",
                   expand = expansion(mult = c(0.01, 0.12))) +
  labs(title = "Cadência do leme em relação à linha da canoa",
       subtitle = paste("Leme menos a mediana de quem está na remada ·",
                        "média móvel de 2 min · apenas durante os blocos"),
       x = NULL, y = "desvio do leme (spm)") +
  theme_tr()

if (!IN_REPORT) {
  ggsave(file.path(FIG, "fig_20260926_cadencia.png"), p_cad,
         width = 9, height = 6, dpi = 150)
}

if (!IN_REPORT) {
  ggsave(file.path(FIG, "fig_20260926_fc.png"), p_hr,
         width = 10, height = 6, dpi = 150)
}



## ---- how the records were fetched ------------------------------------------
## Kept for reproducibility; re-running costs one API call per exercise.
##
## session <- treinus_auth()
## today <- treinus_get_exercises(athlete_id = ids, session = session) |>
##   filter(as.Date(start) == DAY, !is.na(speed), total_time > 300)
## analyses <- purrr::pmap(today, function(id_exercise, id_athlete, ...) {
##   treinus_get_exercise_analysis(exercise_id = id_exercise,
##                                 athlete_id = id_athlete, session = session)
## })
## purrr::map(analyses, ~ tibble::tibble(
##   id_athlete = .x$data$Analysis$IdAthlete,
##   id_exercise = .x$data$Analysis$IdExercise,
##   fullname_athlete = .x$data$Analysis$User$FullName,
##   treinus_extract_records(.x)
## )) |> purrr::list_rbind() |> saveRDS("vignettes/data/treino_20260926.rds")
