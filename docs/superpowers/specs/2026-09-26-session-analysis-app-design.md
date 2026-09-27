# Session analysis: detection, decisions, report

Design for turning the hardcoded single-session analysis
(`vignettes/analyze_20260926.R`) into a reusable pipeline with an interactive
front end.

Date: 2026-09-26
Status: approved design, revised after empirical review

Revision note: an initial draft claimed the three stages were independent and
that every detector was pure. Checking that against the data showed detection
is *staged* (crew detection needs the clock fix; quality needs boat speed and
pieces), and that several thresholds were overfitted to one session. Both are
corrected below. The seat-order heuristic was challenged and survived: it is
correct on all three crews, including the one it was not developed on.

## Problem

`vignettes/analyze_20260926.R` produces a correct analysis of one training
session, but four of its inputs are constants that were arrived at by hand and
are wrong for any other session:

```r
LOCAL_CLOCK     <- c(36, 50)              # whose device stores local time
NOT_IN_TRAINING <- c(23, 29)              # who was not in this training
LEME            <- c(36, 8, 60)           # who steered
ANALYZED        <- c("Canoa 1", "Canoa 2")# which crews to analyse
```

Two further judgment calls are embedded in prose rather than in code: which
heart-rate and cadence traces are too badly measured to use, and what excluding
them costs.

Each of these is a decision a human has to make, but each is also something the
data can argue about. The goal is to make the analysis *propose* every one of
them with its evidence, let the analyst accept or override, and record the
result so the report is reproducible.

## Intended use

Occasional deep dives — races and key trainings, a handful of times a year, run
locally by a single analyst. Not a per-session routine, not a shared team
service. This favours depth and legibility over speed and hardening.

The Quarto report stays the shareable artifact. The app is the cockpit that
configures it.

## Architecture

Three stages, with a settings file as the seam between the interactive part and
the reproducible part.

Detection is **staged, not parallel**. Crew detection works on a shared
10-second grid, so it cannot run until the clocks agree; and two of the quality
rules are defined against boat speed and against the detected blocks. The
honest order is:

```
records (raw)
   |
   v
treinus_fix_clock(records, local)          <- must precede everything
   |                                          `local` from treinus_detect_clock()
   v
treinus_detect_crews(records, near_m, near_pct, min_bins)
   |                                       -> pairs + components + unassigned
   v
treinus_seat_order(records, crews)         -> seat order, steerer, confidence
   |
   v
treinus_prepare_records(records, settings) -> dt, crew/role labels, NA-ing
   |
   v
treinus_boat_speed() -> treinus_pieces()   <- quality needs both
   |
   v
treinus_data_quality(records, crews, boat_speed, pieces)
   |
   +--> Shiny app: analyst accepts or overrides -> treino_YYYYMMDD.yml
                                                        |
        the remaining measure functions, then report.qmd -P settings=
```

`treinus_data_quality()` takes the measure outputs as explicit arguments
rather than pretending to be pure. It is still testable — they are just data.

### Why this split

**Detect functions never read settings; measure functions never detect.** A
detect function takes records and returns suggestions with confidence. A
measure function takes records plus decisions and returns numbers. The app is
the only place the two meet, and no function reads the YAML except
`treinus_read_settings()`. This is what makes both halves testable without
mocking a UI.

**The clock fix is split out of `prepare`.** It has to run before detection,
while everything else in `prepare` needs decisions that detection produces.
Keeping them in one function would make the pipeline impossible to order.

**Exclusion is implemented as `NA`, not as row filtering.** Excluding an
athlete's heart rate sets `heart_rate <- NA` for their rows in `prepare`.
Downstream measure functions already carry `na.rm`, so none of them needs to
know exclusions exist, and the `n` reported in each table shrinks honestly.
Filtering rows instead would corrupt `dt`, cumulative distance and the speed
traces, because those depend on consecutive samples.

**The clock fix is detected from the records alone.** Devices disagree about
whether they store UTC or local time. An earlier draft compared each athlete's
first record against `start_time_as_string` from the exercises table, but a
saved RDS has no such column, and replaying a snapshot is the main use case.
Instead, cluster the first-record timestamps: a training starts within about
half an hour for everyone, so a three-hour separation is unambiguous. The
exercises table, when present, is a cross-check rather than a requirement.

## Settings file

```yaml
schema: 1
session:
  date: "2026-09-26"        # quoted: write_yaml turns a Date into 20722.0
  source: vignettes/data/treino_20260926.rds
  tz: America/Bahia
  crs: 31984                # UTM zone; derivable from median longitude
  start: "07:00"            # analysis window, local
  end: "09:30"
exercises:
  exclude: [223]            # aborted or corrupt recordings
clock:
  local: [36, 50]           # devices that store local time; the rest are UTC
crews:
  detection: {near_m: 25, near_pct: 90, min_bins: 60}
  include: [Canoa 1, Canoa 2]
  members:
    Canoa 1: [5, 7, 8, 37, 53]
    Canoa 2: [36, 38, 48, 50, 54]
  steerer: {Canoa 1: 8, Canoa 2: 36}
exclude:                    # a record list, not an integer-keyed map
  - {athlete: 36, metric: heart_rate}
  - {athlete: 50, metric: heart_rate}
analysis:
  moving_ms: 0.5            max_sample_gap_s: 30
  piece_frac: 0.85          piece_min_s: 60
  piece_cruise_floor_kmh: 3
  cadence_bin_s: 30         cadence_smooth_s: 120
  cadence_steady: ["07:55", "08:15"]
  reference_distance_m: 14000
  hr_zones: [120, 140, 160]
```

Three shapes here were chosen against `yaml` package behaviour rather than
taste, and verified by round-tripping:

- **No integer-keyed maps.** `offset_hours: {5: -3, 36: 0}` survives a round
  trip but `write_yaml` re-emits it with quoted keys and doubles, so the file
  drifts from its hand-written form. `clock: {local: [36, 50]}` states the
  actual fact — this device stores local time — and is a plain integer vector.
  Crew maps keep string keys, which are stable.
- **`exclude` is a record list.** It reads back as a list of lists that
  `dplyr::bind_rows()` turns into a two-column tibble directly.
- **Dates are quoted strings.** `write_yaml(as.Date("2026-09-26"))` emits
  `20722.0`.

One trap to guard in the reader: `write_yaml` collapses length-one vectors to
scalars, so a crew of one becomes `Canoa 2: 36` rather than a list. It reads
back as `integer(1)`, so `%in%` still works, but any `is.list()` check breaks.
`treinus_read_settings()` normalises with `as.integer(unlist())`.

Crew membership is written out explicitly rather than left implicit in the
detection thresholds, so a report re-renders identically even if detection
later changes. `schema` makes an outdated settings file fail loudly.

The fields beyond the four original constants exist because the reference
session and the earlier Cachoeira race analysis between them hardcoded six
more: the projected CRS, the analysis window, the steady window used for the
cadence comparison, the reference distance for the pacing table, the cruise
floor in piece detection, and which exercises to drop.

## Detection

### Crews

Connected components of the graph whose edges join pairs of athletes who spent
more than `near_pct`% of the session within `near_m` of each other, computed on
a common 10-second grid, requiring at least `min_bins` shared bins.

The radius must cover a bow-to-stern pair in a ~13 m hull plus per-device GPS
error. On the reference session the components are identical for any radius
from 15 to 30 m; 25 m sits in the middle of that stable region.

### Seat order and steerer

Project each crew member onto the boat's heading, where the heading is the
centred displacement of the crew centroid on the 10-second grid, and keep only
bins where the centroid is actually moving (displacement > 15 m per bin, about
0.75 m/s). Positive is toward the bow.

**This recovers the stern, not a seating order.** Per-device GPS bias is of the
same order as the spacing between seats. Measured across the validated
sessions, the projected offsets span 16 to 20 m for crews in a hull of roughly
12 to 13 m, and gaps between adjacent paddlers run from 0.09 m to 10.4 m where
every one should sit near 2 m. Only the stern survives: the steerer is a large,
consistent outlier, 4 to 9.7 m behind the next paddler in all six crews
checked, and a gap that size is robust to metre-scale bias.

So `treinus_detect_steerer()` returns `is_stern` and the margin, and emits no
seat ranking. An earlier version returned a `seat` column and drew a seating
diagram; both implied a precision the measurement does not have.

Two refinements matter, and both came out of trying to break the method:

**Compare pairs, not positions.** A centroid-relative `along` shifts for
everyone whenever a crew member's watch drops out, and crew membership does
change mid-session here (Atleta 9 recorded only 07:50-08:22, Atleta 1's watch
paused 13 minutes). Taking the median of `along_i - along_j` over the bins the
pair shares removes the centroid entirely.

**Require the order to hold in both directions of travel.** A wrist GPS carries
a quasi-fixed positional bias of a few metres. On an out-and-back course that
bias projects onto the heading as `+b` outbound and `-b` inbound, so a pooled
median hides it; on a one-way course it would not cancel at all. Splitting the
bins by heading sector and requiring `sign(d_N) == sign(d_S)` exposes exactly
that failure.

Confidence is sector consistency, not a margin in metres. On the reference
session, 22 of 23 pairs agree in both directions; the single disagreement is
Atleta 1-Atleta 8, which is precisely the pair a margin-based reading also called
ambiguous. The two measures agree on where the doubt is, and the sector test
states it as a fact about the data rather than as a tuned threshold.

Validation, including a crew the method was never developed against:

| Crew | Rearmost | Sector-consistent | Ground truth |
|---|---|---|---|
| Canoa 1 | Atleta 2 | yes, all pairs | Atleta 2 |
| Canoa 2 | Atleta 1 | no, vs Atleta 8 | Atleta 1 |
| Canoa 3 | Atleta 3 | yes, all pairs | Atleta 3 |

Three crews, three correct. Canoa 3 matters most: it has only three of six
seats recording and was not used to develop the method.

### A second, independent line on the steerer

Steve West, *Outrigger Canoeing - The Art and Skill of Steering* (Kanu Culture,
7th ed. 2014, pp. 22, 31, 86, 89) is explicit that a steerer stays in time with
the crew and never paddles at a higher rate, because a stroke out of time breaks
the crew's rhythm. But every poke is a stroke not taken, so the steerer's
*measured* cadence sits below the crew's, by an amount that tracks the
paddle-to-poke split - roughly 75/25 on flat water, 50/50 in a moderate sea.

`treinus_steerer_by_cadence()` therefore ranks each paddler by their shortfall
against the median of **the others**, over long blocks only, and
`treinus_check_steerer()` reports where that ranking contradicts the label.
Across the two validated sessions the ranking agrees with the GPS seat order in
five of six crews.

Two limits, both established by testing rather than assumed:

* **A cadence fault imitates a steerer exactly.** A watch that under-counts
  strokes produces the same signature as heavy poking. On 26/09 Canoa 1 the
  flagged device outranks the real steerer, so the athletes flagged for
  `cadence` must be excluded before the ranking means anything.
* **It has no power over a steerer who barely pokes.** On flat water Atleta 1's
  shortfall was 1.0 spm, which no threshold can separate from noise. The check
  is silent there, correctly. It fires where the shortfall is material: 6.2 spm
  for Atleta 2, 11.0 for Atleta 8.

An earlier design put a threshold on the labelled steerer's own deviation
instead, and was abandoned because it had no power at all: where a crew paddles
tightly together, a wrong label shifts the line by about half a stroke.
Comparing candidates against each other works; comparing one against an
absolute value does not.

**The known limit.** This identifies the rearmost *recording device*, which
equals the steerer only when the steerer is wearing a watch. With fewer than
six recording seats that is an assumption, not a finding, so the app shows
`n recording` beside every proposal and never treats a crew with a missing
stern as confident.

### Data quality

Every rule below is derived from a fault observed in the reference session, not
invented.

| Check | Rule | Action | Fires on |
|---|---|---|---|
| HR flatline | longest run of identical values >= 300 s | propose drop | Atleta 11 (818 s) |
| HR never acquired | rolling 5-min median < 100 bpm while the boat holds > 8 km/h, for > 10 min | propose drop | Atleta 1 |
| HR noise | > 10% of consecutive samples change by > 3 bpm **per second** | flag | Atleta 6 (19.6%) |
| HR spike | `max - p99 > 20` bpm | flag | Atleta 7 (22) |
| Cadence dropout | > 10% of **in-block** samples <= 20 spm, steerers exempt | flag | Atleta 5 (22.6%), Atleta 4 (15.0%) |
| Cadence ceiling | session max below the crew's p90 | flag | Atleta 4, Atleta 7 (53 vs crew 60+) |
| Aborted recording | `total_time < 300 s`, or distance implausible against elapsed | propose drop | ex 223 |

Every threshold was checked against all sixteen traces in the reference
session, including the two land-training athletes and the crew excluded from
the report. Four of the seven rules changed as a result:

- **A "> 80% repeated consecutive samples" rule was dropped entirely.** It was
  written from Atleta 11's 93%, but athletes 29 and 36 sit at 69-70% on healthy
  traces — too close to call. The run-length rule alone separates cleanly:
  Atleta 11 818 s, next highest 113 s.
- **HR noise is normalised by `dt`.** Sampling intervals differ sixfold across
  devices, so a per-sample threshold fires on every slow logger. Per second,
  Atleta 6 is at 19.6% and the next highest is 6.6%.
- **The HR spike threshold moved from 15 to 20 bpm.** At 15, Atleta 9
  (14) is a near miss on a trace with nothing wrong with it.
- **Cadence dropout is in-block only, and exempts steerers.** Measured over the
  whole session at a 5% threshold it fires on nine of sixteen athletes, because
  everyone stops paddling sometimes. In-block, the same athletes read 22.6% and
  15.0% against roughly 0% for the rest.

One rule was challenged and kept: cadence ceiling was said to fire on every
steerer, but the three steerers here reach 67, 79 and 107 spm. It stays.

Only the first two and the last propose dropping. The others flag, because they
change how a number should be read without invalidating it: Atleta 7's spike
breaks `fc_max` alone, and a cadence dropout is exactly the thing that should
stay visible rather than be quietly removed.

Thresholds live in the package, not the settings file, because a flag does not
change any published number. If one ever starts driving an exclusion by
default, it moves into the YAML.

Each proposed exclusion reports its cost in valid data. Atleta 1's heart rate is
bad for an hour and fine after 08:23; whole-session exclusion discards the good
43 minutes, and the app says so. Per-window exclusion, if it is ever wanted,
is a change to `prepare` alone.

## Measurement

These carry over from `analyze_20260926.R`, which is retired once they exist.
Current behaviour is the specification, including the corrections made during
its own review:

| Function | Returns |
|---|---|
| `treinus_athlete_summary()` | per athlete: distance, elapsed, moving, speeds, HR, cadence |
| `treinus_hr_zones()` | time and share per zone |
| `treinus_boat_speed()` | one speed per crew per bin |
| `treinus_pieces()` | detected blocks per crew |
| `treinus_km_splits()` | interpolated km crossings |
| `treinus_cadence_line()` | per-rower deviation from the crew stroke |
| `treinus_pace_to_distance()` | moving/stopped/gap time to a reference distance |
| `treinus_crew_gap()` | signed speed difference between any two crews |
| `treinus_fastest_straight()` | thin wrapper over `fastest_straight_distance()` |

Invariants that must survive the move, each of which was a bug once:

- `dt` capped at `max_sample_gap_s`, so auto-pause gaps are not training time.
- Km splits interpolated at whole-kilometre crossings from deduplicated
  cumulative distance, never bucketed by `floor(distance/1000)`.
- Boat speed averaged within athlete per bin first, then median across the
  crew, dropping bins with fewer than two members recording.
- Pieces detected against a per-crew relative threshold, never an absolute
  km/h.
- The cadence line is the median of non-steerers, requiring at least three of
  them, computed only inside pieces and smoothed over `cadence_smooth_s`.

### All-NA guards

Exclusion-as-NA makes latent failures certain, so each measure function must
handle an all-NA metric explicitly. These are live bugs today for any athlete
whose device has no HR sensor:

| Expression | All-NA result |
|---|---|
| `mean(x, na.rm = TRUE)` | `NaN` |
| `max(x, na.rm = TRUE)` | `-Inf`, with a warning |
| `filter(!is.na(heart_rate))` | athlete vanishes from the table with no note |
| `km / (moving_min / 60)` when speed is excluded | `Inf` |
| `seq_len(floor(max(distance)/1000))` when distance is excluded | error |

Guard in the measure functions, returning `NA_real_`, not in `prepare`.
`exclude` is restricted to `heart_rate` and `cadence`; any other metric name is
an error, because excluding `speed` or `distance` is not a coherent request.

The app must also report a second-order cost. Accepting both cadence flags on
Canoa 1 leaves two non-steerers on the line, below the minimum of three, so
that crew's cadence analysis disappears entirely. That consequence is shown
before the exclusion is accepted, not discovered afterwards.

### Figures and the report template

Figures live in `R/` beside the measures, as `treinus_plot_*()` returning
ggplot objects, so the app and the report draw the same charts from the same
code: crew speed, crew gap, HR panels, cadence deviation, seat order.

The report splits in two. `inst/templates/session_report.qmd` is generic and
calls only package functions. Session-specific prose — "no primeiro bloco a
Canoa 1 esteve à frente em 77% do tempo" — cannot live in a shared template, so
each session gets a copy of the template plus its own commentary. The template
is a starting point, not a thing that renders unchanged for every session.

## The app

Six steps down a sidebar. Each shows the evidence for the decision it asks for.

| Step | Shows | Analyst does |
|---|---|---|
| Sessão | date picker, loads from DB or RDS; exercises found, aborted ones marked | confirm which exercises are in |
| Relógio | detected offset per athlete beside its evidence | override any that look wrong |
| Tripulações | track map coloured by crew, pair-separation table | tick crews to analyse, reassign a paddler |
| Bancos | along-hull seat plot, stern margin per crew | pick the steerer; detection preselects |
| Qualidade | flag table with evidence and cost of excluding | accept or reject each exclusion |
| Relatório | YAML preview | render, opens the HTML |

### Dependencies

Moving analysis into `R/` makes several current vignette-only packages into
hard dependencies, and `yachtvaa` inherits them:

- **New `Imports`**: `slider`, `lubridate`, `tidyr`, `ggplot2`.
- **New `Suggests`**: `yaml`, `shiny`, `ggrepel`, `gt`, `quarto`.

`shiny` and `yaml` stay in `Suggests` so the package still installs for
`yachtvaa` without them; the app checks for them at launch. Plots are plain
`ggplot2` — a crew map does not justify a `leaflet` dependency.

## Failure modes

- **No API session.** Deep dives happen after the fact, so the local database
  and a saved RDS are first-class inputs, not a fallback path.
- **Degenerate detection.** A crew of one, or fewer than two crews, stops and
  asks rather than guessing.
- **Merged crews.** Two boats that stay within `near_m` for most of a session —
  drafting, a side-by-side piece, a race start — collapse into one component.
  A component larger than six seats is a hard signal, and the app shows
  component sizes rather than assuming detection was right. The reference
  session's crews were 70 m apart, which is not the general case.
- **Unassigned athletes.** A mid-session seat swap can leave someone with no
  edge above `near_pct`, silently dropping them. The app lists anyone
  unassigned together with their closest pairings.
- **Ambiguous steerer.** Stern margin below 1 m is flagged and the alternatives
  offered; nothing is silently chosen.
- **Quarto missing.** Plain message, not a stack trace.
- **Stale settings file.** `schema` mismatch fails loudly.

## Testing

Test-first, per the repo's usual `devtools::test()` workflow.

- **Unit tests per function** against a small committed fixture: two crews,
  about fifteen minutes, trimmed from the reference session so it stays a few
  hundred KB. Fifteen rather than ten: ten minutes yields about sixty 10-second
  bins, exactly `min_bins`, so the fixture would sit on the boundary.
- **Golden test on the reference session.** Crews resolve to the three known
  components; steerers come out Atleta 2, Atleta 1 **and Atleta 3**; the Canoa 1
  kilometre is 299 s; `treinus_detect_clock()` returns exactly athletes 36 and
  50 as local, from the records alone. This is what stops a refactor from
  quietly changing a published number. Marked `skip_on_cran` — the fastest
  straight search is quadratic per segment.
- **Threshold regression test.** Every quality rule runs against all sixteen
  traces and must fire on exactly the athletes listed in the table above,
  with no additions. This is what stops a retuned threshold from silently
  starting to flag healthy data.
- **Settings round-trip.** Write, read, compare; the only app-level test. The
  UI itself gets none.

## Scope note

This moves `rtreinus` further from "API client" toward "API client plus session
analysis". `fastest_straight_distance()` already crossed that line, and
`yachtvaa` depends on this package, so the direction is deliberate rather than
accidental.

## Out of scope

- Per-window metric exclusion (whole-session only, by decision).
- Hosting or multi-user access.
- Any change to authentication, caching or the API client itself.
