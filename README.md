# rtreinus

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->
**rtreinus** provides authenticated access to the [Treinus](https://webapp.treinus.com.br) workout tracking platform API. Retrieve your training data, performance metrics, and workout history programmatically in R.

## Installation

```r
# install.packages("pak")
pak::pak("leoniedu/rtreinus")
```

## Setup

### Configure Credentials

```r
library(rtreinus)

# Interactive setup (prompts for password securely)
treinus_set_credentials()

# Or provide directly
treinus_set_credentials(
  email = "your.email@example.com",
  password = "your-password"
)
```

This saves credentials to `~/.Renviron`. Restart R or run `readRenviron("~/.Renviron")` to load them.

## Usage

### Authenticate

```r
library(rtreinus)

session <- treinus_auth()
# If you have multiple teams, you'll be prompted to select one
```

### Get Exercises

```r
# Get exercises for an athlete
exercises <- treinus_get_exercises(session, athlete_id = 50)

# Get exercises for multiple athletes
exercises <- treinus_get_exercises_batch(session, athlete_ids = c(50, 51, 52))
```

### Get Exercise Analysis (GPS/Sensor Data)

```r
# Get detailed analysis with GPS records
analysis <- treinus_get_exercise_analysis(
 session,
 exercise_id = 57,
 athlete_id = 50,
 team_id = 2994
)

# Extract time-series records (raw with unit attributes)
records <- treinus_extract_records(analysis)
attr(records, "DistanceUnit")  # Check units

# Extract with standardized units (km, km/h, degrees)
records <- treinus_extract_records(analysis, standardize = TRUE)
```

### Custom API Requests

```r
# Make a custom GET request
resp <- treinus_request(
 session,
 endpoint = "/Athlete/Performance/Index",
 method = "GET"
)

# Parse HTML response
page <- httr2::resp_body_html(resp)
data <- treinus_parse_table(page)
```

## Available Functions

### Authentication
- `treinus_auth()` - Create authenticated session
- `treinus_session_valid()` - Check session validity
- `treinus_set_credentials()` - Configure credentials
- `treinus_has_credentials()` - Check if credentials are set
- `treinus_config()` - View configuration

### Data Retrieval
- `treinus_get_exercises()` - Get exercises for an athlete
- `treinus_get_exercises_batch()` - Get exercises for multiple athletes
- `treinus_get_exercise_analysis()` - Get detailed GPS/sensor data
- `treinus_extract_records()` - Extract and optionally standardize records
- `treinus_get_dashboard()` - Get dashboard summary
- `treinus_request()` - Make custom API requests
- `treinus_parse_table()` - Parse HTML tables

## Security

**Never commit credentials to version control!**

- Use environment variables via `.Renviron`
- Add `.Renviron` to your `.gitignore`
- Use `treinus_set_credentials()` for secure setup

## License

MIT
