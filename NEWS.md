# rtreinus (development version)

## rtreinus 0.1.0

### New Features

* Initial release of rtreinus
* `treinus_auth()` - Authenticate with Treinus webapp
* `treinus_session_valid()` - Check if session is still valid
* `treinus_get_workouts()` - Retrieve workout data
* `treinus_get_dashboard()` - Get dashboard summary
* `treinus_request()` - Make custom API requests
* `treinus_set_credentials()` - Securely configure credentials
* `treinus_has_credentials()` - Check credential status
* `treinus_config()` - View current configuration

### Documentation

* Added comprehensive README with examples
* Created "Getting Started" vignette
* Full function documentation with examples

### Technical Details

* Implements ASP.NET WebForms authentication flow
* Handles ViewState and ViewStateGenerator tokens automatically
* Session cookie management via httr2
* HTML parsing with rvest
* Modern R package structure following best practices
