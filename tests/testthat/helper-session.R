# The committed fixture is a records-only snapshot, so clock detection there
# legitimately falls back on clustering and warns every time. Tests that are
# not about the clock use this helper to keep that expected warning out of the
# output; test-session-detect.R asserts the warning itself.
draft_quietly <- function(records, exercises = NULL, ...) {
  withCallingHandlers(
    treinus_draft_settings(records, exercises, ...),
    warning = function(w) {
      if (grepl("clustering start times", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
