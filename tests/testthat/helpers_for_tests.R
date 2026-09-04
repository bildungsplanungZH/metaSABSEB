# Shared test helpers for metaSABSEB ------------------------------------------
# Automatically sourced by testthat before running any test-*.R file.

# Build an isolated temp metadata file so tests don't depend on the real
# shipped package YAML, and don't risk mutating real package data.
setup_temp_meta <- function() {
  tmp_path <- withr::local_tempfile(
    fileext = ".yaml",
    .local_envir = parent.frame()
  )
  meta <- list(
    TEST_VAR1 = list(
      oberthema = "Kernobst",
      unterthema = "Äpfel",
      var_typ = "Skalenitem: Beliebtheit Kernobst",
      umfrage_item = "Wie gerne mögen Sie Äpfel?",
      skalenberechnung = "Mean",
      status = "aktiv",
      values = list(
        "1" = "gar nicht",
        "2" = "eher nicht",
        "3" = "mittel",
        "4" = "eher gerne",
        "5" = "sehr gerne"
      )
    ),
    TEST_VAR2 = list(
      oberthema = "Kernobst",
      unterthema = "Birnen",
      var_typ = "Skalenitem: Beliebtheit Kernobst",
      umfrage_item = "Wie gerne mögen Sie Birnen?",
      skalenberechnung = "Mean",
      status = "aktiv",
      values = list(
        "1" = "gar nicht",
        "2" = "eher nicht",
        "3" = "mittel",
        "4" = "eher gerne",
        "5" = "sehr gerne"
      )
    )
  )
  yaml::write_yaml(meta, tmp_path)
  tmp_path
}

# Snapshot/restore .meta_env$meta around tests that write to the cache, so
# state can't leak between tests.
local_meta_env_snapshot <- function(env = parent.frame()) {
  old <- .meta_env$meta
  withr::defer(.meta_env$meta <- old, envir = env)
}
