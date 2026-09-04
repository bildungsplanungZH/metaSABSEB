# Test delete_meta() ----------------------------------------------------------

# Function errors if required fields have not been specified ------------------

test_that("delete_meta() fails if var is not specified", {
  expect_error(
    delete_meta(),
    "Bitte spezifizieren Sie"
  )
})

# Function fails if var does not exist in meta --------------------------------

test_that("delete_meta() fails if variable does not exist", {
  tmp_path <- setup_temp_meta()

  expect_error(
    delete_meta(
      var = "THIS_VARIABLE_DOES_NOT_EXIST",
      yaml_path = tmp_path
    ),
    "konnte nicht in den Metadaten gefunden werden"
  )
})

# Function does not modify the file if var does not exist ---------------------

test_that("delete_meta() leaves YAML untouched if variable does not exist", {
  tmp_path <- setup_temp_meta()
  before <- yaml::read_yaml(tmp_path)

  expect_error(
    delete_meta(
      var = "THIS_VARIABLE_DOES_NOT_EXIST",
      yaml_path = tmp_path
    )
  )

  after <- yaml::read_yaml(tmp_path)
  expect_identical(before, after)
})

# Function actually deletes the entry ------------------------------------------

test_that("delete_meta() removes the variable from the YAML and the cache", {
  local_meta_env_snapshot()
  tmp_path <- setup_temp_meta()

  # Sanity check the variable exists before deletion
  expect_true("TEST_VAR1" %in% names(yaml::read_yaml(tmp_path)))

  delete_meta(var = "TEST_VAR1", yaml_path = tmp_path)

  meta_after <- yaml::read_yaml(tmp_path)
  expect_false("TEST_VAR1" %in% names(meta_after))
  expect_false("TEST_VAR1" %in% names(.meta_env$meta))
})

# Function only deletes the specified variable, leaves others intact ----------

test_that("delete_meta() does not affect other variables", {
  local_meta_env_snapshot()
  tmp_path <- setup_temp_meta()

  delete_meta(var = "TEST_VAR1", yaml_path = tmp_path)

  meta_after <- yaml::read_yaml(tmp_path)
  expect_true("TEST_VAR2" %in% names(meta_after))
  expect_identical(
    meta_after$TEST_VAR2,
    list(
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
})

# Function prints a confirmation message ---------------------------------------

test_that("delete_meta() prints a success message", {
  local_meta_env_snapshot()
  tmp_path <- setup_temp_meta()

  expect_message(
    delete_meta(var = "TEST_VAR1", yaml_path = tmp_path),
    "gelöscht"
  )
})
