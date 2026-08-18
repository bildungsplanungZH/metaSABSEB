# Test change_meta() ----------------------------------------------------------

# Shared helpers ----------------------------------------------------------

# Build an isolated temp metadata file so tests don't depend on the real
# shipped package YAML, and don't risk mutating real package data.
setup_temp_meta <- function() {
    tmp_path <- withr::local_tempfile(fileext = ".yaml", .local_envir = parent.frame())
    meta <- list(
        TEST_VAR = list(
            umfrage_item = "Testbeschreibung",
            oberthema   = "alter Wert"
        ),
        OTHER_VAR = list(
            umfrage_item = "test"
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

# Function errors if required fields have not been specified ------------------

test_that("change_meta() fails if var is not specified", {
    expect_error(
        change_meta(),
        "muss mitgegeben werden"
    )
})

test_that("change_meta() fails if field is not specified", {
    expect_error(
        change_meta(var = "TEST_VAR"),
        "muss mitgegeben werden"
    )
})

test_that("change_meta() fails if new_entry is not specified", {
    expect_error(
        change_meta(var = "TEST_VAR", field = "oberthema"),
        "muss mitgegeben werden"
    )
})

# Function replaces an existing field ------------------------------------------

test_that("change_meta() actually changes meta for an existing field", {
    local_meta_env_snapshot()
    tmp_path <- setup_temp_meta()

    # Indicator that mocked binding has been called
    called <- FALSE

    # Temporarily replace confirm_replacement(), which normally asks the user
    # interactively whether the change should be made. The mock bypasses the
    # interactive menu and directly performs the replacement for this test.
    local_mocked_bindings(
        confirm_replacement = function(yaml_path, meta, var, field, new_entry) {
            called <<- TRUE
            meta[[var]][[field]] <- new_entry
            .meta_env$meta <- meta
        },
        .package = "metaSABSEB"
    )

    before <- yaml::read_yaml(tmp_path)$TEST_VAR$oberthema

    change_meta(
        var = "TEST_VAR",
        field = "oberthema",
        new_entry = "TEST",
        yaml_path = tmp_path
    )

    after <- .meta_env$meta$TEST_VAR$oberthema

    expect_true(called)
    expect_false(identical(before, after))
    expect_identical(after, "TEST")
})

# Function fails if var does not exist in meta --------------------------------

test_that("check_var_exists() fails if variable does not exist", {
    meta <- list(
        EFZ_Typ   = list(description = "test"),
        Anst_best = list(description = "test")
    )
    expect_error(
        check_var_exists(var = "THIS_VARIABLE_DOES_NOT_EXIST", meta),
        "konnte nicht in den Metadaten gefunden werden"
    )
})

test_that("change_meta() fails if variable does not exist", {
    tmp_path <- setup_temp_meta()

    expect_error(
        change_meta(
            var = "THIS_VARIABLE_DOES_NOT_EXIST",
            field = "oberthema",
            new_entry = "Test",
            yaml_path = tmp_path
        ),
        "konnte nicht in den Metadaten gefunden werden"
    )
})


