# Test get_meta() -------------------------------------------------------------


# Function errors if var does not exist in meta -------------------------------

# internal helper check_var_exists
test_that("check_var_exists() fails if variable does not exist", {

    meta <- list(
        EFZ_Typ = list(description = "test"),
        Anst_best = list(description = "test")
    )

    expect_error(
        check_var_exists(var="THIS_VARIABLE_DOES_NOT_EXIST", meta),
        "konnte nicht in den Metadaten gefunden werden"
    )
})

# integration level: also test if check_var_exists fails within get_meta()
test_that("get_meta() fails if variable does not exist", {

    expect_error(
        get_meta(var="THIS_VARIABLE_DOES_NOT_EXIST"),
        "konnte nicht in den Metadaten gefunden werden"
    )
})

# Function returns all fields if field is not specified -----------------------
test_that("get_meta() returns all meta for var if field=NULL", {

    # Function call
    result <- get_meta(var="EFZ_Typ")

    # Expect it to return a list object
    expect_type(result, "list")

    # Expect the object to be named
    expect_named(result)

    # Expect it to correspond to the cached metadata entry
    expect_identical(
        result,
        .meta_env$meta[["EFZ_Typ"]]
    )

})

# Function returns all info if a field does not exist for that var ------------
test_that("get_meta() returns all fields if field cannot be found for var", {

    # Expect warning message
    expect_message(
        get_meta(var="EFZ_Typ", field = "INEXISTENT_FIELD"),
        "kein Metadateneintrag"
    )

    # Function call for result
    result <- get_meta(
        var = "EFZ_Typ",
        field = "INEXISTENT_FIELD"
    )


    # Expect it to return a list object
    expect_type(result, "list")

    # Expect the object to be named
    expect_named(result)

    # Expect it to correspond to the cached metadata entry
    expect_identical(
        result,
        .meta_env$meta[["EFZ_Typ"]]
    )

})

# Function returns only returns requested field if field is specified ---------
test_that("get_meta() returns requested field", {

    # Function call
    result <- get_meta(var="EFZ_Typ", field = "umfrage_item")

    # Expect it to return a character value
    expect_type(result, "character")

    # Expect it to correspond to the cached metadata entry
    expect_identical(
        result,
        .meta_env$meta[["EFZ_Typ"]][["umfrage_item"]]
    )
})

# Function refreshes cache if refresh = TRUE ----------------------------------
test_that("get_meta() cache refresh works", {

    # Save current cache so it can be restored after test
    true_meta <- .meta_env$meta
    on.exit({.meta_env$meta <- true_meta})

    # Add fake value to cache
    .meta_env$meta <- list(
        EFZ_Typ = list(
            oberthema = "FAKE VALUE"
        )
    )

    # For refresh = FALSE
    # Check that fake value is returned if cache is not refreshed
    result1 <- get_meta("EFZ_Typ","oberthema", refresh = FALSE)
    expect_identical(result1, "FAKE VALUE")

    # For refresh = TRUE
    result2 <- get_meta("EFZ_Typ","oberthema", refresh = TRUE)
    expect_false(identical(result2, "FAKE VALUE"))
    expect_false(
        identical(
            .meta_env$meta[["EFZ_Typ"]][["oberthema"]],
            "FAKE VALUE"
        )
    )

})

