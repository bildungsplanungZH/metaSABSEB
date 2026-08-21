# Test add_meta() -------------------------------------------------------------

# Test that adding new meta works ---------------------------------------------

test_that("add_meta adds a complete metadata entry to meta", {

    # Use test yaml
    local_meta_env_snapshot()
    yaml_path <- setup_temp_meta()

    # Execute function with all mandatory fields
    add_meta(
        var = "TEST_VAR3",
        oberthema = "Beeren",
        unterthema = "Himbeeren",
        var_typ = "Skala: Beliebtheit Beeren",
        umfrage_item = "Wie gerne mögen Sie Himbeeren?",
        skalenberechnung = "Mean",
        values = list(
            "1" = "gar nicht",
            "2" = "eher nicht",
            "3" = "mittel",
            "4" = "eher gerne",
            "5" = "sehr gerne"
        ),
        yaml_path = yaml_path
    )

    # Test that YAML has been updated -----------------------------------------

    # Read modified yaml
    meta <- yaml::read_yaml(yaml_path)

    # TEST_VAR3 must now appear in meta
    expect_true("TEST_VAR3" %in% names(meta))

    # Fields in meta must correspond to meta entry (one test per field)
    expect_equal(meta$TEST_VAR3$oberthema, "Beeren")
    expect_equal(meta$TEST_VAR3$unterthema, "Himbeeren")
    expect_equal(
        meta$TEST_VAR3$var_typ,
        "Skala: Beliebtheit Beeren"
    )
    expect_equal(
        meta$TEST_VAR3$umfrage_item,
        "Wie gerne mögen Sie Himbeeren?"
    )
    expect_equal(meta$TEST_VAR3$status, "aktiv")
    expect_equal(
        meta$TEST_VAR3$values,
        list(
            "1" = "gar nicht",
            "2" = "eher nicht",
            "3" = "mittel",
            "4" = "eher gerne",
            "5" = "sehr gerne"
        )
    )

    # Test that cache was also updated ----------------------------------------
    expect_true("TEST_VAR3" %in% names(.meta_env$meta))
    expect_equal(.meta_env$meta$TEST_VAR3$oberthema, "Beeren")
    expect_equal(.meta_env$meta$TEST_VAR3$unterthema, "Himbeeren")

    # Test that NULL fields are not added -----------------------------------------
    expect_false("itemformulierung" %in% names(meta$TEST_VAR3))
    expect_false("umfrage_filter" %in% names(meta$TEST_VAR3))
    expect_false("quelle" %in% names(meta$TEST_VAR3))
})


