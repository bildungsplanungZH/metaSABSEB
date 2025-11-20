#-------------------------------------------------------------------------------

# IDENTIFY MISSING METADATA

#-------------------------------------------------------------------------------

# This script aims to:
# - Import the metadata export from the survey agency.
# - Restructure the provided metadata to fit our metadata-model.
# - Join with variable_names from raw data.
# - Identify not defined variables and missing fields.
# - Create an Excel-Table with the metadata that still needs to be added manually.

#-------------------------------------------------------------------------------
# Step 0: Install packages / load libraries using "pacman"
#-------------------------------------------------------------------------------

## a) Ensure that the package "pacman" itself is installed and loaded
if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
}
library(pacman)

## b) Define necessary packages
packages <- c(
    "dplyr",
    "biplaRdb",
    "readr"
)

## c) Install (if missing) and load necessary packages
pacman::p_load(char = packages)


#-------------------------------------------------------------------------------
# Step 1: Import the survey agency's metadata export and clean the data
#-------------------------------------------------------------------------------

## a) Read CSV that we receive from ZEM CES
received_meta <- read.csv(
    "data/received_meta/2025-11-07_metadata.csv",
    header = FALSE,
    stringsAsFactors = FALSE
    )

## b) Fix delimiter issues
    # Note: Issues arise from having some observations quoted and some not

received_meta <- received_meta |>

    # Ensure NULL-Values are handeled correctly (add quotation marks)
    mutate(across(where(is.character), ~ gsub('NULL', '"NULL"', .))) |>
    # Replace commas that were meant to be delimiters with a semicolon
    # (there are also commas in text sections, which aren't delimiters and shouldn't be replaced)
    mutate(across(where(is.character), ~ gsub(',"', ";", .))) |>
    # Remove the rest of the quotation marks
    mutate(across(where(is.character), ~ gsub('"', "", .))) |>
    # Now parse into 6 columns
    separate(
        col = V1,
        into = paste0("V", 1:6),
        sep = ";",
        fill = "right"
    )

## c) Clean received meta
received_meta <- received_meta |>
    # Remove leading and trailing blank spaces
    mutate(across(where(is.character), ~ stringr::str_trim(.))) |>
    # Replace multiple spaces with single spaces
    mutate(across(where(is.character), ~ gsub("\\s+", " ", .))) |>
    # Handle NULL values
    mutate(across(everything(),~ na_if(.x, "NULL")))

## d) Define column names after parsing properly (use first row)
colnames(received_meta) <- received_meta[1, ]
received_meta <- received_meta[-1, ]


#-------------------------------------------------------------------------------
# Step 2: Add received metadata to the structure
#-------------------------------------------------------------------------------

## a) Load current metadata
load("data/metadata.RData")

## b) Fill existing structure with received_meta

# Map
columns_to_add <- c("Skalenname","ItemName", "Gruppentitel", "Itemtext")
target_columns <- c("var_beschreibung", "variable", "var_item", "var_itemformulierung")

# Rename and ensure compatibility
received_meta <- received_meta |>
    select(all_of(columns_to_add)) |>
    rename_with(~ target_columns, everything()) |>
    mutate(across(everything(), ~ as.character(.)))
variables_meta <- variables_meta |>
    mutate(across(everything(), ~ as.character(.)))

# Full join based on variable name (keep all observations from both datasets)
variables_meta <- variables_meta |>
    full_join(received_meta, by = "variable")

#-------------------------------------------------------------------------------
# Step 3: Update current metadata and deal with conflicts
#-------------------------------------------------------------------------------

## a) Deal with conflicting entries

# Initialise conflict column
if(!"var_conflict" %in% colnames(variables_meta)){
    variables_meta$var_conflict <- NA_character_
}

# Define which variables to check
vars <- c("beschreibung", "item", "itemformulierung")

# Check for conflicts, replace if no conflict (loops over all defined variables)
for (v in vars) {
    current_col <- paste0("var_", v, ".x")
    received_col <- paste0("var_", v, ".y")

    variables_meta <- variables_meta |>

        #If current metadata is empty, add received metadata to current
        mutate (!!current_col := if_else(
            is.na(.data[[current_col]]), # check NA in the current column
            .data[[received_col]], # take value from received
            .data[[current_col]] # else keep current
        )) |>
        # Otherwise check if received metadata = current metadata, else indicate conflict
        mutate(var_conflict = case_when(
            !is.na(.data[[current_col]]) & # check current is not NA
                .data[[current_col]] != .data[[received_col]] # check current is not received
            ~ paste0( # Indicate conflict in dedicated column
                var_conflict,
                "Conflict:",
                .data[[current_col]],
                "is not equal",
                .data[[received_col]],
                "\n"),
            TRUE ~ NA_character_
        ))
}

## b) Drop received meta and rename current metadata which has been updated
variables_meta <- variables_meta |>
    # Remove received columns
    select(-ends_with(".y"))  |>
    # Remove .x suffix from updated current data
    rename_with(~ gsub("\\.x$", "", .x), ends_with(".x"))
# Remove received_meta dataframe
rm(received_meta)

#-------------------------------------------------------------------------------
# Step 4: Import variables from raw data
#-------------------------------------------------------------------------------

## a) Import raw data




#-------------------------------------------------------------------------------
# Step 5: Identify variables which have not yet been added to metadata
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Step 6: Create "Add metadata"-CSV, for missing information
#-------------------------------------------------------------------------------















