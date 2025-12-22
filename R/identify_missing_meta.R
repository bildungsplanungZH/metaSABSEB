#-------------------------------------------------------------------------------

# IDENTIFY MISSING METADATA

#-------------------------------------------------------------------------------

# This script aims to:
# - Import the metadata export from the survey agency.
# - Restructure the provided metadata to fit our metadata-model.
# - Join with variable_names from raw data.
# - Identify not defined variables (appear in raw data but not provided metadata)
# - Create an Excel-Table with the metadata that still needs to be added manually.

#-------------------------------------------------------------------------------
# Step 0: Configuration: Install packages and load libraries using "pacman"
#-------------------------------------------------------------------------------

## a) Ensure that the package "pacman" itself is installed and loaded
if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
}
library(pacman)

## b) Define necessary packages to run this script
packages <- c(
    "readr",
    "readxl",
    "openxlsx",
    "dplyr"
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

### i) Map column names
columns_to_add <- c("Skalenname","ItemName", "Gruppentitel", "Itemtext")
target_columns <- c("var_beschreibung", "variable", "var_item", "var_itemformulierung")

### ii) Rename and ensure compatibility
received_meta <- received_meta |>
    select(all_of(columns_to_add)) |>
    rename_with(~ target_columns, everything()) |>
    mutate(across(everything(), ~ as.character(.)))
variables_meta <- variables_meta |>
    mutate(across(everything(), ~ as.character(.)))

### iii) Full join based on variable name (keep all observations from both datasets)
variables_meta <- variables_meta |>
    full_join(received_meta, by = "variable")

#-------------------------------------------------------------------------------
# Step 3: Update current metadata and deal with conflicts
#-------------------------------------------------------------------------------

## a) Deal with conflicting entries

# Initialise To Do column (will list all conflicts and other To Do's)
if(!"var_conflict" %in% colnames(variables_meta)){
    variables_meta$to_do <- ""
}

# Define which variables to check
vars <- c("var_beschreibung", "var_item", "var_itemformulierung")

# Check for conflicts, replace if no conflict (loops over all defined variables)
for (v in vars) {
    current_col <- paste0(v, ".x")
    received_col <- paste0(v, ".y")

    variables_meta <- variables_meta |>

        #If current metadata is empty, add received metadata to current
        mutate (!!current_col := if_else(
            is.na(.data[[current_col]]), # check NA in the current column
            .data[[received_col]], # take value from received
            .data[[current_col]] # else keep current
        )) |>
        # Otherwise check if received metadata = current metadata, else indicate conflict
        mutate(to_do = case_when(
            !is.na(.data[[current_col]]) & # check current is not NA
                .data[[current_col]] != .data[[received_col]] # check current is not received
            ~ paste0( # Indicate conflict in dedicated column
                to_do,
                "CONFLICT: Current and received metadata differ for the column*",
                v,
                '*, "',
                .data[[current_col]],
                '" (current) is not equal to "',
                .data[[received_col]],
                '" (received).\n'),
            TRUE ~ to_do
        ))
}

## b) Drop received meta and rename current metadata which has been updated
# Current metadata (updated)
variables_meta <- variables_meta |>
    # Remove received columns
    select(-ends_with(".y"))  |>
    # Remove .x suffix from updated current data
    rename_with(~ gsub("\\.x$", "", .x), ends_with(".x"))

# Remove received_meta dataframe and helper vectors
rm(received_meta, columns_to_add, current_col, received_col, target_columns, v, vars)

#-------------------------------------------------------------------------------
# Step 4: Import variables from raw data
#-------------------------------------------------------------------------------

## a) Import variable names which are in raw data (Excels we get from ZEM CES)
# Only read the first row
# SAB 2025
sab25 <- read_excel(
    "K:/BI-BP-03-Arbeiten/QM Sek II/11 SAB 2025/3_Daten/76-SAB_2025_Datensatz Kt ZH BfS 250825.xlsx",
    col_names=FALSE)
# SEB 2024
seb24 <- read_excel(
    "K:/BI-BP-03-Arbeiten/QM Sek II/04 SEB 2024/Daten/SEB_2024_Rohdaten Kanton Zürich.xlsx",
    col_names=FALSE
)

# Create indicator if there is only NA values for a variable
sab25_empty <- sab25 |> summarise(across(everything(), ~ all(is.na(.x[-1])))) |> unlist()
seb24_empty <- seb24 |> summarise(across(everything(), ~ all(is.na(.x[-1])))) |> unlist()

# Only keep the first row
first_row_sab25 <- sab25 |>
    slice(1)
first_row_seb24 <- seb24 |>
    slice(1)

# Convert to single column dataframes and drop first row dfs
sab_vars <- data.frame(
    variable = unlist(first_row_sab25),
    var_defs_key = "sab",
    empty_variable = sab25_empty)

seb_vars <- data.frame(
    variable = unlist(first_row_seb24),
    var_defs_key = "seb",
    empty_variable = seb24_empty)

rm(first_row_sab25, first_row_seb24)

# Combine SAB and SEB and identify overlaps
vars_in_raw_data <- full_join(seb_vars, sab_vars, by = "variable") |>
    mutate(var_defs_key = case_when(
        !is.na(var_defs_key.x) & !is.na(var_defs_key.y) ~ "sab&seb",  # in both
        !is.na(var_defs_key.x) & is.na(var_defs_key.y) ~ "seb",
        is.na(var_defs_key.x) & !is.na(var_defs_key.y) ~ "sab"
    )) |>
    mutate(empty_variable = case_when(
        empty_variable.x == TRUE & empty_variable.y==TRUE ~ TRUE,
        TRUE ~ FALSE
    )) |>
    select(variable, var_defs_key, empty_variable)

# Remove obsolete vectors and dataframes
rm(sab_vars, seb_vars, sab25, seb24, sab25_empty, seb24_empty)


#-------------------------------------------------------------------------------
# Step 5: Identify variables which have not yet been added to metadata
#-------------------------------------------------------------------------------

# Merge with current meta based on varname
variables_meta <- full_join(variables_meta, vars_in_raw_data,
                            by = c("variable"),
                            keep = TRUE) |>
    # Replace to_do NA-values from join (Otherwise printing the messages does not work correctly)
    mutate(to_do = case_when(
        is.na(to_do) ~ paste0(""),
        TRUE ~ to_do
    )) |>
    # If variable does not exist in current meta but is in received meta,
    # create message in to-do column to add the metadata
    mutate(to_do = case_when(
        is.na(variable.x) & !is.na(variable.y)
        ~ paste0(to_do,
                 "ADD INFORMATION: The variable *",
                 variable.y,
                 "* from ",
                 var_defs_key.y,
                 " is only in the raw data and not recorded in the metadata. Please add the information. \n"),
        TRUE ~ to_do
    )) |>
    # If variable exists in current metadata but does not exist in raw data
    # create message in to-do column
    mutate(to_do = case_when(
        !is.na(variable.x) & is.na(variable.y)
        ~ paste0(to_do,
                 "CHECK the variable *",
                 variable.x,
                 "*, (recorded in metadata but missing in raw data).  \n"),
        TRUE ~ to_do
    )) |>
    # If variable exists in raw data and is not yet in metadata, add to list
    mutate(variable.x = case_when (
        is.na(variable.x) & !is.na(variable.y)
        ~ variable.y,
        TRUE ~ variable.x
    )) |>
    # Add var_defs_key from raw data if missing in current metadata
    mutate(var_defs_key.x = case_when (
        is.na(var_defs_key.x) & !is.na(var_defs_key.y)
        ~ var_defs_key.y,
        TRUE ~ var_defs_key.x
        )) |>
    # If var_defs_key differ between raw data and current metadata. Generate to do message
    mutate(to_do= case_when(
        var_defs_key.x != var_defs_key.y
        ~ paste0(to_do,
                 "CHECK KEYS: Conflicting var_defs_key: Current metadata contains",
                 var_defs_key.x,
                 "Raw data contains",
                 var_defs_key.y,
                 "check, and when in doubt, assign both datasets."),
        TRUE ~ to_do
    )) |>
    # If variable exists in raw data but is empty, add message
    mutate(to_do = case_when(
        empty_variable == TRUE
        ~ paste0(to_do,
            "EMPTY DATA COLUMN: The variable *",
            variable.y,
            "* is in the raw data, but only contains missing values. \n"),
            TRUE ~ to_do
    )) |>
    # Remove merging columns and rename
    select(-variable.y, -var_defs_key.y, -empty_variable) |>
    rename_with(~ gsub("\\.x$", "", .x), ends_with(".x"))

# Remove vars_in_raw_data after join
rm(vars_in_raw_data)


#-------------------------------------------------------------------------------
# Step 7: Create "Add metadata"-Excel, for missing information
#-------------------------------------------------------------------------------

#Sort by varname
variables_meta <- variables_meta |>
    arrange(variable)

# Create Excel
missing_meta <- createWorkbook()

# Add each dataframe to a sheet
# Sheet: Datasets
addWorksheet(missing_meta, "datasets")
writeData(missing_meta, "datasets", datasets_meta)
# Auto-fit columns to data
setColWidths(missing_meta, "datasets", cols = 1:ncol(datasets_meta), widths = "auto")

# Sheet: Variables
addWorksheet(missing_meta, "variables")
writeData(missing_meta, "variables", variables_meta)
setColWidths(missing_meta, "variables", cols = 1:ncol(variables_meta), widths = "auto")

# Sheet: Values
addWorksheet(missing_meta, "values")
writeData(missing_meta, "values", values_meta)
setColWidths(missing_meta, "values", cols = 1:ncol(values_meta), widths = "auto")

# Save the workbook
saveWorkbook(missing_meta, "data/missing_meta/missing_meta.xlsx")











