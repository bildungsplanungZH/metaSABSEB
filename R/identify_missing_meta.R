#-------------------------------------------------------------------------------

# IDENTIFY MISSING METADATA

#-------------------------------------------------------------------------------

# This script aims to:
# 1. Import the metadata export from the survey agency.
# 2. Restructure the provided metadata to fit our metadata-model.
# 3. Join with variable_names from raw data.
# 4. Identify not defined variables and missing fields.
# 5. Create an Excel-Table with the metadata that needs to be added manually.

#-------------------------------------------------------------------------------
# Step 0: Install packages / load libraries
#-------------------------------------------------------------------------------

## a) Define vector of necessary packages
packages <- c(
    "dplyr"
)

## b) Check if packages are installed, otherwise install, then load from library
for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        print ("Installing new package")
        install.packages(pkg)
        print ("Package installed, loading from library")
        library(pkg, character.only = TRUE)
    } else {
        print ("Package already installed, loading from library")
        library(pkg, character.only = TRUE)
    }
}

#-------------------------------------------------------------------------------
# Step 1: Import the survey agency's metadata export
#-------------------------------------------------------------------------------

## a) Read CSV
received_meta <- read.csv("data/2025-11-07_metadata.csv",header=FALSE)

## b) Fix delimiter issues (first column is unquoted, the rest is quoted)
received_meta <- received_meta |>

    # Replace commas that were meant to be delimiters with a semicolon
    # (note that there are also commas in the text, which aren't delimiters and shouldn't be replaced)
    mutate(across(where(is.character), ~ gsub(',"', ";", .))) |>

    # Remove the rest of the quotation marks
    mutate(across(where(is.character), ~ gsub('"', "", .))) |>

    # Now separate into 6 columns
    separate(
        col = V1,
        into = paste0("V", 1:6),
        sep = ";",
        fill = "right"
    )

## c) Define header after correctly parsing (use first row)
colnames(received_meta) <- received_meta[1, ]
received_meta <- received_meta[-1, ]

#-------------------------------------------------------------------------------
# Step 2: Restructure metadata
#-------------------------------------------------------------------------------

#



















