################################################################################
### Import metadata in CSV structure and transform to a YAML
################################################################################

# Load necessary libraries
library(readr)
library(yaml)

# Ensure UTF-8 encoding
Sys.setlocale("LC_CTYPE", "German_Switzerland.utf8")

# Read the CSVs (semicolon delimited)
variable_meta <- read_csv2("data-raw/qm_sekII_variable-meta.csv")
value_meta <- read_csv2("data-raw/qm_sekII_value-meta.csv")

# Drop variables for other cantons
variable_meta <- variable_meta[
    is.na(variable_meta$umfrage_item) |
        variable_meta$umfrage_item != "Zusatzfragen von anderen Kantonen/Schulen, ignorieren",
]

# Loop over each variable i in variable_meta and create a list
variables_list <- setNames(
  lapply(seq_len(nrow(variable_meta)), function(i) {
    # Select row i from the dataframe (first variable)
    # and convert single row into a named list
    row <- as.list(variable_meta[i, ])
    row$variable <- NULL

    # Get value label key
    key <- row$value_label_key
    row$value_label_key <- NULL

    # If the key exists, extract rows from value_meta
    if (!is.na(key)) {
      # Match key from variable file to value_meta file
      matching <- value_meta[value_meta$value_label_key == key, ]

      # Add value labels to the list
      row$values <-  setNames (
          as.list(matching$value_label),
          matching$value
      )
    } else {
      # Handle variables without value labels
      row$values <- NULL
    }
    # Filter out empty attributes
    row <- Filter(function(v) !(length(v) == 1 && is.na(v)), row)
    row
  }),

  # Use the variable name as name for each list element
  variable_meta$variable
)

# Create inst/metadata/ folder if it doesn't exist yet
dir.create("inst", recursive = TRUE)

# Convert R list into YAML format and save it
# Please do not overwrite the YAML again.
# Otherwise changes made using `add_meta` or `update_meta` may be lost.
# write_yaml(variables_list, "inst/qm_sekII_metadata.yaml", indent = 2)

