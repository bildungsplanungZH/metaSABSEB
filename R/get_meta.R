#' Retrieve metadata fields
#'
#' @param var A character string. The variable name. Cannot be empty
#' and must exist in metadata. If close matches are found, suggestions are
#' offerd in the console.
#'
#' @param field A character string. The metadata field to retrieve.
#' If no `field` or the field does not exist for `var`is defined, all metadata
#' for `var` is returned.
#'
#' @param interactive_mode Whether to enable interactive console prompts. If
#' not passed explicitly defaults to:
#' 1. Session Setting: `metaSABSEB.interactive_mode`, set by
#'    [interactive_mode_on()] and [interactive_mode_off()]
#' 2. `interactive()
#'
#' @param refresh Logical. If TRUE, the metadata cache is reloaded from
#' `yaml_path`. Default is FALSE.
#'
#' @param yaml_path Character string specifying the path to the metadata YAML.
#' Used if `refresh = TRUE` or if the metadata cache is empty. Defaults to
#' the file installed with the package; can be changed e.g. for testing with
#' a temporary file
#'
#' @return If `field = NULL`, a named list containing all metadata
#' associated with `var`. Otherwise the value of the requested
#' metadata field
#'
#' @examples
#' # All metadata for a variable
#' get_meta(var = "EFZ_Typ")
#'
#' # Only a specific metadata field
#' get_meta(var = "Anst_best", field = "quelle")
#'
#' @export
get_meta <- function(var,
                     field = NULL,
                     interactive_mode = NULL,
                     refresh = FALSE,
                     yaml_path = system.file(
                       "qm_sekII_metadata.yaml",
                       package = "metaSABSEB"
                     )) {
  # --------------------------------
  # 1. Setup
  # --------------------------------

  # Resolve interactive mode
  interactive_mode <- resolve_interactive_mode(interactive_mode)

  # Update cache if it is empty or refresh = TRUE
  # Note: metadata is cached for better performance
  if (is.null(.meta_env$meta) || refresh) {
    .meta_env$meta <- yaml::read_yaml(yaml_path)
  }

  # Get metadata from cache
  meta <- .meta_env$meta

  # --------------------------------
  # 2. Check variable existence
  # --------------------------------

  # Check if the variable already is in the metadata.
  # -> use internal helper function from helpers.R
  var <- check_var_exists(var, meta)

  # Retrieve metadata for the requested variable and save in vector
  var_info <- meta[[var]]

  # --------------------------------
  # 3. Check field (if specified)
  # --------------------------------

  # If a field is specified
  if (!is.null(field)) {
    # and it exists for that var
    if (field %in% names(var_info)) {
      # Condense info returned to field info
      return(var_info[[field]])
    }

    # If a field is specified but it does not exist for that var
    if (!(field %in% names(var_info))) {
      cli::cli_alert_warning(paste0(
        "Es existiert kein Metadateneintrag für das Feld ",
        "{.field {field}}. \n\n",
        "Es werden alle verfügbaren Einträge für die ",
        "Variable {.var {var}} aufgelistet."
      ))
      var_info
    }
  } else {
    var_info
  }
}
