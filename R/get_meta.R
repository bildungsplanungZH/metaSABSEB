#' @description
#' Retrieves metadata fields for a specified variable.
#' If no field is specified, all metadata is returned.
#'
#' @param var A character string specifying the variable name
#' @param field A character string specifying the field to retrieve
#' @param refresh Logical. If TRUE,
#' the metadata cache is reloaded from disk.
#' Use this if the underlying metadata file has changed
#' (e.g. during development or after updating the package).
#' Default is FALSE.
#'
#' @return If `field = NULL`, a named list containing all metadata
#' associated with `var`. Otherwise the value of the requested
#' metadata field
#'
#' @examples
#' get_meta(var = "EFZ_Typ")
#' get_meta(var = "Anst_best", "quelle")
#'
#' @export
get_meta <- function(var, field = NULL, refresh = FALSE) {

    # --------------------------------
    # 1. Get metadata
    # --------------------------------

    # Set file path
    file <- system.file("qm_sekII_metadata.yaml", package = "metaSABSEB")

    # Update cache if it is empty or refresh = TRUE
    # Note: metadata is cached for better performance
    if (is.null(.meta_env$meta) || refresh) {
        .meta_env$meta <- yaml::read_yaml(file)
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
        if(field %in% names(var_info)) {

            # Condense info returned to field info
            return(var_info[[field]])
            }

        # If a field is specified but it does not exist for that var
        else {
            cli::cli_alert_warning(paste0(
                "Es existiert kein Metadateneintrag für das Feld ",
                "{.field {field}}. \n\n",
                "Es werden alle verfügbaren Einträge für die ",
                "Variable {.var {var}} aufgelistet."
            ))
            return(var_info)
        }}

    # If no field is specified return all medadata for the variable
    else {return(var_info)}
}
