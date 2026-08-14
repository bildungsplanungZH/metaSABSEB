#' @description
#' This function is used to delete metadata entries
#' May be used if an entry was wrongfully added
#'
#' @param var Characters string. Defines variable for which
#' the metadata should be deleted
#'
#' @export

delete_meta <- function(var = NULL) {

    # Require var to be specified
    if (is.null(var))  {
        cli::cli_abort(paste0(
            "{.strong Bitte spezifizieren Sie für welche Variable}",
            "{.strong die Metadaten gelöscht werden sollen.}"
        ))
    }

    # Get path to metadata yaml
    yaml_path <- system.file(
        "qm_sekII_metadata.yaml",
        package = "metaSABSEB"
    )

    # Read yaml
    meta <- yaml::read_yaml(yaml_path)

    # Check whether variable exists in meta
    check_var_exists(var=var, meta=meta)

    # Delete metadata entry
    meta[[var]] <- NULL

    # Write updated metadata back to YAML
    yaml::write_yaml(meta, yaml_path)

    # Update cache (used for get_meta())
    .meta_env$meta <- meta

    # Write confirmation message
    cli::cli_alert_success(paste0(
        "{.strong Metadaten für die Variable {.field {var}} gelöscht.} \n",
        "Lokal gespeicherte Metadaten erfolgreich aktualisiert.\n\n"
    ))
    cli::cli_alert_info(paste0(
        "Für Änderungen am öffentlichen Metadatensatz",
        "kann ein Pull Request erstellt werden."
    ))

}
