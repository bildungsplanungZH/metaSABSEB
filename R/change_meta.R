#' @description
#' Change metadata for a variable that is already registered
#' The function:
#' - reads the metadata yaml
#' - locates the desired variable and field
#' - shows the current metadata entry
#' - asks for confirmation to change it
#' - changes the metadata yaml on disk
#'
#' @param var A character string specifying the variable name
#' @param field A character string specifying the metadata field
#' to be changed
#' @param new_entry String or numeric, new value for the metadata field
#' @param yaml_path Character string specifying the path to the metadata YAML
#' file. By default, the package's metadata YAML file is used. This argument
#' can be changed, e.g. for testing with a temporary YAML file.
#'
#' @export

change_meta <- function(var = NULL,
                        field = NULL,
                        new_entry = NULL,
                        yaml_path = system.file(
                            "qm_sekII_metadata.yaml",
                            package = "metaSABSEB")
                        ) {

    # 1. Enforce no missing args ----------------------------------------------
    if (is.null(var)) {
        cli::cli_abort("Das Argument {.field var} muss mitgegeben werden.",
                       call = NULL)
    }
    if (is.null(field)) {
        cli::cli_abort("Das Argument {.field field} muss mitgegeben werden.",
                       call = NULL)
    }
    if (is.null(new_entry)) {
        cli::cli_abort(paste0(
            "Das Argument {.field new_entry} ",
            "muss mitgegeben werden."
        ), call = NULL)
    }

    # 2. Get metadata from disk -----------------------------------------------
    meta <- yaml::read_yaml(yaml_path)

    # 3 Check variable existence ----------------------------------------------

    # Check if the variable already is in the metadata.
    # -> use internal helper function from helpers.R
    var <- check_var_exists(var, meta)

    # Retrieve metadata for the requested variable and save in vector
    var_info <- meta[[var]]

    # 4 Check field existence and replace field -------------------------------
    # If field exists for specified variable
    # Confirm that the field should be replaced
    # -> use internal helper function from helpers.R
    if(field %in% names(var_info)) {
        confirm_replacement(yaml_path, meta, var, field, new_entry)
    }

    # If field does not exist yet
    else {

        # Ask user if they meant a different, already existing field
        if (interactive()) {

            # Create choice menu
            field_choice <- utils::menu(
                choices = c("Neues Feld hinzufügen.",
                            names(var_info)),
                title = cli::format_inline(paste0(
                    "Das Feld {.field {field}} existiert nicht für die ",
                    "Variable {.var {var}}.\n Wählen Sie ein bestehendes ",
                    "Feld aus oder fügen Sie es neu hinzu:"
                ))
            )
            # If the user cancelled the menu
            if (field_choice==0) {
                abort_changes_msg()
            }

            # If user wants to add a new field
            if (field_choice==1){

                # Add new entry to meta
                meta[[var]][[field]] <- new_entry

                # Overwrite yaml on disk
                yaml::write_yaml(meta, yaml_path)

                # Update cache (used for get_meta())
                .meta_env$meta <- meta

                # Return message
                cli::cli_alert_success(
                    "Lokal gespeicherte Metadaten erfolgreich aktualisiert."
                    )
                cli::cli_alert_info(paste0(
                    "Für Änderungen am öffentlichen Metadatensatz ",
                    "kann ein Pull Request erstellt werden."
                ))
                cat("\n")
            }

            # If user meant an existing field
            if (field_choice>=2){

                # Retrieve field name
                field <- names(var_info)[field_choice-1]

                # Confirm that the chosen field should be replaced
                # and overwrite meta
                # -> use internal helper function from helpers.R
                confirm_replacement(yaml_path, meta, var, field, new_entry)
            }
        }

        # If not interactive, just add new field
        else{
            # Add new entry to meta
            meta[[var]][[field]] <- new_entry

            # Overwrite yaml on disk
            yaml::write_yaml(meta, yaml_path)

            # Update cache (used for get_meta())
            .meta_env$meta <- meta

            # Return message
            cli::cli_alert_success(
                "Lokal gespeicherte Metadaten erfolgreich aktualisiert."
            )
            cli::cli_alert_info(paste0(
                "Für Änderungen am öffentlichen Metadatensatz ",
                "kann ein Pull Request erstellt werden."
            ))
            cat("\n")
        }
    }
}
