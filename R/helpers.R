#' Check variable existence in metadata
#'
#' Checks if a variable name exists in the metadata list.
#' If not, searches for close matches and asks the user interactively
#' to confirm if they meant a similar variable name.
#'
#' @param var A character string specifying the variable name to look up.
#' @param meta A named list as returned by [yaml::read_yaml()].
#'
#' @return A character string with the (possibly corrected) variable name.
#'   Aborts if no match is found or the user cancels.
#'
#' @keywords internal
#' @noRd
check_var_exists <- function(var, meta) {

    if (var %in% names(meta)) return(var)

    close_match <- agrep(
        pattern = var,
        x = names(meta),
        ignore.case = TRUE,
        value = TRUE,
        max.distance = 0.2
    )

    # No close match found
    if (length(close_match) == 0) {
        cli::cli_abort(
            "Variable {.var {var}} konnte nicht in den Metadaten gefunden werden.",
            call = NULL
        )
    }


    # Close match found, but non-interactive
    if (!interactive()) {
        cli::cli_abort(c(
            "Variable {.var {var}} konnte nicht in den Metadaten gefunden werden.",
            "i" = "Meinten Sie vielleicht eine der folgenden Variablen: {.var {close_match}}?
            Bitte versuchen Sie es erneut."
        ), call = NULL)
    }

    # Close match found, interactive
    if (interactive()) {
        var_choice <- utils::menu(
            choices = c(close_match, "Keine der oben genannten"),
            title = cli::format_inline(
                "Variable {.var {var}} nicht gefunden. Meinten Sie vielleicht?"
            )
        )

        # User does not take any suggestions or aborts
        if (var_choice == 0 || var_choice == length(close_match) + 1) {
            cli::cli_abort("Das Anpassen der Metadaten wurde abgebrochen.
                               Variable {.var {var}} konnte nicht in den Metadaten gefunden werden.",
                           call = NULL)

        }

        # User chooses close match
        if (var_choice <= length(close_match)) {
            return(close_match[var_choice])
        }
    }
}

#' Confirm that an existing entry should be replaced
#' Compares current metadata entry to new entry
#' Asks interactively, if the entry should be replaced
#' Replaces entry on disk or
#'
#' @param yaml_path Path to metadata
#' @param meta A named list as returned by [yaml::read_yaml()].
#' @param var A character string specifying the variable that should be changed
#' @param field A character string specyifing the field
#' that should be changed for the variable defined in `var`
#' @param new_entry String or numeric:
#' Metadata entry that should be used to replace old entry.
#'
#' @return description
#'
#'
confirm_replacement <- function(yaml_path, meta, var, field, new_entry) {

    # If not interactive just replace
    if (!interactive()) {
        # Add new entry to meta
        meta[[var]][[field]] <- new_entry

        # Overwrite yaml on disk
        yaml::write_yaml(meta, yaml_path)

        # Update cache (used for get_meta())
        .meta_env$meta <- meta
    }

    # If interactive ask if replacing is correct
    if (interactive()) {
        # Get current entry
        current_entry <- meta[[var]][[field]]

        # Ask if current entry should be replaced
        replace_choice <- utils::menu(
            choices = c("Ja", "Nein"),
            title = cli::format_inline(
                "Sind Sie sicher, dass Sie den aktuellen Eintrag ",
                "für die Variable {.var {var}} ersetzen möchten?\n\n",
                "  Aktuell:  {.field {field}}: {.val {current_entry}}\n",
                "  Neu:      {.field {field}}: {.val {new_entry}}"
            )
        )


        # "Yes" = Replace current entry
        if (replace_choice == 1) {

            # Add new entry to meta
            meta[[var]][[field]] <- new_entry

            # Overwrite yaml on disk
            yaml::write_yaml(meta, yaml_path)

            # Update cache (used for get_meta())
            .meta_env$meta <- meta

            # Return message
            cli::cli_alert_success("Lokal gespeicherte Metadaten erfolgreich aktualisiert.")
            cli::cli_alert_info(paste0(
                "Für Änderungen am öffentlichen Metadatensatz ",
                "kann ein Pull Request erstellt werden."
            ))
        }

        # "No" = Exit, no change made
        if (replace_choice !=1) {
            abort_changes_msg()
        }
    }
}

#' Error message, for when menu has been exited
#' or changes have been aborted
#'
#' @return cli messages:
#' - changing metadata has been aborted
#' - no changes made
#'
#' @keywords internal

abort_changes_msg <- function() {
    cli::cli_abort(
        c(
        "Das Anpassen der Metadaten wurde abgebrochen.",
        "i" = "Es wurden keine Änderungen vorgenommen."
        ),
        call = NULL)
}
