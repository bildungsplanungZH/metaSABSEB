# INTERNAL HELPERS ------------------------------------------------------------
# This script contains internal helper functions.
# They are not exported, but used within the functions of the package.
# Use the dropdown menu to skip to a specific function.

# check_var_exists() ----------------------------------------------------------

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
  if (var %in% names(meta)) {
    return(var)
  }

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
      "i" = "Meinten Sie vielleicht eine der folgenden Variablen:",
      "{.var {close_match}}?
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
      cli::cli_abort(
        "Das Anpassen der Metadaten wurde abgebrochen.
      Variable {.var {var}} konnte nicht in den Metadaten gefunden werden.",
        call = NULL
      )
    }

    # User chooses close match
    if (var_choice <= length(close_match)) {
      close_match[var_choice]
    }
  }
}

# confirm_replacement() -------------------------------------------------------

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
      cli::cli_alert_success(
        "Lokal gespeicherte Metadaten erfolgreich aktualisiert."
      )
      cli::cli_alert_info(paste0(
        "Für Änderungen am öffentlichen Metadatensatz ",
        "kann ein Pull Request erstellt werden."
      ))
    }

    # "No" = Exit, no change made
    if (replace_choice != 1) {
      abort_changes_msg()
    }
  }
}

# abort_changes_msg() ---------------------------------------------------------

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
    call = NULL
  )
}

#' Resolve the interactive mode for a metaSABSEB functions
#'
#' Internal helper that determines whether a function should prompt the
#' user for missing input. It is called once at the top of every function
#' that uses interactive elements such as `readline()` or `menu()`
#' (e.g. [add_meta()] and [change_meta()].
#'
#' The value is determined in the following order of priority:
#' 1. The explicitly passed `interactive_mode` argument
#' 2. The session setting `metaSABSEB.interactive_mode`, set by
#'    [interactive_mode_on()] and [interactive_mode_off()]
#' 3. `interactive()`
#'
#' Interactive mode cannot be forced in a non-interactive R session
#' (e.g. `Rscript`), because prompts cannot be answered there. In that case,
#' a warning is issued and `FALSE` is returned.
#'
#' @param interactive_mode Logical (`TRUE`/`FALSE`) or `NULL`. The value
#'   passed to the calling function. If `NULL`, the session option is used,
#'   falling back to `interactive()`.
#'
#' @return A single logical value: `TRUE` if the calling function may
#'   prompt the user, `FALSE` otherwise.
#' @keywords internal
#' @noRd

resolve_interactive_mode <- function(interactive_mode = NULL) {
  # Determine the requested mode
  # Priority: explicit argument > session option > interactive()
  if (is.null(interactive_mode)) {
    interactive_mode <- getOption(
      "metaSABSEB.interactive_mode",
      default = interactive()
    )
  }

  # Validate that it is a logical (TRUE or FALSE)
  if (
    !is.logical(interactive_mode) ||
      length(interactive_mode) != 1L ||
      is.na(interactive_mode)
  ) {
    # Abort with error if validation failed
    cli::cli_abort(
      "{.field interactive_mode} muss {.val TRUE} oder {.val FALSE} sein.",
      call = NULL
    )
  }

  # Prevent forcing interactive mode in a non-interactive session
  if (interactive_mode && !interactive()) {
    cli::cli_alert_warning(paste0(
      "{.strong Interaktiver Modus nicht verfügbar.}\n",
      "{.field interactive_mode} wurde auf {.val FALSE} gesetzt, ",
      "da die aktuelle R-Sitzung nicht interaktiv ist. \n\n",
      "Bitte übergeben Sie alle erforderlichen Argumente explizit."
    ))
    interactive_mode <- FALSE
  }

  # Return
  interactive_mode
}
