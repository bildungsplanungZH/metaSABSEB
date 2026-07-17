#' @description
#' Add a new variable to the metadata
#' The function:
#' - creates a list-object with metadata
#' - ensures that required fields are complete,
#'   the following fields are required:
#'   - oberthema
#'   - umfrage_item
#'   - var_typ
#'   - status
#' - allows to add more fields to the list
#' - tries to ensure proper formatting
#' - updates current metadata yaml
#'
#' @param var Character string. Defines variable name. (required)
#' @param oberthema Character string. Defines topic of survey question (required)
#' @param unterthema Character string. (optional)
#' @param var_typ Character string. Describes type of survey question
#' (Einzelitem, Einzelitem-Gruppe, Skala) (required)
#' @param umfrage_item Character string. Defines survey question (required)
#' @param itemformulierung Character string. Further defines the survey
#' question. (required if var_typ = Einzelitem-Grupp)
#' @param skalen_berechnung Character string. Only required if var_typ == Skala
#' @param umfrage_filter Character string. Defines survey filters.
#' -> what is required for the item to be shown in the survey (optional)
#' @param quelle Character string. Source for construction of survey item (optional)
#' @param status Character string. Defines whether variable is still actively used
#' or deprecated
#' @param ... Character string. Allows to add more metadata entries.
#'
#' @export

add_meta <- function(var = NULL,
                     oberthema = NULL,
                     var_typ = NULL,
                     status = NULL,
                     umfrage_item = NULL,
                     itemformulierung = NULL,
                     skalenberechnung = NULL,
                     umfrage_filter = NULL,
                     quelle = NULL,
                     status = "aktiv",
                     ...) {


    # 1. Get current metadata --------------------------------------------------


    # Get path to metadata yaml
    yaml_path <- system.file(
        "qm_sekII_metadata.yaml",
        package = "metaSABSEB"
    )

    # Read yaml
    meta <- yaml::read_yaml(yaml_path)

    # 2. Ensure required fields are not empty ----------------------------------

    # 2.1 Variable name --------------------------------------------------------

    # -> if var is not specified, abort changes
    if (is.null(var)) {
        cli::cli_abort(paste0(
            "Variable nicht definiert.\n\n",
            "Das Argument {.field var} darf nicht leer sein.",
        ),
        call = NULL)
    }

    # -> if var already exists in meta
    #    abort and suggest change_meta()
    if (var %in% names(meta)) {
        cli::cli_abort(paste0(
            "Die Variable {.var {var}} existiert bereits in den Metadaten.\n\n",
            "Bitte nutzen Sie {.arg metaSABSEB::change_meta()} um Metadaten ",
            "von bereits existierenden Variablen anzupassen.",
            call = NULL)
        )
    }

    # 2.2. Oberthema -----------------------------------------------------------

    # if oberthema is empty
    if (is.null(oberthema)) {

        # Create warning message
        cli::cli_alert_warning("Kein Oberthema definiert.")

        # Get vector of current entries
        # -> no duplicates
        # -> empty entries filtered out
        current_entries <- unique(
            vapply(
                Filter(function(x) !is.null(x$oberthema)
                       && length(x$oberthema) > 0, meta),
                function(x) x$oberthema,
                character(1)
            )
        )

        # Create menu that asks user to
        # - either choose an already existing option
        # - or to choose a new topic
        oberthema_choice <- utils::menu(
            choices = c(current_entries, "Neues Oberthema"),
            title = cli::format_inline(
                "Bitte wählen Sie ein bestehendes Oberthema ",
                "oder erstellen Sie ein neues."
                )
            )

        # If menu is exited without making a choice
        # abort changes
        if (oberthema_choice == 0) {
            abort_changes_msg()
        }

        # If they choose an existing choice
        if (oberthema_choice <= length(current_entries)) {
            oberthema <- current_entries[oberthema_choice]
        }

        # If they want to set a new oberthema themselves
        else {
            # Force oberthema entry
            # Repeat until oberthema is not empty
            repeat {

                # Warning message
                cli::cli_alert_warning(
                    "Bitte definieren Sie ein neues Oberthema."
                    )

                # Interactive field to enter oberthema in console
                oberthema <- trimws(readline("oberthema: "))

                # Break repetition loop if oberthema is not empty anymore
                if (nzchar(oberthema)) break

                # Danger message, oberthema must not be empty
                cli::cli_alert_danger(
                    "Das Feld {.field oberthema} darf nicht leer sein."
                )
            }
        }



        # Now that oberthema should not be empty anymore
        # -> print info message on Oberthema
        if (length(oberthema) > 0 && !is.na(oberthema)) {
            # Return information banner
            cli::cli_alert_info(paste0(
                "Herzlichen Dank.\n\n",
                "Sie haben folgendes Oberthema für ",
                "die Variable {.var {var}} gesetzt. \n\n",
                "{.field oberthema: {oberthema}}"
                )
            )
        }
    }


    # 2.3 Variablentyp ---------------------------------------------------------
    if (is.null(var_typ)) {

        # Issue warning that var_type empty
        cli::cli_alert_warning("Kein Variablentyp definiert.")

        # Ask to specify one of three types
        typ_choice <- utils::menu(
            choices= c("Einzelitem", "Einzelitem-Gruppe", "Skala"),
            title = cli::format_inline(
                "Bitte wählen Sie einen der folgenden Typen"
            )
        )

        # If they exit the menu, abort changes
        if (typ_choice == 0) {
            abort_changes_msg()
        }

        # 2.3.1 Einzelitem -----------------------------------------------------
        # -> ask for title
        if (typ_choice == 1) {
            repeat {

                # Issue warning to specify
                cli::cli_alert_warning(paste0(
                    "Einzelitem genauer spezifizieren \n\n",
                    "Beschreiben sie das Einzelitem in Kürze"
                ))

                # Interactive field to specify
                description <- trimws(readline("Einzelitem: "))

                # Break repetition loop if description is not empty anymore
                if (nzchar(description))  break

                # Issue warning otherwise
                cli::cli_alert_danger("Einzelitem muss zwingend beschrieben werden.")
            }

            # Glue type and description together
            var_typ <- paste0("Einzelitem: ", description)
        }

        # 2.3.2 Einzelitem-Gruppe ----------------------------------------------
        # -> ask to choose existing group or create new group
        if (typ_choice == 2) {

            # Get current groups in metadata

            # -> extract var_typ
            var_types <- vapply(
                meta,
                function(x) if (is.null(x$var_typ)) NA_character_ else x$var_typ,
                character(1)
            )

            # -> extract "Einzelitem-Gruppe" within var_typ
            current_groups_raw <- unique(
                var_types[!is.na(var_types) & startsWith(var_types, "Einzelitem-Gruppe:")]
            )

            # -> remove prefix
            current_groups <- sub("^Einzelitem-Gruppe:\\s*", "", current_groups_raw)

            # Issue warning that group must be specified
            cli::cli_alert_warning("Einzelitem-Gruppe spezifizieren")

            # Ask user to either choose existing group
            # or define new group.
            gruppen_choice <- utils::menu(
                choices = c(current_groups, "Neue Gruppe"),
                title = cli::format_inline(
                    "Wählen Sie eine existierende Gruppe ",
                    "oder spezifizieren Sie eine neue Gruppe"
                )
            )
            if (gruppen_choice == 0) {
                abort_changes_msg()
            }

            if (gruppen_choice <= length(current_groups)) {
                var_typ <- paste0(
                    "Einzelitem-Gruppe: ",
                    current_groups[gruppen_choice]
                                  )
            }

            else {
                repeat {
                    # Warning
                    cli::cli_alert_warning(
                        "Bitte definieren Sie eine neue Gruppe"
                    )

                    # Interactive field to specify
                    description <- trimws(readline("Einzelitem-Gruppe: "))

                    # Break repetition loop if description is not empty anymore
                    if (nzchar(description))  break

                    # Issue warning otherwise
                    cli::cli_alert_danger(paste0(
                        "Gruppenname für Einzelitem-Gruppe muss ",
                        "zwingend definiert werden. "
                    ))
                }
                var_typ <- paste0("Einzelitem-Gruppe: ", description)
            }
        }

        # 2.3.3 Skala ----------------------------------------------------------
        if (typ_choice == 3) {

            # Get current scales in metadata

            # -> extract var_typ
            var_types <- vapply(
                meta,
                function(x) if (is.null(x$var_typ)) NA_character_ else x$var_typ,
                character(1)
            )

            # -> extract "Skala" within var_typ
            current_scales_raw <- unique(
                var_types[!is.na(var_types) & startsWith(var_types, "Skala:")]
            )

            # -> remove prefix
            current_scales <- sub("^Skala:\\s*", "", current_scales_raw)

            # Issue warning that group must be specified
            cli::cli_alert_warning("Skala spezifizieren")

            # Ask user to either choose existing group
            # or define new group.
            skala_choice <- utils::menu(
                choices = c(current_scales, "Neue Skala"),
                title = cli::format_inline(
                    "Wählen Sie eine existierende Skala ",
                    "oder spezifizieren Sie eine neue Skala"
                )
            )
            if (skala_choice == 0) {
                abort_changes_msg()
            }

            if (skala_choice <= length(current_scales)) {
                var_typ <- paste0(
                    "Skala: ",
                    current_scales[skala_choice]
                )
            }

            else {
                repeat {
                    # Warning
                    cli::cli_alert_warning(
                        "Bitte definieren Sie eine neue Skala"
                    )

                    # Interactive field to specify
                    description <- trimws(readline("Skala: "))

                    # Break repetition loop if description is not empty anymore
                    if (nzchar(description))  break

                    # Issue warning otherwise
                    cli::cli_alert_danger(
                        "Name für Skala muss zwingend definiert werden. "
                    )
                }
                var_typ <- paste0("Skala: ", description)
            }
        }
    }

    # 2.4 Umfrageitem ---------------------------------------------------------

    #

}
