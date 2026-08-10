#' @description
#' Add a new variable to the metadata
#' The function:
#' - creates a list-object with metadata
#' - ensures that required fields are complete,
#' - allows to add more fields to the list
#' - tries to ensure proper formatting and valid entries
#' - updates current metadata yaml
#'
#' @param var Character string. Defines variable name.
#' No empty spaces, snake case (always required)
#' @param oberthema Character string. Defines topic of survey question (always required)
#' @param unterthema Character string. (optional)
#' @param var_typ Character string. Describes type of survey question
#' (Einzelitem, Item-Gruppe, Skala) (always required)
#' @param umfrage_item Character string. Defines survey question (always required)
#' @param itemformulierung Character string. Further defines the survey
#' question. (required if var_typ = Item-Gruppe)
#' @param skalenberechnung Character string. Only required if var_typ == Skala
#' @param umfrage_filter Character string. Defines survey filters.
#' -> what is required for the item to be shown in the survey (optional)
#' @param quelle Character string. Source for construction of survey item
#' (optional)
#' @param status Character string. Defines whether variable is still actively used
#' or deprecated. Default is "aktiv"
#' @param values List-Object. Lists encoding of all values. (always required)
#' @param ... Character string. Allows to add more metadata entries.
#' Must be a named argument.
#'
#' @export

add_meta <- function(var = NULL,
                     oberthema = NULL,
                     unterthema = NULL,
                     var_typ = NULL,
                     umfrage_item = NULL,
                     itemformulierung = NULL,
                     skalenberechnung = NULL,
                     umfrage_filter = NULL,
                     quelle = NULL,
                     status = "aktiv",
                     values = NULL,
                     ...) {

    # 1 Get current metadata --------------------------------------------------

    # Get path to metadata yaml
    yaml_path <- system.file(
        "qm_sekII_metadata.yaml",
        package = "metaSABSEB"
    )

    # Read yaml
    meta <- yaml::read_yaml(yaml_path)

    # 2 Ensure required fields are not empty ----------------------------------

    # -> First handle the non interactive case.
    #    This is used as a fallback for non-interactive R-Sessions that do not
    #    allow inputs directly from the terminal.

    if (!interactive()) {

        # Ensure that required arguments are complete. Otherwise, abort

        # Arguments that are always required
        missing_required <-
            is.null(var) || !is.character(var) || length(var) != 1L ||
            is.na(var) || var == "" ||
            is.null(oberthema) || is.null(var_typ) || is.null(umfrage_item) ||
            is.null(values)

        # Itemformulierung (only required if var_typ is Item-Gruppe/Skalenitem)
        needs_itemformulierung <-
            !is.null(var_typ) &&
            (stringr::str_detect(var_typ, "Item-Gruppe:") ||
                 stringr::str_detect(var_typ, "Skalenitem:")) &&
            is.null(itemformulierung)

        # Skalenberechnung (only required if var_typ is a skala)
        needs_skalenberechnung <-
            !is.null(var_typ) &&
            stringr::str_detect(var_typ, "Skalenitem:") &&
            is.null(skalen_berechnung)

        # Abort if any of the conditions are not satisfied
        if (missing_required ||
            needs_itemformulierung ||
            needs_skalenberechnung) {
            cli::cli_abort(c(
                "{.strong Nicht alle obligatorischen Argumente definiert.}\n\n",
                "In einer nicht-interaktiven Sitzung müssen alle ",
                "erforderlichen Argumente explizit übergeben werden:\n",
                "- {.field var}\n",
                "- {.field oberthema}\n",
                "- {.field var_typ}\n",
                "- {.field umfrage_item}\n",
                if (needs_itemformulierung) "- {.field itemformulierung}\n",
                if (needs_skalenberechnung) "- {.field skalen_berechnung}\n"
            ), call = NULL)
        }
    }

    # Otherwise if it is interactive:
    # Use console inputs and suggestions to complete the metadata entry
    else {

        # 2.1 Variable name ---------------------------------------------------

        # -> if var is not specified, abort changes
        if (
            is.null(var) ||
            !is.character(var) ||
            length(var) != 1L ||
            is.na(var) ||
            var == ""
        ) {
            cli::cli_abort(c(
                "{.strong Variable nicht definiert.} ",
                "Das Argument {.field var} darf nicht leer sein.\n\n"
            ), call = NULL)
        }

        # -> make sure that var does not have whitespaces
        original <- var
        var <- trimws(var) # remove leading and trailing whitespace
        var <- gsub("\\s+", "_", var) # replace whitespaces with underscore
        var <- gsub("_+", "_", var) # avoid double underscores
        if (!identical(original, var)) { # issue message
            cli::cli_alert_info(c(
                "{.strong Die Variablenbezeichnung darf keine Leerschläge ",
                "enthalten.}\n ",
                "Variablenname wurde automatisch geändert zu {.field {var}}."
            ))
            cat("\n")
        }

        # -> if var already exists in meta
        #    abort and suggest change_meta()
        if (var %in% names(meta)) {
            cli::cli_abort(c(
                "{.strong Die Variable {.field {var}}} ",
                "{.strong ist bereits in den Metadaten erfasst.} \n\n",
                "Bitte nutzen Sie {.fn metaSABSEB::change_meta()} um die ",
                "Metadaten von bereits existierenden Variablen anzupassen.\n\n"
            ), call = NULL
            )
        }

        # 2.2. Oberthema ------------------------------------------------------

        # if oberthema is empty, ask user to
        # - either choose an already existing option
        # - or to create a new topic
        if (is.null(oberthema)) {

            # Create warning message
            cli::cli_alert_warning("{.strong Kein Oberthema definiert.}")

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

            # Create menu that asks the user interactively
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

            # If they choose an existing topic
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
                    cat("\n")
                }
            }


            # Now that oberthema should not be empty anymore
            # -> print info message on Oberthema
            if (length(oberthema) > 0 && !is.na(oberthema)) {
                # Return information banner
                cli::cli_alert_info(c(
                    "Herzlichen Dank. Sie haben folgendes Oberthema für ",
                    "die Variable {.code {var}} gesetzt. \n",
                    "  {.field oberthema:} {.val {oberthema}} \n\n"
                )
                )
                cat("\n")
            }
        }


        # 2.3 Variablentyp ----------------------------------------------------
        if (is.null(var_typ)) {

            # Issue warning that var_type empty
            cli::cli_alert_warning("{.strong Kein Variablentyp definiert.}")

            # Ask to specify one of three types
            typ_choice <- utils::menu(
                choices= c(
                    "Einzelitem  - für einzelne, in sich geschlossene Items",
                    paste0(
                        "Item-Gruppe - für Items, die konzeptuell verbunden ",
                        "sind, jedoch nicht zu einer Skala gehören ",
                        "(z.B. Multiple-Choice Antworten)"
                    ),
                    "Skalenitem - für Items, die einer Skala angehören"
                ),
                title = cli::format_inline(
                    "Bitte wählen Sie einen der folgenden Typen"
                )
            )

            # If they exit the menu, abort changes
            if (typ_choice == 0) {
                abort_changes_msg()
            }

            # 2.3.1 Einzelitem ------------------------------------------------
            # -> ask for description
            if (typ_choice == 1) {
                # Repeat until description has been entered into console
                repeat {

                    # Issue warning to specify
                    cli::cli_alert_warning(c(
                        "{.strong Einzelitem genauer spezifizieren.} \n",
                        "Beschreiben sie das Einzelitem in Kürze"
                    ))

                    # Interactive field to specify
                    description <- trimws(readline("Einzelitem: "))

                    # Break repetition loop if description is not empty anymore
                    if (nzchar(description))  break

                    # Issue warning otherwise
                    cli::cli_alert_danger(
                        "Einzelitem muss zwingend beschrieben werden."
                    )
                }

                # Glue type and description together
                var_typ <- paste0("Einzelitem: ", description)
            }

            # 2.3.2 Item-Gruppe -----------------------------------------------
            # -> ask to choose existing group or create new group
            if (typ_choice == 2) {

                # Get current groups in metadata
                # -> extract var_typ
                var_types <- vapply(
                    meta,
                    function(x)
                        if (is.null(x$var_typ)) NA_character_
                    else x$var_typ,
                    character(1)
                )

                # -> extract "Item-Gruppe" within var_typ
                current_groups_raw <- unique(
                    var_types[!is.na(var_types) &
                                  startsWith(var_types, "Item-Gruppe:")]
                )

                # -> remove prefix
                current_groups <- sub("^Item-Gruppe:\\s*", "",
                                      current_groups_raw)

                # Issue warning that group must be specified
                cli::cli_alert_warning("{.strong Item-Gruppe spezifizieren}")

                # Ask user to either choose existing group
                # or define new group.
                group_choice <- utils::menu(
                    choices = c(current_groups, "Neue Gruppe"),
                    title = cli::format_inline(
                        "Wählen Sie eine existierende Gruppe ",
                        "oder spezifizieren Sie eine neue Gruppe"
                    )
                )
                # abort if no choice was made
                if (group_choice == 0) {
                    abort_changes_msg()
                }

                # if existing group was chosen, add to var_typ
                if (group_choice <= length(current_groups)) {

                    # Create string for var_typ
                    var_typ <- paste0(
                        "Item-Gruppe: ",
                        current_groups[group_choice]
                    )

                    # Note: For each Item-Gruppe,
                    # the umfrage_item is the same.
                    # Hence, if they choose a pre-existing one.
                    # This field can automatically be filled.

                    # Get original scale name to look up in meta
                    selected_group_raw <- current_groups_raw[group_choice]

                    # Filter first variable where var_typ matches chosen scale
                    matching_meta <- Filter(
                        function(x) identical(x$var_typ, selected_group_raw),
                        meta
                    )

                    # Get item description from that variable
                    umfrage_item <- matching_meta[[1]]$umfrage_item

                    # Print info message
                    cli::cli_alert_info(c(
                        "Die Beschreibung des Umfrage-Items wurde automatisch ",
                        "anhand der ausgewählten Item-Gruppe übernommen.\n\n",
                        "{.strong {selected_group_raw}} \n",
                        "{.field {umfrage_item}}"

                    ))
                    cat("\n")

                }

                # if user wants to define new group, enforce entry
                else {
                    repeat {
                        # Warning
                        cli::cli_alert_warning(
                            "Bitte definieren Sie eine neue Gruppe"
                        )

                        # Interactive field to specify
                        description <- trimws(readline("Item-Gruppe: "))

                        # Break if description is not empty anymore
                        if (nzchar(description))  break

                        # Issue warning otherwise
                        cli::cli_alert_danger(c(
                            "Gruppenname für Item-Gruppe muss ",
                            "zwingend definiert werden. "
                        ))
                    }
                    # If new group was defined, add to var_typ
                    var_typ <- paste0("Item-Gruppe: ", description)
                }
            }


            # 2.3.3 Skalenitem ------------------------------------------------
            if (typ_choice == 3) {

                # Get current scales in metadata

                # -> extract var_typ
                var_types <- vapply(
                    meta,
                    function(x)
                        if (is.null(x$var_typ)) NA_character_ else x$var_typ,
                    character(1)
                )

                # -> extract "Skalenitem" within var_typ
                current_scales_raw <- unique(
                    var_types[!is.na(var_types) & startsWith(var_types, "Skalenitem:")]
                )

                # -> remove prefix
                current_scales <- sub("^Skalenitem:\\s*", "", current_scales_raw)

                # Issue warning that group must be specified
                cli::cli_alert_warning(c(
                    "{.strong Skala spezifizieren}\n ",
                    "{.emph Zu welcher Skala gehört das Skalenitem?}"
                ))

                # Ask user to either choose existing group
                # or define new group.
                scale_choice <- utils::menu(
                    choices = c(current_scales, "Neue Skala"),
                    title = cli::format_inline(
                        "Wählen Sie eine existierende Skala ",
                        "oder spezifizieren Sie den Namen ",
                        "einer neuen Skala."
                    )
                )

                # Abort if no choice was made
                if (scale_choice == 0) {
                    abort_changes_msg()
                }

                # If they choose a pre-existing option
                if (scale_choice <= length(current_scales)) {
                    var_typ <- paste0(
                        "Skalenitem: ",
                        current_scales[scale_choice]
                    )

                    # Note: For each Skala, the itemformulierung
                    # and the skalenberechnung is the same.
                    # Hence, if they choose a pre-existing one.
                    # This can automatically be filled.

                    # Get original scale name to look up in meta
                    selected_scale_raw <- current_scales_raw[scale_choice]

                    # Filter out first variable where var_typ matches chosen scale
                    matching_meta <- Filter(
                        function(x) identical(x$var_typ, selected_scale_raw),
                        meta
                    )

                    # Get item description from that variable
                    umfrage_item <- matching_meta[[1]]$umfrage_item

                    # Get skalenberechnung
                    skalenberechnung <-matching_meta[[1]]$skalenberechnung

                    # Print info message
                    cli::cli_alert_info(c(
                        "Die Beschreibung des Umfrage-Items sowie die Angaben ",
                        "zur Berechnung der Skala wurden automatisch ",
                        "anhand der ausgewählten Skala übernommen.\n\n",
                        "{.strong {selected_scale_raw}} \n",
                        "Umfrage-Item: {.field {umfrage_item}}\n",
                        "Skalenberechnung: {skalenberechnung}"

                    ))
                    cat("\n")
                }

                # If they want to define a new Skala
                else {

                    # Repeat until Skala is defined
                    repeat {
                        # Warning
                        cli::cli_alert_warning(
                            "Bitte definieren Sie den Namen der neuen Skala"
                        )

                        # Interactive field to specify
                        description <- trimws(readline("Skala: "))

                        # Break repetition loop if description is not empty anymore
                        if (nzchar(description))  break

                        # Issue warning otherwise
                        cli::cli_alert_danger(
                            "Name der Skala muss zwingend definiert werden. "
                        )
                    }
                    var_typ <- paste0("Skalenitem: ", description)
                }
            }
        }

        # 2.4 Umfrageitem ---------------------------------------------------------
        # Ask user to define survey question
        if (is.null(umfrage_item)) {
            # Repeat until description has been entered into console
            repeat {

                # Issue warning to specify
                cli::cli_alert_warning(c(
                    "{.strong Umfrage-Item definieren}\n\n",
                    "Beschreiben sie den Wortlaut des Umfrage-Items \n",
                    "{.emph (Welche Frage wurde gestellt?)}"
                )
                )

                # Interactive field to specify
                umfrage_item <- trimws(readline("Umfrage-Item: "))

                # Break repetition loop if umfrage_item is not empty anymore
                if (nzchar(umfrage_item))  break

                # Issue warning otherwise
                cli::cli_alert_danger(
                    "Umfrage-item muss zwingend beschrieben werden."
                )
            }
        }

        # 2.5  Itemformulierung ---------------------------------------------------
        # Note: This field is only mandatory if var_typ is
        # either "Item-Gruppe" or "Skalenitem"
        if ((stringr::str_detect(var_typ, "Item-Gruppe:")
             ||  stringr::str_detect(var_typ, "Skalenitem:"))
            && is.null(itemformulierung)) {

            # Repeat until itemformulierung is provided
            repeat {
                # Issue warning to specify
                cli::cli_alert_warning(c(
                    "{.strong Itemformulierung definieren}\n\n",
                    "Geben Sie die konkrete Formulierung des Items an. \n",
                    "{.emph (Welche Unterfrage, Ausprägung oder ",
                    "Antwortoption gehört zum Item?)}"
                ))

                # Interactive field to specify
                itemformulierung <- trimws(readline("Itemformulierung: "))

                # Break repetition loop if umfrage_item is not empty anymore
                if (nzchar(itemformulierung))  break

                # Issue warning otherwise
                cli::cli_alert_danger(
                    "Itemformulierung muss zwingend definiert werden."
                )
            }
        }

        # 2.6 Skalenberechnung-----------------------------------------------------
        # Note: This field is only mandatory if var_typ = Skalenitem
        if (stringr::str_detect(var_typ, "Skalenitem:")
            && is.null(skalenberechnung)) {

            # Repeat until skalenberechnung is specified
            repeat {
                cli::cli_alert_warning(c(
                    "{.strong Skalenberechnung definieren}\n\n",
                    "Geben Sie an, wie die Skala, die zu diesem Item gehört,",
                    "berechnet wird. \n",
                    "{.emph (z.B. Mean, Ausschluss wenn Missings >1)}"
                ))

                #Interactive field to specify
                skalenberechnung <- trimws(readline("Skalenberechnung: "))

                # Break loop if skalenberechnung is not empty anymore
                if (nzchar(skalenberechnung)) break

                # Issue warning otherwise
                cli::cli_alert_danger(
                    "Skalenberechnung muss zwingend definiert werden"
                )
            }
        }

        # 2.7 Values ----------------------------------------------------------
        if (is.null(values)){


        }

    }



    # 4 Add list-object to metadata -------------------------------------------
}
