#' Get variable information from metadata
#'
#' @description
#' Retrieves metadata fields for a specified variable.
#' If no field is specified, all attributes are returned.
#'
#' @param var A character string specifying the variable name
#' @param field A character string specifying the field to retrieve
#'
#' @return A list of all fields if `field = NULL`,
#'          or a single field value if `field` is specified
#'
#' @examples
#' get_meta(var = "EFZ_Typ")
#' get_meta(var = "Anst_best", "quelle")
#'
#' @export

get_meta <- function(var, field = NULL) {
  meta <- yaml::read_yaml(system.file("qm_sekII_metadata.yaml",
                                      package = "metaSABSEB"))

  if (!var %in% names(meta)) {
    close_match <- names(meta)[tolower(names(meta)) == tolower(var)]

    if (length(close_match) > 0) {
      if (interactive()) {
        choice <- utils::menu(
          choices = c("Ja", "Nein"),
          title = paste0("Variable '", var,
                         "' nicht gefunden. Meinten Sie vielleicht '",
                         close_match[1], "'?")
        )
        if (choice == 1) {
          var <- close_match[1]
        } else {
          stop("Variable '", var,
               "' konnte nicht in den Metadaten gefunden werden.")
        }
      } else {
        stop(
          "Variable '", var,
          "' konnte nicht in den Metadaten gefunden werden. ",
          "Meinten Sie vielleicht '",
          close_match[1], "' ?"
        )
      }
    } else {
      stop("Variable '", var, "' konnte in den Metadaten nicht gefunden werden")
    }
  }

  info <- meta[[var]]
  if (!is.null(field)) {
    if (!field %in% names(info)) {
      stop("Field '", field, "' not found for variable '", var, "'.")
    }
    info[[field]]
  } else {
    info
  }
}
