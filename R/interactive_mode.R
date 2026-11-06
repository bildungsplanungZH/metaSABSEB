#' Turn interactive mode in metaSABSEB ON
#'
#' Activates interactive use by setting the default for the
#' `interactive_mode` argument to `TRUE`
#' [add_meta], [change_meta], and [delete_meta] for the rest
#' of the R-Session (or until changed). An explicitly passed
#' `interactive_mode` argument always takes precedence
#'
#' @return Logical. TRUE
#'
#' @export

interactive_mode_on <- function() {
  options(metaSABSEB.interactive_mode = TRUE)
  cli::cli_alert_info("Interactive mode is now {.emph enabled}.")
  invisible(TRUE)
}


#' Turn interactive mode in metaSABSEB OFF
#'
#' Activates interactive use by setting the default for the
#' `interactive_mode` argument to `FALSE`
#' [add_meta], [change_meta], and [delete_meta] for the rest
#' of the R-Session (or until changed). An explicitly passed
#' `interactive_mode` argument always takes precedence
#'
#' @return Logical. FALSE
#'
#' @export

interactive_mode_off <- function() {
  options(metaSABSEB.interactive_mode = FALSE)
  cli::cli_alert_info("Interactive mode is now {.emph disabled}.")
  invisible(FALSE)
}
