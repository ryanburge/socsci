#' Set the Default Font for socsci Plot Helpers
#'
#' Sets a session-wide default font family used by plotting helpers such as
#' [lab_bar()] and [add_text()]. By default, socsci uses the graphics device's
#' standard font. Font registration (for example with showtext) remains under
#' the user's control.
#'
#' @param family A character string naming an available font family. Use `""`
#'   to restore the graphics device default.
#'
#' @return The selected family, invisibly.
#' @export
#'
#' @examples
#' set_socsci_font("Arial")
#' getOption("socsci.font_family")
#' set_socsci_font()
set_socsci_font <- function(family = "") {
  if (!is.character(family) || length(family) != 1L || is.na(family)) {
    stop("`family` must be a single, non-missing character string.", call. = FALSE)
  }

  options(socsci.font_family = family)
  invisible(family)
}

socsci_font <- function(family = NULL) {
  if (is.null(family)) {
    family <- getOption("socsci.font_family", "")
  }

  if (!is.character(family) || length(family) != 1L || is.na(family)) {
    stop("`family` must be NULL or a single, non-missing character string.", call. = FALSE)
  }

  family
}
