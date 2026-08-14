#' Add Text to a Plot
#'
#' A compact wrapper around [ggplot2::annotate()] for adding a text annotation
#' at fixed plot coordinates.
#'
#' @param x,y Coordinates at which to place the text.
#' @param word Text to display.
#' @param sz Text size.
#' @param color Text color.
#' @param family Font family. The default, `NULL`, uses the value set by
#'   [set_socsci_font()] or the graphics device default when no option is set.
#' @param ... Additional arguments passed to [ggplot2::annotate()].
#'
#' @return A ggplot2 annotation layer.
#' @export
#' @importFrom ggplot2 annotate
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   add_text(4, 30, "Example label")
add_text <- function(x, y, word, sz = 5, color = "black",
                     family = NULL, ...) {
  family <- socsci_font(family)

  ggplot2::annotate(
    "text",
    x = x,
    y = y,
    label = word,
    size = sz,
    color = color,
    family = family,
    ...
  )
}
