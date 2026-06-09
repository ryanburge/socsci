
#' Percent-Formatted Axis Scales
#'
#' Shortcuts for continuous axis scales with percentage labels, for plotting
#' the 0–1 proportions returned by [ct()] and [mean_ci()].
#'
#' @param accuracy Number the percentage labels are rounded to, passed to
#'   [scales::percent_format()]. The default `1` gives whole percentages.
#' @param ... Additional arguments passed on to
#'   [ggplot2::scale_x_continuous()] or [ggplot2::scale_y_continuous()]
#'   (e.g. `limits`, `breaks`, `expand`).
#'
#' @return A ggplot2 scale that can be added to a plot with `+`.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' mtcars %>%
#'   ct(cyl) %>%
#'   ggplot(aes(x = factor(cyl), y = pct)) +
#'   geom_col() +
#'   y_pct()
#'
#' @export
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
x_pct <- function(accuracy = 1, ...) {
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = accuracy), ...)
}

#' @rdname x_pct
#' @export
y_pct <- function(accuracy = 1, ...) {
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = accuracy), ...)
}
