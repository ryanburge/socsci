
#' Error Bars for mean_ci() Output
#'
#' A shortcut for [ggplot2::geom_errorbar()] that maps the `lower` and `upper`
#' columns produced by [mean_ci()], with dodging that matches a standard
#' dodged bar chart.
#'
#' @param wd Width of the error bar caps. Default is `0.2`.
#'
#' @return A ggplot2 layer that can be added to a plot with `+`.
#'
#' @details
#' The data must contain columns named `lower` and `upper`, as returned by
#' [mean_ci()]. The layer uses `position_dodge(0.9)` so the bars line up with
#' `geom_col(position = "dodge")` defaults.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   mean_ci(mpg) %>%
#'   ggplot(aes(x = factor(cyl), y = mean)) +
#'   geom_col() +
#'   error_bar()
#'
#' @export
#' @importFrom ggplot2 geom_errorbar aes position_dodge
#' @importFrom rlang .data
error_bar <- function(wd = 0.2) {
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
    width = wd,
    position = ggplot2::position_dodge(0.9)
  )
}
