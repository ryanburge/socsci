
#' Percentage Labels for Bar Charts
#'
#' A shortcut for [ggplot2::geom_text()] that labels bars with the percentage
#' stored in a proportion column — typically the `pct` column returned by
#' [ct()]. Labels are dodged to match `geom_col(position = "dodge")`.
#'
#' @param type The proportion column to label (unquoted), e.g. `pct` from [ct()].
#'   Values are expected on the 0–1 scale.
#' @param pos Vertical offset for the label. When `above = TRUE` the label is
#'   drawn at `type + pos`; when `above = FALSE` it is drawn at `pos` itself
#'   (useful for placing labels at a fixed height inside the bars). Default `0`.
#' @param sz Text size. Default is `8`.
#' @param above Logical; if `TRUE` (default), place labels relative to the top
#'   of each bar. If `FALSE`, place them at the fixed height `pos`.
#' @param color Text color. Default is `"black"`; use `"white"` for labels
#'   drawn inside dark bars.
#' @param family Font family. Defaults to `""`, the ggplot2 default font.
#' @param digits Number of decimal places the proportion is rounded to before
#'   conversion to a percentage. The default `2` yields whole percentages.
#'
#' @return A ggplot2 layer that can be added to a plot with `+`.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' mtcars %>%
#'   ct(cyl) %>%
#'   ggplot(aes(x = factor(cyl), y = pct)) +
#'   geom_col() +
#'   lab_bar(pct, pos = 0.02)
#'
#' @export
#' @importFrom ggplot2 geom_text aes position_dodge
#' @importFrom rlang enquo
lab_bar <- function(type, pos = 0, sz = 8, above = TRUE, color = "black",
                    family = "", digits = 2) {
  type <- rlang::enquo(type)

  ggplot2::geom_text(
    ggplot2::aes(
      y = if (above) !!type + pos else pos,
      label = paste0(round(!!type, digits) * 100, "%")
    ),
    position = ggplot2::position_dodge(width = 0.9),
    size = sz,
    color = color,
    family = family
  )
}
