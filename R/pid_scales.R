
#' Party Identification and Ideology Color Scales
#'
#' Manual fill and color scales for ordered party identification and ideology
#' factors, such as those created with [frcode()]. All palettes run from the
#' Democratic/liberal end (blue) to the Republican/conservative end (red),
#' matching the standard codebook order (e.g. 1 = Strong Democrat through
#' 7 = Strong Republican). Use `reverse = TRUE` if your factor levels run the
#' other way.
#'
#' \itemize{
#'   \item `pid3` — three levels: Democrat, Independent, Republican.
#'   \item `pid7` — seven levels: Strong Democrat through Strong Republican.
#'   \item `id5` — five levels: Very Liberal through Very Conservative.
#' }
#'
#' @param reverse Logical; if `TRUE`, reverse the palette so it runs from the
#'   Republican/conservative end to the Democratic/liberal end.
#' @param ... Additional arguments passed on to
#'   [ggplot2::scale_fill_manual()] or [ggplot2::scale_color_manual()].
#'
#' @return A ggplot2 scale that can be added to a plot with `+`.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' df <- tibble::tibble(pid7 = sample(1:7, 100, replace = TRUE))
#'
#' df %>%
#'   mutate(pid_new = frcode(
#'     pid7 == 1 ~ "Strong Democrat",
#'     pid7 == 2 ~ "Not Strong Democrat",
#'     pid7 == 3 ~ "Lean Democrat",
#'     pid7 == 4 ~ "Independent",
#'     pid7 == 5 ~ "Lean Republican",
#'     pid7 == 6 ~ "Not Strong Republican",
#'     pid7 == 7 ~ "Strong Republican"
#'   )) %>%
#'   ct(pid_new) %>%
#'   ggplot(aes(x = pid_new, y = pct, fill = pid_new)) +
#'   geom_col() +
#'   scale_fill_pid7()
#'
#' @name pid_scales
NULL

# Palettes ordered Democrat/liberal (blue) -> Republican/conservative (red)
pid_palettes <- list(
  pid3 = c("dodgerblue3", "azure4", "firebrick3"),
  pid7 = c("#000080", "#2166AC", "#67A9CF", "azure4", "#EF8A62", "#B2182B", "#8D021F"),
  id5  = c("#2166AC", "#67A9CF", "azure4", "#EF8A62", "#B2182B")
)

pid_values <- function(palette, reverse) {
  values <- pid_palettes[[palette]]
  if (reverse) rev(values) else values
}

#' @rdname pid_scales
#' @export
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
scale_fill_pid3 <- function(reverse = FALSE, ...) {
  ggplot2::scale_fill_manual(values = pid_values("pid3", reverse), ...)
}

#' @rdname pid_scales
#' @export
scale_color_pid3 <- function(reverse = FALSE, ...) {
  ggplot2::scale_color_manual(values = pid_values("pid3", reverse), ...)
}

#' @rdname pid_scales
#' @export
scale_fill_pid7 <- function(reverse = FALSE, ...) {
  ggplot2::scale_fill_manual(values = pid_values("pid7", reverse), ...)
}

#' @rdname pid_scales
#' @export
scale_color_pid7 <- function(reverse = FALSE, ...) {
  ggplot2::scale_color_manual(values = pid_values("pid7", reverse), ...)
}

#' @rdname pid_scales
#' @export
scale_fill_id5 <- function(reverse = FALSE, ...) {
  ggplot2::scale_fill_manual(values = pid_values("id5", reverse), ...)
}

#' @rdname pid_scales
#' @export
scale_color_id5 <- function(reverse = FALSE, ...) {
  ggplot2::scale_color_manual(values = pid_values("id5", reverse), ...)
}
