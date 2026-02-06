
#' A Crosstab Stacked Bar Graph
#'
#' Creates a stacked bar chart showing the distribution of one variable within
#' levels of another.
#' @param df A data frame.
#' @param var1 The grouping variable (x-axis).
#' @param var2 The variable to count within groups (fill).
#' @param count Logical; if `TRUE` (default), display percentage labels on the bars.
#' @keywords Crosstab stacked-bar-graph
#' @export
#' @importFrom rlang enquo !! .data
#' @examples
#'
#' mtcars %>% xbar(cyl, am)


xbar <- function(df, var1, var2, count = TRUE) {
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)

  df1 <- df %>%
    dplyr::group_by(!!var1) %>%
    ct(!!var2) %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!var1, !!var2, fill = list(n = 0, pct = 0)) %>%
    replace(is.na(.), 0)

  p <- df1 %>%
    ggplot2::ggplot(ggplot2::aes(x = !!var1, y = .data$pct, fill = !!var2)) +
    ggplot2::geom_col(color = "black") +
    ggplot2::scale_fill_gradient(low = "#556270", high = "#FF6B6B") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_y_continuous(labels = scales::percent)

  if (count) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(.data$pct * 100, "%")),
        position = ggplot2::position_stack(vjust = 0.5),
        size = 4
      )
  }

  p
}
