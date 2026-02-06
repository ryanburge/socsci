
#' A Crosstab Heatmap
#'
#' Creates a heatmap showing the distribution of one variable within levels of another.
#' @param df A data frame.
#' @param var1 The grouping variable (x-axis).
#' @param var2 The variable to count within groups (y-axis).
#' @param count Logical; if `TRUE`, display both percentage and count in each tile.
#'   If `FALSE` (default), display only the percentage.
#' @keywords Crosstab heatmap
#' @export
#' @importFrom rlang enquo !! .data
#' @examples
#'
#' mtcars %>% xheat(cyl, am)


xheat <- function(df, var1, var2, count = FALSE) {
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)

  df1 <- df %>%
    dplyr::group_by(!!var1) %>%
    ct(!!var2) %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!var1, !!var2, fill = list(n = 0, pct = 0)) %>%
    replace(is.na(.), 0)

  p <- df1 %>%
    ggplot2::ggplot(ggplot2::aes(x = !!var1, y = !!var2)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$pct), color = "black") +
    ggplot2::scale_fill_gradient(low = "#556270", high = "#FF6B6B") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if (count) {
    df1 <- dplyr::mutate(df1, lab = paste0(.data$pct * 100, "%\nN=", .data$n))
    p <- p +
      ggplot2::geom_text(data = df1, ggplot2::aes(label = .data$lab))
  } else {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label = paste0(.data$pct * 100, "%")))
  }

  p
}
