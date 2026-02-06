
#' Gives You the Mean and Median
#'
#' Computes the mean and median of a numeric variable. Respects existing dplyr groups.
#' @param df A data frame.
#' @param var Variable to find the mean and median of (unquoted).
#' @param na.rm Logical; if `TRUE` (default), remove `NA` values before computing.
#' @keywords Mean Median
#' @export
#' @importFrom rlang enquo
#' @examples
#'
#' df <- data.frame(x = c(1, 2, 3, 4, 5, NA))
#' mean_med(df, x)
#' mean_med(df, x, na.rm = FALSE)

mean_med <- function(df, var, na.rm = TRUE) {
  var <- rlang::enquo(var)

  dplyr::summarise(
    df,
    mean   = mean(!!var, na.rm = na.rm),
    median = stats::median(!!var, na.rm = na.rm)
  )
}
