
#' Correlation with p-values
#'
#' Computes a Pearson correlation test between two variables and returns a tidy tibble
#' with the correlation estimate, p-value, confidence interval, and sample size.
#' @param df A data frame.
#' @param var1 First variable (unquoted).
#' @param var2 Second variable (unquoted).
#' @param na.rm Logical; if `TRUE` (default), drop rows where either variable is `NA`.
#' @keywords correlation p-value
#' @export
#' @importFrom rlang enquo
#' @examples
#'
#' test <- data.frame(
#'   x = c(1, 2, 3, 7, 5, 6, 8),
#'   y = c(11, 23, 1, 4, 6, 34, 22)
#' )
#'
#' corr(test, x, y)

corr <- function(df, var1, var2, na.rm = TRUE) {
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)

  df2 <- dplyr::select(df, !!var1, !!var2)
  df2 <- dplyr::rename(df2, vv1 = !!var1, vv2 = !!var2)

  if (na.rm) {
    df2 <- df2[stats::complete.cases(df2), ]
  }

  result <- stats::cor.test(df2$vv1, df2$vv2)

  tibble::tibble(
    estimate  = result$estimate[[1]],
    statistic = result$statistic[[1]],
    p.value   = result$p.value,
    conf.low  = result$conf.int[1],
    conf.high = result$conf.int[2],
    method    = result$method,
    n         = nrow(df2)
  )
}
