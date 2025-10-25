#' Count and tabulate with percentages (weighted or unweighted)
#'
#' Produces frequency counts with percentages for a categorical variable, with
#' optional survey weights. Can exclude `NA` values and optionally add cumulative
#' counts and percentages.
#'
#' @param df A data frame.
#' @param var Variable to tabulate (unquoted, tidyselect-style). Must select exactly one column.
#' @param wt Optional weighting variable (unquoted). If omitted, produces unweighted counts.
#' @param show_na Logical; if `FALSE`, removes `NA` values from `var` (and, if `wt` is supplied,
#'   removes rows with `NA` in `wt`). Default is `TRUE`.
#' @param cum Logical; if `TRUE`, adds cumulative count (`cum_n`) and cumulative
#'   percentage (`cum_pct`) columns. Default is `FALSE`.
#' @param sort Logical; if `TRUE`, sorts rows by descending `n` before computing `cum_*`.
#'   Default is `FALSE` (preserves input order / factor order).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item The grouping variable (name matches `var`).
#'   \item \code{n} Count (sum of weights if \code{wt} supplied).
#'   \item \code{pct} Proportion (rounded to 3 decimal places).
#'   \item \code{cum_n}, \code{cum_pct} (if \code{cum = TRUE}) cumulative count and percentage.
#' }
#'
#' @details
#' Internally uses [dplyr::count()] with optional weights. Percentages are computed
#' as `n / sum(n)` over the returned table (after any `NA` filtering), then rounded
#' to three decimals for readability. The function first drops any existing grouping
#' on `df` so results are computed for the whole data.
#'
#' @examples
#' library(dplyr)
#' df <- tibble::tibble(
#'   race = c("White", "Black", "Hispanic", "Asian", NA, "White"),
#'   weight = c(1, 1.2, 0.8, 1.5, 1, 1)
#' )
#'
#' # Basic unweighted count
#' df %>% ct(race)
#'
#' # Weighted count
#' df %>% ct(race, wt = weight)
#'
#' # Exclude NAs
#' df %>% ct(race, show_na = FALSE)
#'
#' # With cumulative totals, sorted by frequency
#' df %>% ct(race, cum = TRUE, sort = TRUE)
#'
#' @export
#' @importFrom dplyr count mutate filter ungroup arrange
#' @importFrom rlang enquo quo_is_missing sym
#' @importFrom tidyselect eval_select
ct <- function(df, var, wt, show_na = TRUE, cum = FALSE, sort = FALSE) {
  var_quo <- rlang::enquo(var)
  wt_quo  <- rlang::enquo(wt)
  
  sel <- tidyselect::eval_select(var_quo, df)
  if (length(sel) != 1L) stop("`var` must select exactly one column.", call. = FALSE)
  var_sym <- rlang::sym(names(sel))
  
  df <- dplyr::ungroup(df)
  
  if (!show_na) {
    if (rlang::quo_is_missing(wt_quo)) {
      df <- dplyr::filter(df, !is.na(!!var_sym))
    } else {
      df <- dplyr::filter(df, !is.na(!!var_sym), !is.na(!!wt_quo))
    }
  }
  
  if (rlang::quo_is_missing(wt_quo)) {
    out <- dplyr::count(df, !!var_sym, name = "n")
  } else {
    out <- dplyr::count(df, !!var_sym, wt = !!wt_quo, name = "n")
  }
  
  if (nrow(out) == 0L) {
    out$pct <- numeric(0)
    if (cum) {
      out$cum_n   <- numeric(0)
      out$cum_pct <- numeric(0)
    }
    return(out)
  }
  
  if (sort) out <- dplyr::arrange(out, dplyr::desc(.data$n))
  
  total_n <- sum(out$n, na.rm = TRUE)
  out <- dplyr::mutate(out, pct = round(n / total_n, 3))
  
  if (cum) {
    out <- dplyr::mutate(out, cum_n = cumsum(n), cum_pct = round(cum_n / total_n, 3))
  }
  
  return(out)  # ← ensure a value is returned when cum = FALSE
}