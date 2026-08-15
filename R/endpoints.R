#' Select the First or Last Observation in Each Series
#'
#' Pulls the first, last, or both endpoint observations according to an x
#' variable. Existing dplyr groups are honored, and additional series or facet
#' variables can be supplied with `by`. This is useful for building data passed
#' to direct-label layers such as `geom_text()` or `ggrepel::geom_text_repel()`.
#'
#' @param df A data frame, possibly grouped.
#' @param x The variable that orders observations, such as a year or date
#'   column (unquoted).
#' @param by Optional tidyselect specification of additional variables that
#'   identify separate series, such as `series` or `c(series, facet)`.
#' @param side Which endpoint to return: `"last"` (the default), `"first"`, or
#'   `"both"`.
#' @param with_ties Logical; when `TRUE`, keep every row tied at an endpoint.
#'   The default keeps one row per endpoint and group.
#' @param na.rm Logical; when `TRUE` (the default), remove rows with missing
#'   values of `x` before selecting endpoints.
#'
#' @return A tibble containing the selected rows and the original columns.
#'   The input's original grouping structure is preserved; groups supplied
#'   only through `by` are temporary.
#' @export
#' @importFrom rlang enquo quo_is_null sym syms .data
#' @importFrom tidyselect eval_select
#'
#' @examples
#' df <- tibble::tibble(
#'   year = rep(2020:2022, 2),
#'   series = rep(c("A", "B"), each = 3),
#'   value = c(10, 12, 14, 8, 9, 11)
#' )
#'
#' endpoints(df, year, by = series)
#' endpoints(df, year, by = series, side = "both")
endpoints <- function(df, x, by = NULL,
                      side = c("last", "first", "both"),
                      with_ties = FALSE, na.rm = TRUE) {
  side <- match.arg(side)
  x_quo <- rlang::enquo(x)
  by_quo <- rlang::enquo(by)

  x_sel <- tidyselect::eval_select(x_quo, df)
  if (length(x_sel) != 1L) {
    stop("`x` must select exactly one column.", call. = FALSE)
  }
  x_name <- names(x_sel)
  x_sym <- rlang::sym(x_name)

  by_names <- character(0)
  if (!rlang::quo_is_null(by_quo)) {
    by_names <- names(tidyselect::eval_select(by_quo, df))
  }

  original_groups <- dplyr::group_vars(df)
  endpoint_groups <- unique(c(original_groups, by_names))

  out <- df
  if (na.rm) {
    out <- dplyr::filter(out, !is.na(!!x_sym))
  }
  if (length(endpoint_groups)) {
    out <- dplyr::group_by(out, !!!rlang::syms(endpoint_groups))
  }

  first_rows <- function(data) {
    dplyr::slice_min(
      data,
      order_by = .data[[x_name]],
      n = 1,
      with_ties = with_ties
    )
  }

  last_rows <- function(data) {
    dplyr::slice_max(
      data,
      order_by = .data[[x_name]],
      n = 1,
      with_ties = with_ties
    )
  }

  out <- switch(
    side,
    first = first_rows(out),
    last = last_rows(out),
    both = dplyr::bind_rows(first_rows(out), last_rows(out))
  )

  if (side == "both" && nrow(out)) {
    out <- out[!duplicated(out), , drop = FALSE]
  }

  out <- dplyr::ungroup(out)
  if (length(original_groups)) {
    out <- dplyr::group_by(out, !!!rlang::syms(original_groups))
  }

  out
}
