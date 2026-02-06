#' Count and tabulate with percentages (weighted or unweighted)
#'
#' Respects existing dplyr groups and computes percentages within each group.
#'
#' @param df A data frame (possibly grouped).
#' @param var The categorical variable to tabulate (unquoted, tidyselect-style).
#' @param wt Optional numeric weights column (unquoted).
#' @param show_na Logical; if `TRUE` (default), include `NA` rows. If `FALSE`, drop them.
#' @param cum Logical; if `TRUE`, add cumulative count and cumulative percentage columns.
#' @param sort Logical; if `TRUE`, sort rows in descending order by count.
#'
#' @return A tibble with columns for the variable, `n`, `pct`, and optionally `cum_n`
#'   and `cum_pct`. Grouping structure from the input is preserved.
#'
#' @export
#' @importFrom dplyr count mutate filter arrange group_by group_vars desc all_of
#' @importFrom rlang enquo quo_is_missing sym syms .data
#' @importFrom tidyselect eval_select
ct <- function(df, var, wt, show_na = TRUE, cum = FALSE, sort = FALSE) {
  var_quo <- rlang::enquo(var)
  wt_quo  <- rlang::enquo(wt)

  # Validate: exactly one column selected
  sel <- tidyselect::eval_select(var_quo, df)
  if (length(sel) != 1L) stop("`var` must select exactly one column.", call. = FALSE)
  var_sym <- rlang::sym(names(sel))

  # Optional NA filtering (on var and, if provided, wt)
  if (!show_na) {
    if (rlang::quo_is_missing(wt_quo)) {
      df <- dplyr::filter(df, !is.na(!!var_sym))
    } else {
      df <- dplyr::filter(df, !is.na(!!var_sym), !is.na(!!wt_quo))
    }
  }

  # Existing groups (0 length if ungrouped)
  gvars <- dplyr::group_vars(df)
  gsyms <- rlang::syms(gvars)

  # Count within groups (or overall if ungrouped)
  if (rlang::quo_is_missing(wt_quo)) {
    out <- dplyr::count(df, !!!gsyms, !!var_sym, name = "n")
  } else {
    out <- dplyr::count(df, !!!gsyms, !!var_sym, wt = !!wt_quo, name = "n")
  }

  if (nrow(out) == 0L) {
    out$pct <- numeric(0)
    if (cum) {
      out$cum_n   <- numeric(0)
      out$cum_pct <- numeric(0)
    }
    return(out)
  }

  # Group by existing groups for per-group pct/cum
  if (length(gvars)) out <- dplyr::group_by(out, !!!gsyms)

  # Compute group total once, then derive pct (and optionally cumulative columns)
  out <- dplyr::mutate(out, pct = round(.data$n / sum(.data$n, na.rm = TRUE), 3))

  # Optional sort within group(s)
  if (sort) {
    if (length(gvars)) {
      out <- dplyr::arrange(out, !!!gsyms, dplyr::desc(.data$n))
    } else {
      out <- dplyr::arrange(out, dplyr::desc(.data$n))
    }
  }

  # Cumulative per group
  if (cum) {
    out <- dplyr::mutate(
      out,
      cum_n   = cumsum(.data$n),
      cum_pct = round(.data$cum_n / sum(.data$n, na.rm = TRUE), 3)
    )
  }

  out  # keep grouping as-is so downstream ops can continue per-group
}
