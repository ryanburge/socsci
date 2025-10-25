#' Recode variables while preserving order as factor levels
#'
#' A wrapper around [dplyr::case_when()] that automatically converts the result
#' to a factor with levels in the order they appear in the recoding statements.
#' This is useful for creating ordered categorical variables for plotting or tables
#' where you want to control the display order.
#'
#' @param ... A sequence of two-sided formulas in the form `condition ~ label`.
#'   The left-hand side (LHS) determines which values to recode; the right-hand
#'   side (RHS) provides the new value. Formulas are evaluated in order.
#'
#' @return A factor vector with levels ordered as they appear in the recoding formulas.
#'   Any `NA` or unmatched values will appear as `NA` in the factor.
#'
#' @details
#' Unlike [dplyr::case_when()], which returns the common underlying type (often a
#' character vector), `frcode()` returns a factor whose level order matches the order
#' of your recode statements. This often removes the need for a follow-up call
#' to `forcats::fct_relevel()` or `factor(levels = ...)`.
#'
#' The function extracts the right-hand side of each formula to determine factor
#' levels, preserving the order in which conditions are specified. Duplicate labels
#' are handled automatically (only the first occurrence sets the position). RHS
#' values that evaluate to `NA` are not included among the factor levels.
#'
#' @examples
#' library(dplyr)
#' df <- tibble::tibble(pid7 = c(1,2,3,4,5,6,7,NA,4))
#'
#' df %>%
#'   mutate(
#'     pid_new = frcode(
#'       pid7 == 1 ~ "Strong Democrat",
#'       pid7 == 2 ~ "Not Strong Democrat",
#'       pid7 == 3 ~ "Lean Democrat",
#'       pid7 == 4 ~ "Independent",
#'       pid7 == 5 ~ "Lean Republican",
#'       pid7 == 6 ~ "Not Strong Republican",
#'       pid7 == 7 ~ "Strong Republican",
#'       TRUE ~ NA_character_
#'     )
#'   )
#'
#' Recode variables while preserving order as factor levels
#'
#' @export
#' @importFrom dplyr case_when
#' @importFrom rlang enquos get_expr is_formula f_lhs f_rhs eval_tidy caller_env
frcode <- function(...) {
  # Capture formulas as quosures so this works in mutate()/data masks
  quos <- rlang::enquos(..., .ignore_empty = "all")
  if (!length(quos)) {
    stop("`frcode()` requires at least one two-sided formula like `condition ~ label`.", call. = FALSE)
  }
  
  env <- rlang::caller_env()
  levels <- character(0)
  
  for (i in seq_along(quos)) {
    expr_i <- rlang::get_expr(quos[[i]])
    
    # Backward-compatible formula validation:
    if (!rlang::is_formula(expr_i)) {
      stop("All arguments to `frcode()` must be two-sided formulas like `condition ~ label`.\n",
           "Problem at argument ", i, ".", call. = FALSE)
    }
    lhs <- rlang::f_lhs(expr_i)
    rhs <- rlang::f_rhs(expr_i)
    if (is.null(lhs) || is.null(rhs)) {
      stop("Each formula must be two-sided: `condition ~ label`.\nProblem at argument ", i, ".", call. = FALSE)
    }
    
    # Evaluate RHS label in the caller env (not data mask)
    rhs_val <- rlang::eval_tidy(rhs, env = env)
    
    # Record non-NA scalar labels in first-seen order
    if (length(rhs_val) == 1 && !is.na(rhs_val)) {
      lab <- as.character(rhs_val)
      if (!lab %in% levels) levels <- c(levels, lab)
    }
  }
  
  # Do the recode in the data mask
  out <- dplyr::case_when(!!!quos)
  
  # Factor with requested order
  factor(out, levels = levels)
}
