
#' Recodes Variables and Keeps Factor Levels
#'
#' This a Modification of case_when that keeps things in the order they were recoded
#' @param df Name of the Dataset
#' @keywords Mean
#' @export
#' @examples
#' frcode()



frcode <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}