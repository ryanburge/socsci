
#' A Count Function with Weights
#'
#' This function gives you a simple count with percentages that is weighted
#' @param df Name of the Dataset
#' @param var Variable to Count
#' @param wt Weighting Variable
#' @keywords Count
#' @export
#' @examples
#' ct_wt()

ct <- function(df, var, wt) {
  var <- enquo(var)
  wt <- enquo(wt)
  
  if(quo_is_missing(wt)) {
    df %>%
      count(!! var) %>% 
      mutate(pct = prop.table(n)) %>% 
      mutate(pct = round(pct, 3))
    
  } else {
  
  df %>%
    count(!! var, wt = !! wt) %>% 
    mutate(pct = prop.table(n)) %>% 
      mutate(pct = round(pct, 3))
    
  }
}







