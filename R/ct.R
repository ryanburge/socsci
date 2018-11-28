
#' A Count Function with Weights
#'
#' This function gives you a simple count with percentages that is weighted
#' @param df Name of the Dataset
#' @param var Variable to Count
#' @param wt Weighting Variable
#' @param cum Will add a cumulative total if set to TRUE
#' @keywords Count
#' @export
#' @examples
#' ct()


ct <- function(df, var, wt, cum = FALSE) {
  var <- enquo(var)
  wt <- enquo(wt)
  
  if(quo_is_missing(wt)) {
    df1 <- df %>%
      count(!! var) %>% 
      mutate(pct = prop.table(n)) %>% 
      mutate(pct = round(pct, 3)) 
    
  } else {
    
    df1 <- df %>%
      count(!! var, wt = !! wt) %>% 
      mutate(pct = prop.table(n)) %>% 
      mutate(pct = round(pct, 3))
    
  }
  
  if(cum == TRUE){
    
    df1 %>% 
      mutate(cum = cumsum(n))
  } else {
    
    df1
  }
}










