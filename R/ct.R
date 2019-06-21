
#' A Count Function with Weights
#'
#' This function gives you a simple count with percentages that is weighted
#' @param df Name of the Dataset
#' @param var Variable to Count
#' @param wt Weighting Variable
#' @param show_na Will remove the NAs before the calculation if set to TRUE, defaults to FALSE 
#' @param cum Will add a cumulative total if set to TRUE
#' @keywords Count
#' @export
#' @examples
#' ct()



ct <- function(df, var, wt, cum = FALSE, show_na = TRUE) {
  
  var <- enquo(var)
  wt <- enquo(wt)
  
  if(quo_is_missing(wt) && show_na == FALSE) {
    df1 <- df %>%
      filter(!! var != "NA") %>% 
      count(!! var) %>% 
      mutate(pct = prop.table(n)) %>% 
      mutate(pct = round(pct, 3)) 
    
  } else if(show_na == FALSE){
    
    df1 <- df %>%
      filter(!! var != "NA") %>% 
      count(!! var, wt = !! wt) %>% 
      mutate(pct = prop.table(n)) %>% 
      mutate(pct = round(pct, 3)) 
    
  }
  
  else {
    
    df1 <- df %>%
      count(!! var, wt = !! wt) %>% 
      mutate(pct = prop.table(n)) %>% 
      mutate(pct = round(pct, 3))
    
  }
  
  if(cum == TRUE){
    
    df1 %>% 
      mutate(cum_n = cumsum(n)) %>% 
      mutate(cum_pct = cumsum(pct))
  } else {
    
    df1
    
    
    
    
  }
}










