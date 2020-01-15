
#' An All-Purpose Count Function 
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
#' 
#' 
#' cces <- read_csv("https://raw.githubusercontent.com/ryanburge/blocks/master/cces.csv")
#' cces %>% 
#'    ct(race)
#'    
#'# With a weight 
#' cces %>% 
#'      ct(race, wt = commonweight_vv)            
#'      
#'cces %>% 
#'    mutate(race2 = frcode(race == 1 ~ "White",
#'                      race == 2 ~ "Black", 
#'                      race == 3 ~ "Hispanic",
#'                       race == 4 ~ "Asian")) %>% 
#'   ct(race2, show_na = FALSE, commonweight_vv)   
#'      
#'# Cumulative Counts       
#' 
#' cces %>% 
#'      ct(race, cum = TRUE)
#' 



ct <- function(df, var, wt, cum = FALSE, show_na = TRUE) {
  
  var <- enquo(var)
  wt <- enquo(wt)
  
  if(quo_is_missing(wt) && show_na == FALSE) {
    df1 <- df %>%
      dplyr::filter(!! var != "NA") %>% 
      dplyr::count(!! var) %>% 
      dplyr::mutate(pct = prop.table(n)) %>% 
      dplyr::mutate(pct = round(pct, 3)) 
    
  } else if(show_na == FALSE){
    
    df1 <- df %>%
      dplyr::filter(!! var != "NA") %>% 
      dplyr::count(!! var, wt = !! wt) %>% 
      dplyr::mutate(pct = prop.table(n)) %>% 
      dplyr::mutate(pct = round(pct, 3)) 
    
  }
  
  else {
    
    df1 <- df %>%
      dplyr::count(!! var, wt = !! wt) %>% 
      dplyr::mutate(pct = prop.table(n)) %>% 
      dplyr::mutate(pct = round(pct, 3))
    
  }
  
  if(cum == TRUE){
    
    df1 %>% 
      dplyr::mutate(cum_n = cumsum(n)) %>% 
      dplyr::mutate(cum_pct = cumsum(pct))
  } else {
    
    df1
    
    
    
    
  }
}










