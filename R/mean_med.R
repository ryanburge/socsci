
#' Gives You the Mean and Median
#'
#' This function gives you the mean and the median of a dataset
#' @param df Name of the Dataset
#' @param var Variable to find the mean and median of
#' @keywords Mean
#' @export
#' @examples
#' 
#' money1 <- read_csv("https://raw.githubusercontent.com/ryanburge/pls2003_sp17/master/sal_work.csv")
#' 
#' money1 %>% 
#'   mean_med(salary)
#' 
#' 
#' 

mean_med <- function(df, var) {
  var <- enquo(var)
  
  df %>% 
    dplyr::summarise(mean = mean(!! var), median =  median(!! var))
   
}


