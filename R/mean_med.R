
#' Gives You the Mean and Median
#'
#' This function gives you the mean and the median of a dataset
#' @param df Name of the Dataset
#' @param var Variable to find the mean and median of
#' @keywords Mean
#' @export
#' @examples
#' mean_med()

mean_med <- function(df, var) {
  var <- enquo(var)
  
  df %>% 
    summarise(mean = mean(!! var), median =  median(!! var))
   
}


