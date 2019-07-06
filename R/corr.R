
#' Correlation with p-values
#'
#' This function gives you a mean with 95 percent CIs
#' @param df Name of the Dataset
#' @param var1 First variable 
#' @param var2 Second variable 
#' @keywords correlation, p-value
#' @export
#' @examples
#' 
#' x <- c(1, 2, 3, 7, 5, 777, 6, 411, 8)
#' y <- c(11, 23, 1, 4, 6, 22455, 34, 22, 22)
#' z <- c(34, 3, 21, 4555, 75, 2, 3334, 1122, 22312)
#' 
#' test <- data.frame(x,y,z) %>% as.tibble()
#'
#' test %>% 
#'  filter(z > 10) %>% 
#'  corr(x,y)
#' 


corr <- function(df, var1, var2){
  
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  
  df2 <- df %>% 
    select(!! var1, !! var2) %>% 
    rename(vv1 = !! var1, vv2 = !! var2)
  
  
  cor.test(df2$vv1, df2$vv2) %>% 
    tidy() %>% 
    rename(n = parameter)
  
}

