
#' Correlation with p-values
#'
#' This function gives you a mean with 95 percent CIs
#' @param df Name of the Dataset
#' @param var1 First variable 
#' @param var2 Second variable 
#' @keywords correlation, p-value
#' @export
#' @examples
#' mean_ci()


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

