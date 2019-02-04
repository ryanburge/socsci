
#' A Crosstab Heatmap
#'
#' This function creates a crosstabulation heatmap
#' @param df Name of the Dataset
#' @param var1 This will be the variable that you do group_by with
#' @param var2 This is the variable that will be counted
#' @keywords Crosstab, heatmap
#' @export
#' @examples
#' xheat()


xheat <- function(df, var1, var2){
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  
  
  df1 <- df %>% 
    group_by(!! var1) %>% 
    ct(!! var2)
  
   df1 %>% 
     ggplot(., aes(x = !! var1, y = !! var2)) +
     geom_tile(aes(fill = pct), color = "black") +
     geom_text(aes(x= !! var1, y = !! var2, label = paste0(pct*100, '%'))) +
     theme_minimal() +
     theme(legend.position = "none")
   

}

 