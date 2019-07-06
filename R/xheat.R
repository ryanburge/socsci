
#' A Crosstab Heatmap
#'
#' This function creates a crosstabulation heatmap
#' @param df Name of the Dataset
#' @param var1 This will be the variable that you do group_by with
#' @param var2 This is the variable that will be counted
#' @param count Will add the total count to each square, it's off by default. 
#' @keywords Crosstab, heatmap
#' @export
#' @examples
#' 
#' mtcars %>% xheat(cyl, am)
#' 
#' 
#' xheat()


xheat <- function(df, var1, var2, count = FALSE) {
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  
  
  if(count == TRUE){
    df1 <- df %>% 
      group_by(!! var1) %>% 
      ct(!! var2) %>% 
      ungroup(!! var1) %>% 
      complete(!! var1, !! var2, fill = list(count = 0)) %>% 
      replace(is.na(.), 0)
    
    df1 %>% 
      mutate(lab = paste0(pct*100,'%\nN=',n)) %>% 
      ggplot(., aes(x = !! var1, y = !! var2)) +
      geom_tile(aes(fill = pct), color = "black") +
      geom_text(aes(x= !! var1, y = !! var2, label = lab)) +
      scale_fill_gradient(low = "#556270", high = "#FF6B6B")+ 
      theme_minimal() +
      theme(legend.position = "none") 
    
    
  } else {
    
    df1 <- df %>% 
      group_by(!! var1) %>% 
      ct(!! var2) %>% 
      ungroup(!! var1) %>% 
      complete(!! var1, !! var2, fill = list(count = 0)) %>% 
      replace(is.na(.), 0)
    
    df1 %>% 
      ggplot(., aes(x = !! var1, y = !! var2)) +
      geom_tile(aes(fill = pct), color = "black") +
      geom_text(aes(x= !! var1, y = !! var2, label = paste0(pct*100, '%'))) +
      scale_fill_gradient(low = "#556270", high = "#FF6B6B")+ 
      theme_minimal() +
      theme(legend.position = "none") 
    
    
  }
  
}

 