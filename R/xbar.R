
#' A Crosstab Stacked Bar Graph
#'
#' This function creates a crosstabulation heatmap
#' @param df Name of the Dataset
#' @param var1 This will be the variable that you do group_by with
#' @param var2 This is the variable that will be counted
#' @param count Will add the total count to each square if added
#' @keywords Crosstab, stacked bar graph
#' @export
#' @examples
#' 
#' mtcars %>% xbar(cyl, am)
#' 
#' 


xbar <- function(df, var1, var2, count = TRUE) {
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  
  
  if(missing(count)){
    df1 <- df %>% 
      dplyr::group_by(!! var1) %>% 
      ct(!! var2) %>% 
      dplyr::ungroup(!! var1) %>% 
      complete(!! var1, !! var2, fill = list(count = 0)) %>% 
      replace(is.na(.), 0)
    
    df1 %>% 
      ggplot(., aes(x = !! var1, y = pct, fill = !! var2)) +
      geom_col(color = "black") +
      scale_fill_gradient(low = "#556270", high = "#FF6B6B")+ 
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(aes(label = paste0(pct*100, '%')), position = position_stack(vjust = 0.5), size = 14) +
      scale_y_continuous(labels = percent)
      
    
  } else {
    
    df1 <- df %>% 
      dplyr::group_by(!! var1) %>% 
      ct(!! var2) %>% 
      dplyr::ungroup(!! var1) %>% 
      complete(!! var1, !! var2, fill = list(count = 0)) %>% 
      replace(is.na(.), 0)
    
    df1 %>% 
      ggplot(., aes(x = !! var1, y = pct, fill = !! var2)) +
      geom_col(color = "black") +
      scale_fill_gradient(low = "#556270", high = "#FF6B6B")+ 
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(aes(label = paste0(pct*100, '%')), position = position_stack(vjust = 0.5), size = 4)+
      scale_y_continuous(labels = percent)
    
    
  }
  
}


