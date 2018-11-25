
#' A Mean with 95 percent CIs
#'
#' This function gives you a mean with 95 percent CIs
#' @param df Name of the Dataset
#' @param var Variable to find the mean of
#' @param ci Confidence Interval, expressed as a decimal. i.e. .84. Defaults to .95
#' @param wt Weight to be applied  
#' @keywords Mean
#' @export
#' @examples
#' mean_ci()

mean_ci <- function(df, var, wt, ci = .95) {
  var <- enquo(var)
  ci <- enquo(ci)
  wt <- enquo(wt)
  
  if(quo_is_missing(wt) & quo_is_missing(ci)) {
    df %>% 
      summarise(mean = mean(!! var, na.rm = TRUE),
                sd = sd(!! var, na.rm = TRUE), 
                n = n()) %>% 
      mutate(se = sd/sqrt(n),
             lower = mean - qt(1 - (0.05 /2),  n -1) * se,
             upper = mean + qt(1 - (0.05 /2),  n -1) * se) 
  }
  
  if(quo_is_missing(wt)) {
    df %>% 
      summarise(mean = mean(!! var,  na.rm = TRUE),
                sd = sd(!! var, na.rm = TRUE), 
                n = n()) %>% 
      mutate(level = 1 - !! ci) %>% 
      mutate(se = sd/sqrt(n),
             lower = mean - qt(1 - (level /2),  n -1) * se,
             upper = mean + qt(1 - (level /2),  n -1) * se) 
  }
  
  else {
    
    df %>% 
      summarise(mean = weighted.mean(!! var, w = !! wt, na.rm = TRUE),
                sd = sd(!! var, na.rm = TRUE), 
                n = n()) %>% 
      mutate(level = 1 - !! ci) %>% 
      mutate(se = sd/sqrt(n),
             lower = mean - qt(1 - (level /2),  n -1) * se,
             upper = mean + qt(1 - (level /2),  n -1) * se) %>% 
      select(-level)
  } 
}

