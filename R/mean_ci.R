
#' Calculate A Mean with Confidence Intervals 
#'
#' This function gives you a mean with 95 percent CIs
#' @param df Name of the Dataset
#' @param var Variable to find the mean of
#' @param ci Confidence Interval, expressed as a decimal. i.e. .84. Defaults to .95
#' @param wt Weight to be applied  
#' @keywords Mean
#' @export
#' @examples
#' 
#' cces <- read_csv("https://raw.githubusercontent.com/ryanburge/blocks/master/cces.csv")
#' 
#' cces %>% 
#'    mean_ci(gender)
#'    
#' # Weighted Means    
#' 
#' cces %>% 
#'    mean_ci(gender, wt = commonweight_vv)
#'    
#' # Change the Confidence Interval
#' 
#' cces %>% 
#'    mean_ci(gender, ci = .84)
#' 
#' 

mean_ci <- function(df, var, wt, ci = .95) {
  var <- enquo(var)
  ci <- enquo(ci)
  wt <- enquo(wt)
  
  if(quo_is_missing(wt) & quo_is_missing(ci)) {
    df %>% 
      dplyr::summarise(mean = mean(!! var, na.rm = TRUE),
                sd = sd(!! var, na.rm = TRUE), 
                n = n()) %>% 
      dplyr::mutate(se = sd/sqrt(n),
             lower = mean - qt(1 - (0.05 /2),  n -1) * se,
             upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
      dplyr::mutate(mean = round(mean, 3))
  }
  
  if(quo_is_missing(wt)) {
    df %>% 
      dplyr::summarise(mean = mean(!! var,  na.rm = TRUE),
                sd = sd(!! var, na.rm = TRUE), 
                n = n()) %>% 
      dplyr::mutate(level = 1 - !! ci) %>% 
      dplyr::mutate(se = sd/sqrt(n),
             lower = mean - qt(1 - (level /2),  n -1) * se,
             upper = mean + qt(1 - (level /2),  n -1) * se) %>% 
      dplyr::mutate(mean = round(mean, 3))
  }
  
  else {
    
    df %>% 
      dplyr::summarise(mean = weighted.mean(!! var, w = !! wt, na.rm = TRUE),
                sd = sd(!! var, na.rm = TRUE), 
                n = n()) %>% 
      dplyr::mutate(level = 1 - !! ci) %>% 
      dplyr::mutate(se = sd/sqrt(n),
             lower = mean - qt(1 - (level /2),  n -1) * se,
             upper = mean + qt(1 - (level /2),  n -1) * se) %>% 
      dplyr::select(-level) %>% 
      dplyr::mutate(mean = round(mean, 3))
  } 
}

