
#' Recodes Variables and Keeps Factor Levels
#'
#' This a Modification of case_when that keeps things in the order they were recoded
#' @param df Name of the Dataset
#' @keywords Mean
#' @export
#' @examples
#' 
#' cces <- read_csv("https://raw.githubusercontent.com/ryanburge/cces/master/CCES%20for%20Methods/small_cces.csv")
#'
#' cces %>% 
#'     mutate(pid_new = frcode(pid7 == 1 ~ "Strong Democrat", 
#'                             pid7 == 2 ~ "Not Strong Democrat", 
#'                             pid7 == 3 ~ "Lean Democrat", 
#'                             pid7 == 4 ~ "Independent", 
#'                             pid7 == 5 ~ "Lean Republican", 
#'                             pid7 == 6 ~ "Not Strong Republican", 
#'                             pid7 == 7 ~ "Strong Republican", 
#'                             TRUE ~ "REMOVE")) %>% 
#'            ct(pid_new) %>% 
#'            filter(pid_new != "REMOVE") %>% 
#'            ggplot(., aes(x = pid_new, y = pct)) +
#'            geom_col()
#'   
#' 



frcode <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}