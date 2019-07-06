
#' Binding the Rows of Many Dataframes Together
#'
#' This function will bind the rows of all dataframes together that match a certain pattern
#' @param input Pattern of the dataframes that you want to row bind
#' @keywords bind_rows
#' @export
#' @examples
#' 
#'  dd1 <- data.frame(a = 1, b = 2)
#'  dd2 <- data.frame(a = 3, b = 4)
#'  dd3 <- data.frame(a = 5, b = 6) 
#' 
#' bind_df("dd")

bind_df <- function(input) {
  
  list_of_objects <- mget(ls(.GlobalEnv, pattern = glue("*{input}")), envir = .GlobalEnv)
  df <- bind_rows(list_of_objects)
  df
}

