
#' Binding the Rows of Many Dataframes Together
#'
#' This function will bind the rows of all dataframes together that match a certain pattern
#' @param input Pattern of the dataframes that you want to row bind
#' @keywords bind_rows
#' @export
#' @examples
#' bind_df()

bind_df <- function(input) {
  
  list_of_objects <- mget(ls(.GlobalEnv, pattern = glue("*{input}")), envir = .GlobalEnv)
  df <- bind_rows(list_of_objects)
  df
}

