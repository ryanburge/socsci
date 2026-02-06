
#' Binding the Rows of Many Dataframes Together
#'
#' This function will bind the rows of all dataframes together that match a certain pattern
#' in the global environment.
#' @param input Pattern (regex) of the dataframes that you want to row bind.
#' @keywords bind_rows
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#'
#'  dd1 <- data.frame(a = 1, b = 2)
#'  dd2 <- data.frame(a = 3, b = 4)
#'  dd3 <- data.frame(a = 5, b = 6)
#'
#' bind_df("dd")

bind_df <- function(input) {
  pattern <- paste0(".*", input, ".*")
  matching <- ls(.GlobalEnv, pattern = pattern)
  if (!length(matching)) {
    warning("No objects in .GlobalEnv matched pattern: ", input)
    return(dplyr::bind_rows())
  }
  list_of_objects <- mget(matching, envir = .GlobalEnv)
  dplyr::bind_rows(list_of_objects)
}
