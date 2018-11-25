
#' Converts NAs in a dataset to zeroes
#'
#' This function will convert all the NA values in a vector to 0
#' @param df Name of the Dataset
#' @param var Variable to find convert NAs to Zero
#' @keywords Mean
#' @export
#' @examples
#' na_zero()

na_zero <- function(df, var) {
  var <- enquo(var)
  new_var <- quo_name(var)
  
  df %>% 
    mutate(nvar = !! var) %>% 
    replace_na(list(nvar =0)) %>% 
    mutate(!! new_var := nvar) %>% 
    select(-nvar)
}



