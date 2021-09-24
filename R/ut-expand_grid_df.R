#' Expand on two data.frames 
#'
#' @description An extension of expand.grid() to expand on two `data.frame`s
#' @param data_1 first data.frame
#' @param data_2 second data.frame
#' @import data.table
#' @export
#' @examples

expand_grid_df <- function(data_1, data_2) {

  data_1_ex <- 
    data_1[rep(1:nrow(data_1), each = nrow(data_2)), ] %>% 
    dplyr::mutate(rowid := 1:nrow(.)) %>% 
    data.table()

  data_2_ex <- 
    data_2[rep(1:nrow(data_2), nrow(data_1)), ] %>% 
    dplyr::mutate(rowid := 1:nrow(.)) %>% 
    data.table()

  expanded_data <- data_2_ex[data_1_ex, on = "rowid"]

  if ("tbl" %in% class(data_1)) {
    expanded_data <- as_tibble(expanded_data)
  } 

  return(expanded_data)

}
