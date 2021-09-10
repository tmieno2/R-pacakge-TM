#' @title st_intersects_join
#'
#' @description This function joins the attributes of two sf objects based on st_intesects() 
#' with geometry information removed. This is much faster than st_intersection().
#' @param sf_1 An sf object 
#' @param sf_2 An sf object 
#' @import data.table
#' @export
#' @examples

st_intersects_join <- function(sf_1, sf_2) {

  sf_1_to_2 <- 
    st_intersects(sf_1, sf_2) %>% 
    tibble(intersections = .) %>% 
    rowwise() %>% 
    mutate(data = list(
      data.table(row_2 = intersections)
    )) %>% 
    .$data %>% 
    rbindlist(idcol = "row_1")

  temp_data_1 <- data.table(sf_1) %>% 
    .[, row_1 := 1:nrow(.)] %>% 
    .[, geometry := NULL]

  temp_data_2 <- data.table(sf_2) %>% 
    .[, row_2 := 1:nrow(.)] %>% 
    .[, geometry := NULL]

  return_data <- sf_1_to_2 %>% 
    temp_data_1[., on = "row_1"] %>% 
    temp_data_2[., on = "row_2"] %>% 
    .[, `:=`(
      row_1 = NULL,
      row_2 = NULL
    )]

  return(return_data)

}
