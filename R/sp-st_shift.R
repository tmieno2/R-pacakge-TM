#' @title st_shift
#'
#' @description shift an sf object
#' @param data_sf An sf object 
#' @param shift A vector of two numeric numbers (x, y): data_sf is shifted by (x, y) 
#' @param keep_data keep the non-geometry data part of the data (default: TRUE)
#' @export
#' @examples

st_shift <- function(data_sf, shift, keep_data = TRUE) {

  data_geom <- st_geometry(data_sf) 
  temp_crs <- st_crs(data_sf) 

  shift_sfc <- st_point(shift) %>% st_sfc()

  geom_shifted <- (data_geom + shift_sfc) %>% 
    st_set_crs(temp_crs)

  if (keep_data == TRUE){
    data_sf <- st_drop_geometry(data_sf) 
    data_sf$geometry <- geom_shifted
    return(st_as_sf(data_sf))
  } else {
    return(geom_shifted)
  }

}