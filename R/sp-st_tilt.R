#' @title st_tilt
#'
#' @description Tilt an sf object by a specified angle
#' @param data_sf An sf object 
#' @param angle (angle to tilt, numeric)
#' @param base_sf If supplied, `data_sf` is shifted to the centroid of this sf object (default: NULL)
#' @param keep_data keep the non-geometry data part of the data (default: TRUE)
#' @export
#' @examples

st_tilt <- function(data_sf, angle, base_sf = NULL, keep_data = TRUE) {

  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  if ("sf" %in% class(base_sf)) {
    wf_bbox <- st_bbox(base_sf) %>% st_as_sfc()
  } else {
    wf_bbox <- st_bbox(data_sf) %>% st_as_sfc()
  }
  
  base_point <- st_centroid(wf_bbox)
  data_geom <- st_geometry(data_sf)

  data_tilted <- ((data_geom - base_point) * rot(angle / 180 * pi) + base_point) %>%
    st_set_crs(st_crs(data_sf)) 

  if (keep_data == TRUE) {

    data_sf$geometry <- data_tilted 
    return(data_sf)

  } else {

    return(data_tilted)

  }
  
}
