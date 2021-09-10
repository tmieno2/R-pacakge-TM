#' Reproject an sf object to UTM
#'
#' This function allows you to express your love of cats.
#' @param sf_obj An sf object 
#' @export
#' @examples

st_utm <- function(sf_obj) {

  # Function to get UTM Zone from mean longitude:
  long2UTM <- function(long) {
    (floor((long + 180) / 6) %% 60) + 1
  }

  # Check if the object class is 'sf':
  obj_c <- class(sf_obj)[1]

  if (obj_c == "sf") {
    # In case the object has no projectin assigned,
    #  assume it to geographic WGS84 :

    if (is.na(sf::st_crs(sf_obj))) {
      stop("The sf object does not have any CRS assigned.")
    }

    # Get the center longitude in degrees:
    bb <- 
      sf::st_as_sfc(sf::st_bbox(sf_obj)) %>% 
      sf::st_transform(bb, sf::st_crs(4326))

    # Get UTM Zone from mean longitude:
    utmzone <- long2UTM(mean(sf::st_bbox(bb)[c(1, 3)]))

    # Get the hemisphere based on the latitude:
    NS <- 100 * (6 + (mean(sf::st_bbox(bb)[c(2, 4)]) < 0))

    # Add all toghether to get the EPSG code:
    projutm <- sf::st_crs(32000 + NS + utmzone)

    # Reproject data:
    sf_obj <- sf::st_transform(sf_obj, projutm)

    return(sf_obj)

  } else {

    options(error = NULL)
    stop("Object class is not 'sf', please insert a sf object!")

  }

}
