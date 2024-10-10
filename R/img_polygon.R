
#' @export
img_sf_polygon <- function(path){

  r <- img_raster(path)
  p <- terra::as.polygons(r, values=TRUE)

  # Convert terra object to sf object
  sf_object <- sf::st_as_sf(p)

  sf_object

}

