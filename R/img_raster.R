

#' @export
img_raster <- function(path, negate = FALSE){
  ext <- tools::file_ext(path)
  img <- img_read(path)
  ext <- img_format(img)
  if(ext %in% c("jpeg", "jpg", "png")){
    img <- img_negate(img)
  }
  if(negate){
    img <- img_negate(img)
  }
  file <- img_write(img)
  r <- terra::rast(file)
  #plot(r)

  if(ext %in% c("jpeg", "jpg") || length(as.list(r)) == 1){
    r2 <- r
  }else{
    r2 <- r[[2]]
  }

  #plot(r2)
  # Convert to logical raster
  r2 <- terra::ifel(r2 < 255/2, NA, 1)
  unlink(file)
  r2
}
