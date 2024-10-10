
#' @export
img_write <- function(img, path = NULL, format = NULL){
  if(!is.null(path)){
    ext <- tools::file_ext(path)
  } else {
    ext <- "jpeg"
    path <- tempfile(fileext = paste0(".jpeg"))
  }
  magick::image_write(img, path = path, format = img_format(img))
}
