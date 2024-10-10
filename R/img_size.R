



#' @export
img_ratio <- function(im){
  info <- image_info(im)
  info$height/info$width
}

#' @export
img_size <- function(m){
  info <- image_info(m)
  c(info$width, info$height)
}

