
#' @export
img_bw <- function(img, dither = TRUE){
  image_quantize(img, max = 2, colorspace = 'gray', dither = dither)
}


#' @export
img_grayscale <- function(im){
  image_convert(im, colorspace = "Gray")
}

