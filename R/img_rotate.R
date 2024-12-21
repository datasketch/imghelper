
#' Rotate an image
#' @param img A magick-image object
#' @param degrees Numeric. Rotation angle in degrees (clockwise)
#' @param background Character. Color to use for background when rotating creates empty space. Default "transparent"
#' @param expand Logical. If TRUE, expands the output image to hold the entire rotated image. Default TRUE
#' @return A magick-image object
#' @export
img_rotate <- function(img, degrees = 90) {
  if (!inherits(img, "magick-image")) {
    stop("Input must be a magick-image object")
  }

  if (!is.numeric(degrees)) {
    stop("degrees must be a numeric value")
  }

  magick::image_rotate(img, degrees)
}

