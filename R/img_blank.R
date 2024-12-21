

#' Create a blank square image
#' @param size Integer. Width and height of the square image
#' @param color Character. Color name or hex code
#' @return A magick-image object
#' @export
img_blank <- function(width = 200, height = NULL, color = "black") {
  if (!is.numeric(width) || width < 1) {
    stop("size must be a positive number")
  }

  height <- height %||% width

  magick::image_blank(width, height, color)
}



