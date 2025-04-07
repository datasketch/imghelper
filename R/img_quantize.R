#' Quantize an image to reduce the number of colors
#' @param img A magick-image object
#' @param colors Integer. Number of colors to quantize the image to (default: 16)
#' @param colorspace Character. Colorspace to use for quantization:
#'   "sRGB", "gray", "HCL", "HSB", "HSL", "Lab", "RGB" (default: "sRGB")
#' @param dither Logical. Whether to apply dithering (default: FALSE)
#' @param treedepth Integer. Maximum depth of the quantization tree (default: 0 for auto)
#' @return A magick-image object with reduced colors
#' @export
img_quantize <- function(img, colors = 16, colorspace = "sRGB", dither = FALSE, treedepth = 0) {
  if (!inherits(img, "magick-image")) {
    stop("Input must be a magick-image object")
  }

  if (!is.numeric(colors) || colors < 1 || colors > 256) {
    stop("colors must be a number between 1 and 256")
  }

  valid_colorspaces <- c("sRGB", "gray", "HCL", "HSB", "HSL", "Lab", "RGB")
  if (!colorspace %in% valid_colorspaces) {
    stop(paste("colorspace must be one of:", paste(valid_colorspaces, collapse = ", ")))
  }

  if (!is.logical(dither)) {
    stop("dither must be a logical value (TRUE/FALSE)")
  }

  if (!is.numeric(treedepth) || treedepth < 0 || treedepth > 8) {
    stop("treedepth must be a number between 0 and 8")
  }

  magick::image_quantize(img,
                         max = colors,
                         colorspace = colorspace,
                         dither = dither,
                         treedepth = treedepth)
}
