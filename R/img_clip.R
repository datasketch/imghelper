#' Clip an image with different shapes or custom mask
#' @param img A magick-image object
#' @param shape Character. Shape of the mask ("circle", "rounded", "frame", "diamond", "oval")
#' @param radius Numeric. Shape modifier (0-1):
#'   - circle: ignored
#'   - rounded: corner radius as proportion
#'   - frame: frame thickness as proportion
#'   - diamond: diamond size as proportion (1=smallest)
#'   - oval: height/width ratio (1 = circle)
#' @param mask Optional magick-image object to use as mask. If provided, shape and radius are ignored
#' @return A magick-image object
#' @export
img_clip <- function(img, shape = c("circle", "rounded", "frame", "diamond", "oval"),
                     radius = 0.1, mask = NULL) {
  if (!inherits(img, "magick-image")) {
    stop("Input must be a magick-image object")
  }

  info <- magick::image_info(img)

  # Validate and process custom mask if provided
  if (!is.null(mask)) {
    if (!inherits(mask, "magick-image")) {
      stop("mask must be a magick-image object")
    }

    mask_info <- magick::image_info(mask)
    if (mask_info$width != info$width || mask_info$height != info$height) {
      stop("mask dimensions must match image dimensions")
    }
  } else {
    # Process shape-based mask
    shape <- match.arg(shape)
    if (!is.numeric(radius) || radius < 0 || radius > 1) {
      stop("radius must be a number between 0 and 1")
    }

    if (info$width != info$height && shape != "oval") {
      stop("Image must be square for all shapes except 'oval'")
    }

    mask <- img_mask(info$width, shape, radius)
  }

  # Convert mask black to transparent and apply
  mask <- magick::image_transparent(mask, "black")
  result <- magick::image_convert(img, 'png')
  result <- magick::image_composite(result, mask, operator = 'CopyOpacity')

  result
}

