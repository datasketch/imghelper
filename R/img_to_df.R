#' Convert image to data frame of pixel values
#' @param img A magick-image object
#' @return A data frame with columns:
#'   - x,y: pixel coordinates
#'   - R,G,B: RGB values for color images (0-1)
#'   - I: intensity value for grayscale images (0-1)
#' @export
img_to_df <- function(img) {
  if (!inherits(img, "magick-image")) {
    stop("Input must be a magick-image object")
  }

  # Get image info
  info <- magick::image_info(img)

  # Convert to array of pixels
  pixels <- magick::image_data(img, 'rgb')

  # Check if grayscale (all channels identical)
  is_gray <- all(pixels[1,,] == pixels[2,,]) && all(pixels[1,,] == pixels[3,,])

  # Create coordinate grid
  coords <- expand.grid(
    x = seq_len(info$width),
    y = seq_len(info$height)
  )

  if (is_gray) {
    # For grayscale, just use one channel
    df <- data.frame(
      coords,
      I = as.numeric(as.vector(pixels[1,,])) / 255
    )
  } else {
    # For RGB, use all channels
    df <- data.frame(
      coords,
      R = as.numeric(as.vector(pixels[1,,])) / 255,
      G = as.numeric(as.vector(pixels[2,,])) / 255,
      B = as.numeric(as.vector(pixels[3,,])) / 255
    )
  }

  df
}
