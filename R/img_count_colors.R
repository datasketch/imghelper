
#' @export
img_count_colors <- function(img) {
  # Check if input is a magick image
  tryCatch({
    magick::image_info(img)
  }, error = function(e) {
    stop("Input must be a magick image object")
  })

  # Convert to RGB to ensure consistent color representation
  img_rgb <- magick::image_convert(img, colorspace = "sRGB")

  # Get raw pixel data
  img_data <- magick::image_data(img_rgb)

  # Get dimensions
  dims <- dim(img_data)

  # Create a vector to store color strings
  n_pixels <- dims[2] * dims[3]
  pixel_colors <- character(n_pixels)

  # Create color strings by combining R, G, B values
  for (i in 1:n_pixels) {
    # Calculate row and column in the image
    col <- ((i - 1) %% dims[2]) + 1
    row <- ((i - 1) %/% dims[2]) + 1

    # Get R, G, B values for this pixel
    r <- as.integer(img_data[1, col, row])
    g <- as.integer(img_data[2, col, row])
    b <- as.integer(img_data[3, col, row])

    # Create color string
    pixel_colors[i] <- paste(r, g, b, sep = "-")
  }

  # Count unique colors
  unique_color_count <- length(unique(pixel_colors))

  return(unique_color_count)
}

