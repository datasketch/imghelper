#' Quickly check if an image has more than n colors
#' @param img A magick-image object
#' @param n Number of colors threshold (default: 20)
#' @return Logical. TRUE if image has more than n colors, FALSE otherwise
#' @export
img_has_more_than_n_colors <- function(img, n = 20) {
  # Check if input is a magick image
  tryCatch({
    magick::image_info(img)
  }, error = function(e) {
    stop("Input must be a magick image object")
  })
  
  # Use sampling to speed up the check
  # First, resize the image to reduce pixel count
  # A 100x100 image has 10,000 pixels, which should be enough for a good estimate
  max_dimension <- 100
  img_info <- magick::image_info(img)
  if (img_info$width > max_dimension || img_info$height > max_dimension) {
    img <- magick::image_resize(img, paste0(max_dimension, "x", max_dimension))
  }
  
  # Convert to RGB
  img_rgb <- magick::image_convert(img, colorspace = "sRGB")
  
  # Get pixel data
  img_data <- magick::image_data(img_rgb)
  
  # Get dimensions
  dims <- dim(img_data)
  
  # Sample pixels (take every 10th pixel to speed things up further)
  sampled_r <- as.integer(img_data[1, seq(1, dims[2], by = 10), seq(1, dims[3], by = 10)])
  sampled_g <- as.integer(img_data[2, seq(1, dims[2], by = 10), seq(1, dims[3], by = 10)])
  sampled_b <- as.integer(img_data[3, seq(1, dims[2], by = 10), seq(1, dims[3], by = 10)])
  
  # Combine into color values
  sampled_colors <- paste(sampled_r, sampled_g, sampled_b, sep = "-")
  
  # Count unique colors in the sample
  unique_color_count <- length(unique(sampled_colors))
  
  # If even the sample has more than n colors, the full image definitely does
  return(unique_color_count > n)
} 