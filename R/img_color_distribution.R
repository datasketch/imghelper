#' Get color distribution from an image
#' @param img A magick-image object
#' @param max_colors Integer. Maximum number of colors to include in the distribution (default: 20)
#' @param method Character. Method to use for color reduction if needed: "quantize" (default) or "kmeans"
#' @return A tibble with columns:
#'   - color: Hex color code
#'   - count: Number of pixels with this color
#'   - distribution: Proportion of pixels with this color
#' @export
img_color_distribution <- function(img, max_colors = 20, method = c("quantize", "kmeans")) {
  # Check if required packages are available
  if (!requireNamespace("magick", quietly = TRUE) ||
      !requireNamespace("tibble", quietly = TRUE)) {
    stop("Packages 'magick' and 'tibble' are required")
  }

  # Check if input is a magick image
  tryCatch({
    magick::image_info(img)
  }, error = function(e) {
    stop("Input must be a magick image object")
  })

  # Validate max_colors parameter
  if (!is.numeric(max_colors) || max_colors < 1) {
    stop("max_colors must be a positive integer")
  }

  # Validate method parameter
  method <- match.arg(method)

  # Convert to RGB to ensure consistent color representation
  img_rgb <- magick::image_convert(img, colorspace = "sRGB")
  
  # Scale down large images for performance regardless of method
  img_info <- magick::image_info(img_rgb)
  if (img_info$width > 200 || img_info$height > 200) {
    img_rgb <- img_scale(img_rgb, w = 200)
  }

  # Use the helper function to quickly check if color reduction is needed
  needs_reduction <- img_has_more_than_n_colors(img_rgb, max_colors)

  # Reduce colors if needed
  if (needs_reduction) {
    if (method == "quantize") {
      # Use the quantize method
      img_rgb <- img_quantize(img_rgb, colors = max_colors)
    } else if (method == "kmeans") {
      # Flag that we need to use kmeans
      use_kmeans <- TRUE
    }
  }

  # Get raw pixel data
  img_data <- magick::image_data(img_rgb)

  # Get dimensions
  dims <- dim(img_data)

  # Total number of pixels
  n_pixels <- dims[2] * dims[3]

  # Create a data frame of RGB values
  df_rgb <- data.frame(
    r = as.integer(img_data[1,,]),
    g = as.integer(img_data[2,,]),
    b = as.integer(img_data[3,,])
  )

  # Apply kmeans if needed (and if that's the selected method)
  if (needs_reduction && exists("use_kmeans") && use_kmeans) {
    # Use k-means clustering for color reduction
    if (!requireNamespace("stats", quietly = TRUE)) {
      stop("Package 'stats' is required for kmeans method")
    }

    # Create a matrix for kmeans clustering
    rgb_matrix <- as.matrix(df_rgb)

    # Run k-means clustering
    set.seed(42)  # For reproducibility
    kmeans_result <- stats::kmeans(rgb_matrix, centers = max_colors, nstart = 5)

    # Get cluster centers (these will be our colors)
    centers <- round(kmeans_result$centers)

    # Replace each pixel with its cluster center color
    for (i in 1:nrow(df_rgb)) {
      cluster <- kmeans_result$cluster[i]
      df_rgb$r[i] <- centers[cluster, 1]
      df_rgb$g[i] <- centers[cluster, 2]
      df_rgb$b[i] <- centers[cluster, 3]
    }
  }

  # Convert RGB values to hex colors
  pixel_colors <- sprintf("#%02X%02X%02X", df_rgb$r, df_rgb$g, df_rgb$b)

  # Count occurrences of each color
  color_counts <- table(pixel_colors)

  # Calculate distribution (proportion of each color)
  color_distribution <- as.numeric(color_counts) / n_pixels

  # Create tibble with colors and their distribution
  result <- tibble::tibble(
    color = names(color_counts),
    count = as.numeric(color_counts),
    distribution = color_distribution
  )

  # Sort by distribution (descending)
  result <- result[order(-result$distribution), ]

  # Ensure we only return max_colors colors at most
  if (nrow(result) > max_colors) {
    result <- result[1:max_colors, ]
    # Renormalize distribution
    result$distribution <- result$distribution / sum(result$distribution)
  }

  return(result)
}

