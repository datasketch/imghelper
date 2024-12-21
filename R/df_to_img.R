#' Convert data frame of pixel values to image
#' @param df A data frame with columns:
#'   - x,y: pixel coordinates (required)
#'   - Either:
#'     - R,G,B: RGB values for color images (0-1)
#'     - I: intensity value for grayscale images (0-1)
#' @return A magick image object
#' @export
df_to_img <- function(df) {
  # Validate required columns
  if (!all(c("x", "y") %in% names(df))) {
    stop("Data frame must contain 'x' and 'y' columns")
  }

  # Determine if RGB or grayscale
  is_rgb <- all(c("R", "G", "B") %in% names(df))
  is_gray <- "I" %in% names(df)

  if (!is_rgb && !is_gray) {
    stop("Data frame must contain either RGB (R,G,B) or intensity (I) columns")
  }

  # Value range validation
  if (is_rgb) {
    if (any(df$R < 0 | df$R > 1 | df$G < 0 | df$G > 1 | df$B < 0 | df$B > 1)) {
      stop("RGB values must be between 0 and 1")
    }
  } else {
    if (any(df$I < 0 | df$I > 1)) {
      stop("Intensity values must be between 0 and 1")
    }
  }

  # Get dimensions
  width <- max(df$x)
  height <- max(df$y)

  # Order data frame by y, x for proper pixel arrangement
  df <- df[order(df$y, df$x), ]

  if (is_rgb) {
    # Create raw array for RGB
    pixels <- array(as.raw(0), dim = c(3, width, height))

    # Fill in RGB values as raw
    for(i in 1:height) {
      rows <- ((i-1) * width + 1):(i * width)
      pixels[1,,i] <- as.raw(round(df$R[rows] * 255))
      pixels[2,,i] <- as.raw(round(df$G[rows] * 255))
      pixels[3,,i] <- as.raw(round(df$B[rows] * 255))
    }
  } else {
    # Create raw array for grayscale
    pixels <- array(as.raw(0), dim = c(3, width, height))

    # Fill in intensity values
    for(i in 1:height) {
      rows <- ((i-1) * width + 1):(i * width)
      # Direct conversion to raw
      intensity_val <- round(df$I[rows] * 255)
      raw_intensity <- as.raw(intensity_val)
      pixels[1,,i] <- raw_intensity
      pixels[2,,i] <- raw_intensity
      pixels[3,,i] <- raw_intensity
    }
  }

  magick::image_read(pixels)
}
