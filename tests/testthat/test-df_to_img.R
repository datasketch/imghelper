# Tests
test_that("df_to_img handles RGB data", {
  # Create test RGB data frame
  df <- expand.grid(
    x = 1:100,
    y = 1:100,
    stringsAsFactors = FALSE
  )
  df$R <- 0.9
  df$G <- 0.9
  df$B <- 0.1

  # Convert to image
  img <- df_to_img(df)

  # Check result
  expect_s3_class(img, "magick-image")
  info <- magick::image_info(img)
  expect_equal(info$width, 100)
  expect_equal(info$height, 100)
})

test_that("df_to_img handles grayscale data", {

  # Create test image with bottom right diagonal
  df <- expand.grid(
    x = 1:100,
    y = 1:100,
    stringsAsFactors = FALSE
  )
  df$I <- 1  # white background
  # Create black diagonal (bottom left)
  df$I[df$x <= df$y] <- 0  # black diagonal
  img <- df_to_img(df)
  img



  # Create test grayscale data frame
  df <- expand.grid(
    x = 1:100,
    y = 1:100,
    stringsAsFactors = FALSE
  )
  df$I <- 128

  # Convert to image
  img <- df_to_img(df)

  # Check result
  expect_s3_class(img, "magick-image")
  info <- magick::image_info(img)
  expect_equal(info$width, 100)
  expect_equal(info$height, 100)
})

test_that("df_to_img handles invalid inputs", {
  # Missing coordinates
  df_bad <- data.frame(R = 1, G = 1, B = 1)
  expect_error(df_to_img(df_bad))

  # Missing color information
  df_bad <- data.frame(x = 1, y = 1)
  expect_error(df_to_img(df_bad))
})

