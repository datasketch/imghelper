test_that("img_quantize reduces colors properly", {
  # Create a test image with multiple colors
  img <- img_blank(100, 100, color = "white")

  # Draw some colored shapes on it
  img <- magick::image_draw(img)
  graphics::rect(0, 0, 50, 50, col = "red", border = NA)
  graphics::rect(50, 0, 100, 50, col = "blue", border = NA)
  graphics::rect(0, 50, 50, 100, col = "green", border = NA)
  graphics::rect(50, 50, 100, 100, col = "yellow", border = NA)
  dev.off()

  # Apply quantization with different parameters
  quantized_8 <- img_quantize(img, colors = 8)
  quantized_2 <- img_quantize(img, colors = 2)
  quantized_gray <- img_quantize(img, colors = 8, colorspace = "gray")
  quantized_dither <- img_quantize(img, colors = 3, dither = TRUE)

  # Check that all results are valid magick images
  expect_s3_class(quantized_8, "magick-image")
  expect_s3_class(quantized_2, "magick-image")
  expect_s3_class(quantized_gray, "magick-image")
  expect_s3_class(quantized_dither, "magick-image")

  # Check that the number of colors is reduced
  # We can convert to data frame and count unique colors
  df_original <- img_to_df(img)
  df_quantized_2 <- img_to_df(quantized_2)

  # Count unique RGB combinations
  original_colors <- df_original |>
    dplyr::select(R, G, B) |>
    dplyr::distinct() |>
    nrow()

  quantized_colors <- df_quantized_2 %>%
    dplyr::select(R, G, B) %>%
    dplyr::distinct() %>%
    nrow()

  # The quantized image should have fewer colors
  expect_lte(quantized_colors, original_colors)

  # With colors=2, we should have at most 2 colors
  expect_lte(quantized_colors, 2)
})

test_that("img_quantize handles invalid inputs", {
  img <- img_blank(100)

  # Test invalid inputs
  expect_error(img_quantize("not_an_image"))
  expect_error(img_quantize(img, colors = 0))
  expect_error(img_quantize(img, colors = 1000))
  expect_error(img_quantize(img, colorspace = "invalid"))
  expect_error(img_quantize(img, dither = "yes"))
  expect_error(img_quantize(img, treedepth = 10))
})
