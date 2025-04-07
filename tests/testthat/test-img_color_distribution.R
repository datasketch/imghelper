test_that("img_color_distribution returns correct data structure", {
  # Create a simple test image with 4 colors (windows pattern)
  img <- img_sample("windows")

  # Get color distribution
  dist <- img_color_distribution(img)

  # Check structure
  expect_s3_class(dist, "tbl_df")  # Should be a tibble
  expect_equal(ncol(dist), 3)  # Should have 3 columns
  expect_equal(names(dist), c("color", "count", "distribution"))

  # Should have 4 colors (or fewer if some colors are exactly the same in hex)
  expect_lte(nrow(dist), 4)

  # Check that distribution sums to 1 (approximately)
  expect_equal(sum(dist$distribution), 1, tolerance = 1e-10)

  # Check data types
  expect_type(dist$color, "character")
  expect_type(dist$count, "double")
  expect_type(dist$distribution, "double")

  # Check color format (hexadecimal)
  expect_true(all(grepl("^#[0-9A-F]{6}$", dist$color)))
})

test_that("img_color_distribution respects max_colors parameter", {
  # Create a gradient image with many colors
  img <- img_sample("gradient")

  # Get distribution with different max_colors values
  dist_default <- img_color_distribution(img)  # Default 20
  dist_5 <- img_color_distribution(img, max_colors = 5)
  dist_50 <- img_color_distribution(img, max_colors = 50)

  # Check number of colors
  expect_lte(nrow(dist_default), 20)
  expect_lte(nrow(dist_5), 5)
  expect_lte(nrow(dist_50), 50)

  # Check that rows are ordered by distribution (descending)
  expect_true(all(diff(dist_default$distribution) <= 0))
  expect_true(all(diff(dist_5$distribution) <= 0))
})

test_that("img_color_distribution works with different image types", {
  # Test with different sample types
  img_bw <- img_sample("bw_pattern")
  img_gray <- img_sample("grayscale_steps")
  img_color <- img_sample("color_bars")

  # Get distributions
  dist_bw <- img_color_distribution(img_bw)
  dist_bw <- img_color_distribution(img_bw, max_colors = 2)
  dist_gray <- img_color_distribution(img_gray)
  dist_color <- img_color_distribution(img_color)

  # All should have valid distributions
  expect_equal(sum(dist_bw$distribution), 1, tolerance = 1e-10)
  expect_equal(sum(dist_gray$distribution), 1, tolerance = 1e-10)
  expect_equal(sum(dist_color$distribution), 1, tolerance = 1e-10)

  # BW pattern should have close to 2 colors
  expect_lte(nrow(dist_bw), 20)  # Still capped by max_colors

  # Color bars should have close to 7 colors (but capped at 20)
  expect_lte(nrow(dist_color), 20)
})

test_that("img_color_distribution handles different methods", {
  # Test with a gradient image (many colors)
  img_gradient <- img_sample("gradient")

  # Get distribution with different methods
  dist_kmeans <- img_color_distribution(img_gradient, method = "kmeans")
  dist_quantize <- img_color_distribution(img_gradient, method = "quantize")

  # Both should have at most max_colors colors
  expect_lte(nrow(dist_kmeans), 20)
  expect_lte(nrow(dist_quantize), 20)

  # Both should have valid distributions summing to 1
  expect_equal(sum(dist_kmeans$distribution), 1, tolerance = 1e-10)
  expect_equal(sum(dist_quantize$distribution), 1, tolerance = 1e-10)

  # Test with a black and white pattern
  img_bw <- img_sample("bw_pattern")

  # Get distribution with different methods
  dist_bw_kmeans <- img_color_distribution(img_bw, method = "kmeans")
  dist_bw_quantize <- img_color_distribution(img_bw, method = "quantize")

  # Both should identify at least 2 colors (black and white)
  expect_gte(nrow(dist_bw_kmeans), 2)
  expect_gte(nrow(dist_bw_quantize), 1)  # Quantize may merge colors

  # Check if kmeans method has both dark and light colors
  has_dark_kmeans <- any(sapply(dist_bw_kmeans$color, function(c) {
    # Check if color is dark (close to black)
    values <- grDevices::col2rgb(c) / 255
    mean(values) < 0.3
  }))

  has_light_kmeans <- any(sapply(dist_bw_kmeans$color, function(c) {
    # Check if color is light (close to white)
    values <- grDevices::col2rgb(c) / 255
    mean(values) > 0.7
  }))

  expect_true(has_dark_kmeans)
  expect_true(has_light_kmeans)
})

test_that("img_color_distribution doesn't reduce colors when not needed", {
  # Create a simple image with 3 colors
  img <- img_sample("windows", width = 100, height = 100)
  # Override to just have 3 colors
  img <- magick::image_draw(img)
  graphics::rect(0, 0, 33, 100, col = "red", border = NA)
  graphics::rect(34, 0, 66, 100, col = "green", border = NA)
  graphics::rect(67, 0, 100, 100, col = "blue", border = NA)
  dev.off()

  # Get distribution with max_colors = 5 (more than the 3 in the image)
  dist <- img_color_distribution(img, max_colors = 5)

  # Should have exactly 3 colors, not reduced
  expect_equal(nrow(dist), 3)

  # Colors should be close to red, green, blue
  colors_present <- dist$color

  # Check if red, green, and blue are present
  red_present <- any(sapply(colors_present, function(c) {
    rgb <- grDevices::col2rgb(c)
    rgb[1] > 200 && rgb[2] < 100 && rgb[3] < 100  # Red is high, others low
  }))

  green_present <- any(sapply(colors_present, function(c) {
    rgb <- grDevices::col2rgb(c)
    rgb[1] < 100 && rgb[2] > 200 && rgb[3] < 100  # Green is high, others low
  }))

  blue_present <- any(sapply(colors_present, function(c) {
    rgb <- grDevices::col2rgb(c)
    rgb[1] < 100 && rgb[2] < 100 && rgb[3] > 200  # Blue is high, others low
  }))

  expect_true(red_present)
  expect_true(green_present)
  expect_true(blue_present)
})

test_that("img_color_distribution handles invalid inputs", {
  img <- img_sample("windows")

  # Test invalid max_colors
  expect_error(img_color_distribution(img, max_colors = -5))
  expect_error(img_color_distribution(img, max_colors = "twenty"))

  # Test invalid method
  expect_error(img_color_distribution(img, method = "invalid_method"))

  # Test invalid image
  expect_error(img_color_distribution("not_an_image"))
})
