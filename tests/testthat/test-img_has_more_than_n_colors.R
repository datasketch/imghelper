test_that("img_has_more_than_n_colors correctly identifies color count", {
  # Create a simple test image with 4 colors (windows pattern)
  img_4_colors <- img_sample("windows")
  
  # Create a gradient with many colors
  img_many_colors <- img_sample("gradient")
  
  # Create a black and white image
  img_bw <- img_sample("bw_pattern")
  
  # Check function results
  expect_false(img_has_more_than_n_colors(img_4_colors, n = 5))
  expect_true(img_has_more_than_n_colors(img_4_colors, n = 2))
  expect_true(img_has_more_than_n_colors(img_many_colors, n = 10))
  expect_false(img_has_more_than_n_colors(img_bw, n = 5))
})

test_that("img_has_more_than_n_colors processes images of different sizes", {
  # Create images of different sizes
  small_img <- img_sample("windows", width = 50, height = 50)
  medium_img <- img_sample("windows", width = 100, height = 100)
  large_img <- img_sample("windows", width = 500, height = 500)
  
  # The function should resize large images for processing
  # All should give the same result
  expect_equal(
    img_has_more_than_n_colors(small_img, n = 3),
    img_has_more_than_n_colors(medium_img, n = 3)
  )
  
  expect_equal(
    img_has_more_than_n_colors(medium_img, n = 3),
    img_has_more_than_n_colors(large_img, n = 3)
  )
})

test_that("img_has_more_than_n_colors handles invalid inputs", {
  # Test invalid image
  expect_error(img_has_more_than_n_colors("not_an_image"))
  
  # Create a valid image
  img <- img_sample("windows")
  
  # Test invalid n
  expect_error(img_has_more_than_n_colors(img, n = "five"))
}) 