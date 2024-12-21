

test_that("img_rotate handles basic rotation", {
  img <- img_blank(100, color = "blue")

  # Test arbitrary angle
  rotated <- img_rotate(img, 45)
  expect_s3_class(rotated, "magick-image")

  # Test with background color
  rotated <- img_rotate(img, 45, background = "red")
  expect_s3_class(rotated, "magick-image")

  # Test without expand
  rotated <- img_rotate(img, 45, expand = FALSE)
  expect_s3_class(rotated, "magick-image")
})

test_that("img_rotate handles invalid inputs", {
  img <- img_blank(100)

  expect_error(img_rotate("not_an_image"))
  expect_error(img_rotate(img, "invalid"))
})

