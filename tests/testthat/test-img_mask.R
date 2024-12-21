
test_that("masks", {

  img_mask(400, "circle")
  img_mask(400, "circle", 0.5)
  img_mask(400, "rounded", 0.2)
  img_mask(400, "rounded", 0.5)
  img_mask(400, "rounded", 1)
  img_mask(400, "frame", 0.5)
  img_mask(400, "diamond", 0.2)
  img_mask(400, "diamond", 0.5)
  img_mask(400, "diamond", 1)
  # img_mask(400, "diamond", 2) # todo should work and be bigger
  img_mask(400, "oval", 0.2)
  img_mask(400, "oval", 0.5)
  img_mask(400, "oval", 1)

  ## TODO
  # Masks with images

})


test_that("img_mask creates valid masks for all shapes", {
  shapes <- c("circle", "rounded", "frame", "diamond", "oval")

  for (shape in shapes) {
    expect_s3_class(img_mask(100, shape, 0.5), "magick-image")
  }

  # Test specific error conditions
  expect_error(img_mask(100, "invalid"))
  expect_error(img_mask(100, "frame", -0.1))
  expect_error(img_mask(-100, "diamond"))
})

test_that("img_mask handles edge cases", {
  # Test minimum radius
  expect_s3_class(img_mask(100, "frame", 0.01), "magick-image")

  # Test maximum radius
  expect_s3_class(img_mask(100, "diamond", 1), "magick-image")

  # Test oval with different ratios
  expect_s3_class(img_mask(100, "oval", 0.5), "magick-image")
  expect_s3_class(img_mask(100, "oval", 1), "magick-image")
})

