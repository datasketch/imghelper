
test_that("img_clips", {

  img <- img_blank(400, color = "blue")
  clip <- img_clip(img, shape = "circle", radius = 1)
  clip

  img <- img_blank(400, color = "green")
  mask <- img_mask(400, shape = "diamond")
  clip <- img_clip(img, mask = mask)
  clip


})

test_that("img_clip handles custom masks", {
  img <- img_blank(100, color = "blue")
  custom_mask <- img_blank(100, color = "white")

  # Test valid custom mask
  expect_s3_class(img_clip(img, mask = custom_mask), "magick-image")

  # Test invalid mask dimensions
  wrong_size_mask <- img_blank(200, color = "white")
  expect_error(img_clip(img, mask = wrong_size_mask))

  # Test invalid mask type
  expect_error(img_clip(img, mask = "not_a_mask"))
})

test_that("img_clip works with both mask types", {
  img <- img_blank(100, color = "blue")
  custom_mask <- img_blank(100, color = "white")

  # Test with shape
  shape_clip <- img_clip(img, "circle", 1)
  expect_s3_class(shape_clip, "magick-image")

  # Test with custom mask
  mask_clip <- img_clip(img, mask = custom_mask)
  expect_s3_class(mask_clip, "magick-image")
})
