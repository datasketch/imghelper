test_that("img_sample creates various sample images", {
  # Test all sample types with default parameters
  sample_types <- c("windows", "checkerboard", "circles", "gradient", 
                   "stripes", "color_bars", "grayscale_steps", "bw_pattern")
  
  for (type in sample_types) {
    img <- img_sample(type)
    expect_s3_class(img, "magick-image")
    
    # Check dimensions
    info <- magick::image_info(img)
    expect_equal(info$width, 200)
    expect_equal(info$height, 200)
  }
  
  # Test custom dimensions
  custom_size <- img_sample("windows", width = 300, height = 400)
  info <- magick::image_info(custom_size)
  expect_equal(info$width, 300)
  expect_equal(info$height, 400)
  
  # Test custom colors
  custom_colors <- img_sample("windows", colors = c("purple", "orange", "pink", "brown"))
  expect_s3_class(custom_colors, "magick-image")
  
  # Test circles with different numbers of shapes
  circles_one <- img_sample("circles", n_shapes = 1)
  circles_many <- img_sample("circles", n_shapes = 5)
  expect_s3_class(circles_one, "magick-image")
  expect_s3_class(circles_many, "magick-image")
})

test_that("img_sample demos work with other imghelper functions", {
  # Create temp directory for test output
  temp_dir <- tempfile("img_sample_test")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # 1. Quantize a color_bars sample
  color_bars <- img_sample("color_bars")
  quantized <- img_quantize(color_bars, colors = 3)
  expect_s3_class(quantized, "magick-image")
  
  # Write and confirm file exists
  output_file <- file.path(temp_dir, "color_bars_quantized.png")
  img_write(quantized, output_file)
  expect_true(file.exists(output_file))
  
  # 2. Rotate a checkerboard
  checkerboard <- img_sample("checkerboard")
  rotated <- img_rotate(checkerboard, degrees = 45)
  expect_s3_class(rotated, "magick-image")
  
  # 3. Clip a gradient to circle
  gradient <- img_sample("gradient")
  clipped <- img_clip(gradient, shape = "circle")
  expect_s3_class(clipped, "magick-image")
  
  # 4. Apply scanned effect to stripes
  stripes <- img_sample("stripes")
  scanned <- img_make_scanned(stripes, rotation_angle = 2, wrinkle_intensity = 30)
  expect_s3_class(scanned, "magick-image")
})

test_that("img_sample handles invalid inputs", {
  # Test invalid type
  expect_error(img_sample("invalid_type"))
  
  # Test invalid dimensions
  expect_error(img_sample("windows", width = -100))
}) 