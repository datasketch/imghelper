library(imghelper)

# Create a directory for the sample images
dir.create("sample_images", showWarnings = FALSE)

# Generate all sample types with default parameters
sample_types <- c("windows", "checkerboard", "circles", "gradient", 
                 "stripes", "color_bars", "grayscale_steps", "bw_pattern")

# Create and save each sample type
for (type in sample_types) {
  cat("Creating", type, "sample...\n")
  img <- img_sample(type)
  
  # Save the image
  output_path <- file.path("sample_images", paste0(type, ".png"))
  img_write(img, output_path)
  
  cat("Saved to", output_path, "\n")
}

# Show some customizations

# 1. Windows pattern with custom colors
custom_windows <- img_sample("windows", colors = c("purple", "teal", "orange", "pink"))
img_write(custom_windows, "sample_images/windows_custom.png")

# 2. Gradient with wider aspect ratio
wide_gradient <- img_sample("gradient", width = 400, height = 200)
img_write(wide_gradient, "sample_images/gradient_wide.png")

# 3. Checkerboard with different size
large_checkerboard <- img_sample("checkerboard", width = 400)
img_write(large_checkerboard, "sample_images/checkerboard_large.png")

# 4. Many concentric circles
concentric <- img_sample("circles", n_shapes = 10, 
                        colors = rainbow(10))
img_write(concentric, "sample_images/circles_concentric.png")

# Combine with other imghelper functions

# 1. Quantized color bars (reduce to 3 colors)
color_bars <- img_sample("color_bars")
quantized <- img_quantize(color_bars, colors = 3)
img_write(quantized, "sample_images/color_bars_quantized.png")

# 2. Rotated checkerboard
checkerboard <- img_sample("checkerboard")
rotated <- img_rotate(checkerboard, degrees = 45)
img_write(rotated, "sample_images/checkerboard_rotated.png")

# 3. Circular clip on gradient
gradient <- img_sample("gradient")
clipped <- img_clip(gradient, shape = "circle")
img_write(clipped, "sample_images/gradient_circular.png")

# 4. Apply "scanned" effect to stripes
stripes <- img_sample("stripes")
scanned <- img_make_scanned(stripes, rotation_angle = 2, wrinkle_intensity = 30)
img_write(scanned, "sample_images/stripes_scanned.png")

cat("\nAll sample images created in the 'sample_images' directory.\n")
cat("Run the following to see a list of generated files:\n")
cat("list.files('sample_images', pattern = '.png')\n") 