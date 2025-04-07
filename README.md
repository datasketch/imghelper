# imghelper

An R package for advanced image manipulation, based on the `magick` package.

## Installation

```r
# Install from GitHub
devtools::install_github("datasketch/imghelper")
```

## Overview

`imghelper` provides a set of helper functions for working with images in R. It builds upon the powerful `magick` package to offer convenient functions for common image manipulation tasks:

- Converting images to/from data frames
- Creating various shapes and masks
- Clipping images with different shapes
- Rotating and scaling images
- Creating effects like "scanned document" appearance
- Manipulating image colors and backgrounds
- Reading and writing various image formats

## Basic Usage

```r
library(imghelper)

# Read an image
img <- img_read("path/to/image.jpg")

# Get image info
img_info(img)

# Rotate an image
rotated <- img_rotate(img, degrees = 45)

# Scale an image
scaled <- img_scale(img, width = 500)

# Create a blank image
blank <- img_blank(width = 300, height = 200, color = "blue")

# Write an image to file
img_write(img, "output.png")
```

## Dataframe Conversion

Convert images to data frames and back:

```r
# Convert an image to a data frame of pixels
df <- img_to_df(img)

# Manipulate pixels as needed
df_modified <- df |>
  dplyr::filter(x > 100, y < 200) |>
  dplyr::mutate(R = 1 - R)  # Invert red channel

# Convert back to an image
img_new <- df_to_img(df_modified)
```

## Image Masking and Clipping

Apply various mask shapes to images:

```r
# Create a circular mask
circle_mask <- img_mask(400, "circle")

# Create a rounded rectangle mask
rounded_mask <- img_mask(400, "rounded", radius = 0.2)

# Clip an image to a circle
img_circle <- img_clip(img, shape = "circle")

# Clip an image to a diamond
img_diamond <- img_clip(img, shape = "diamond", radius = 0.5)

# Clip with a custom mask
img_custom <- img_clip(img, mask = my_mask)
```

## Special Effects

Apply special effects to images:

```r
# Make an image look like a scanned document
scanned <- img_make_scanned(img, rotation_angle = 1, wrinkle_intensity = 20)

# Negate an image
negated <- img_negate(img)

# Add text annotation
annotated <- img_annotate(img, text = "Hello World", size = 20)
```

## Grid Layout

Arrange multiple images in a grid:

```r
# Create a list of images
img_list <- list(img1, img2, img3, img4)

# Arrange in a 2x2 grid
grid <- img_grid(img_list, ncol = 2, nrow = 2)
```

## License

MIT

## Author

Juan Pablo Marin Diaz
