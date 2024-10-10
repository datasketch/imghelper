

#' @export
img_make_scanned <- function(image_path, output_path = NULL, rotation_angle = 1, wrinkle_intensity = 20) {
  img <- img_read(image_path)

  # Create a gradient mask for the bottom left corner
  mask <- magick::image_blank(width = magick::image_info(img)$width,
                              height = magick::image_info(img)$height,
                              color = "white") |>
    magick::image_fx(expression = "1-(i/(w*0.7))*(j/(h*0.7))")

  # Create noise layer
  noise <- magick::image_blank(width = magick::image_info(img)$width,
                               height = magick::image_info(img)$height,
                               color = "gray50") |>
    magick::image_noise() |>
    magick::image_blur(radius = 1, sigma = 1)

  # Composite noise with mask
  masked_noise <- magick::image_composite(noise, mask, operator = "multiply")

  # Apply noise to image
  img <- magick::image_composite(img, masked_noise, operator = "blend", compose_args = paste0(wrinkle_intensity, "%")) |>
    magick::image_convert(colorspace = "gray") |>
    magick::image_noise("gaussian") |>
    magick::image_blur(radius = 0.5, sigma = 0.5) |>
    magick::image_contrast(sharpen = 1.2) |>
    magick::image_modulate(brightness = 105) |>
    magick::image_rotate(rotation_angle)

  if (!is.null(output_path)) {
    magick::image_write(img, path = output_path)
  }

  img
}
