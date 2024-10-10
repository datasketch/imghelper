


# img_svg_to_png <- function(input_svg_path, width = 800) {
#   img <- magick::image_read_svg(input_svg_path, width = width)
#   f <- tempfile(fileext = ".png")
#   magick::image_write(img, path = f, format = "png")
#   f
# }
#
# img_jpeg_negate <- function(path){
#   img <- magick::image_read(path)
#   f <- tempfile(fileext = ".jpeg")
#   img <- magick::image_negate(img)
#   magick::image_write(img, path = f, format = "jpeg")
#   f
# }
