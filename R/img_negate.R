
img_negate <- function(img){
  magick::image_negate(img)
}


img_file_negate <- function(path, output_path){
  ext <- tools::file_ext(path)
  img <- magick::image_read(path)
  f <- tempfile(fileext = ext)
  img <- magick::image_negate(img)
  magick::image_write(img, path = f, format = "jpeg")
  f
}
