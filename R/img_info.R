
#' @export
img_width <- function(im){
  image_info(im)[["width"]]
}

#' @export
img_height <- function(im){
  image_info(im)[["height"]]
}

#' @export
img_channels <- function(im){
  ncol(img_to_df(im)) - 2
}

#' @export
img_format <- function(x){
  if(is_img(x)){
    return(tolower(magick::image_info(img)$format))
  }
  tools::file_ext(x)
}

