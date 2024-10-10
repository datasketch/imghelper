
#' @export
img_read <- function(path, ...){

  if(is_img(path)) return(path)

  args <- list(...)
  ext <- tools::file_ext(path)
  if(ext == "svg"){
    # img <- magick::image_read_svg(path, width = width)
    if(is.null(args$width)){
      width <- 800
    }
    img <- magick::image_read_svg(path, width = width, ...)
  }else if(ext == "pdf"){
    img <- magick::image_read_pdf(path, ...)
  }else{
    img <- magick::image_read(path)
  }
  img

}
