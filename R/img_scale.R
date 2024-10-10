
#' @export
img_scale <- function(im, w = NULL, h = NULL, scale = NULL){
  if(is.null(w) && is.null(h) && is.null(scale))
    stop("Need w and h, or scale")
  ratio <- img_ratio(im)
  if(!is.null(w) && is.null(h)){
    w_out <- w
    h_out <- w * ratio
  }
  if(!is.null(h) && is.null(w)){
    h_out <- h
    w_out <- h / ratio
  }
  if(!is.null(w) && !is.null(h)){
    h_out <- h
    w_out <- w
  }
  if(!is.null(scale)){
    w_out <- img_width(im) * scale
    h_out <- img_height(im) * scale
  }
  image_scale(im, paste0(w_out,"x",h_out,"!"))
}


