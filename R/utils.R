



#' @export
sys_imghelper <- function(...){
  system.file(..., package = "imghelper")
}

#' @export
is_img <- function(x){
  inherits(x, "magick-image")
}

