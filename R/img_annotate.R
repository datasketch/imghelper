
#' @export
img_annotate <- function(img, text, color = "black", size = 10,
                         font = "Times New Roman",
                         weight = "400",
                         gravity = "NorthWest",
                         y = 0, x = 0){
  y <- y %||% 0
  x <- x %||% 0
  loc_y <- img_size(img)[2] * y/100
  loc_x <- img_size(img)[1] * x/100
  location <- paste0("+",loc_x,"+",loc_y)
  str(location)
  image_annotate(img, text = text,
                 color = color, size = size,
                 degrees = 0,
                 font = font,
                 weight = weight,
                 gravity = gravity,
                 location = location)
}
