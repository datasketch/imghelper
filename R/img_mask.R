#' Create an image mask
#' @param size Integer. Size of the square mask
#' @param shape Character. Shape of the mask ("circle", "rounded", "frame", "diamond", "oval")
#' @param radius Numeric. Shape modifier (0-1):
#'   - circle: ignored
#'   - rounded: corner radius as proportion
#'   - frame: frame thickness as proportion
#'   - diamond: diamond size as proportion (1=smallest)
#'   - oval: height/width ratio (1 = circle)
#' @return A magick-image object
#' @export
img_mask <- function(size, shape = c("circle", "rounded", "frame", "diamond", "oval"),
                     radius = NULL) {

  shape <- match.arg(shape)

  if (!is.numeric(size) || size < 1) {
    stop("size must be a positive number")
  }

  if(shape == "rounded"){
    radius <- radius %||% 0.1
  }
  radius <- radius %||% 1

  if (!is.numeric(radius) || radius < 0 || radius > 1) {
    stop("radius must be a number between 0 and 1")
  }

  # Create base mask
  mask <- magick::image_blank(width = size, height = size, color = "black")
  mask <- magick::image_draw(mask)

  switch(shape,
         "circle" = {
           graphics::symbols(size/2 - 2, size/2 -2,
                             circles = size/2 - 2,
                             inches = FALSE,
                             fg = "white",
                             bg = "white",
                             add = TRUE)
         },
         "rounded" = {
           corner_radius <- size/2 * radius

           # Fill the entire area in white first
           graphics::rect(0, 0, size, size, col = "white", fill = "white")

           # Draw black triangles in corners
           graphics::polygon(
             x = c(0, corner_radius, 0),
             y = c(0, 0, corner_radius),
             col = "black", fill = "black"
           )
           graphics::polygon(
             x = c(size, size - corner_radius, size),
             y = c(0, 0, corner_radius),
             col = "black", fill = "black"
           )
           graphics::polygon(
             x = c(0, corner_radius, 0),
             y = c(size, size, size - corner_radius),
             col = "black", fill = "black"
           )
           graphics::polygon(
             x = c(size, size - corner_radius, size),
             y = c(size, size, size - corner_radius),
             col = "black", fill = "black"
           )

           # Draw corner circles
           graphics::symbols(
             x = c(corner_radius, size - corner_radius,
                   corner_radius, size - corner_radius),
             y = c(corner_radius, corner_radius,
                   size - corner_radius, size - corner_radius),
             circles = rep(corner_radius, 4),
             inches = FALSE,
             fg = "white",
             bg = "white",
             add = TRUE
           )
         },
         "frame" = {
           frame_width <- size * (1 - radius) / 2  # Normalize frame width
           graphics::rect(0, 0, size, size,
                          col = "white", fill = "white")
           graphics::rect(frame_width, frame_width,
                          size - frame_width, size - frame_width,
                          col = "black", fill = "black")
         },
         "diamond" = {
           # Invert radius for diamond (1 = smallest, 0 = largest)
           diamond_size <- size * (radius)
           center <- size/2
           half_size <- diamond_size/2
           graphics::polygon(
             x = c(center, center + half_size, center, center - half_size),
             y = c(center - half_size, center, center + half_size, center),
             col = "white",
             border = "white"  # Add border parameter
           )
         },
         "oval" = {
           # Use radius as height/width ratio modifier
           center <- size/2
           rx <- size/2
           ry <- size/2 * radius

           # Approximate ellipse with polygon
           theta <- seq(0, 2*pi, length.out = 100)
           x <- center + rx * cos(theta)
           y <- center + ry * sin(theta)

           graphics::polygon(x, y, col = "white", fill = "white")
         }
  )

  dev.off()
  mask
}

