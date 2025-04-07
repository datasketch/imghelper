#' Create sample images for testing and demonstration
#' @param type Character. Type of sample image to create: "windows", "checkerboard", "circles", 
#'   "gradient", "stripes", "color_bars", "grayscale_steps", "bw_pattern"
#' @param width Integer. Width of the image in pixels (default: 200)
#' @param height Integer. Height of the image in pixels (default: same as width)
#' @param bg_color Character. Background color (default: "white")
#' @param colors Character vector. Colors to use for the sample (will be recycled if needed)
#' @param n_shapes Integer. Number of shapes to draw for applicable sample types (default: 4)
#' @return A magick-image object
#' @export
img_sample <- function(type = c("windows", "checkerboard", "circles", "gradient", 
                               "stripes", "color_bars", "grayscale_steps", "bw_pattern"), 
                      width = 200, height = NULL, bg_color = "white", 
                      colors = NULL, n_shapes = 4) {
  
  type <- match.arg(type)
  height <- height %||% width
  
  # Create base blank image
  img <- img_blank(width, height, color = bg_color)
  
  # Define default colors for different types if not specified
  if (is.null(colors)) {
    colors <- switch(type,
                   "windows" = c("red", "blue", "green", "yellow"),
                   "checkerboard" = c("black", "white"),
                   "circles" = rainbow(n_shapes),
                   "gradient" = NULL, # Not used for gradient
                   "stripes" = c("red", "orange", "yellow", "green", "blue", "purple"),
                   "color_bars" = c("red", "green", "blue", "cyan", "magenta", "yellow", "black"),
                   "grayscale_steps" = NULL, # Generated within function
                   "bw_pattern" = c("black", "white"),
                   rainbow(n_shapes)) # Default to rainbow if no match
  }
  
  # Draw the sample based on type
  img <- magick::image_draw(img)
  
  switch(type,
         "windows" = {
           # Create a Windows logo-like image (four colored quadrants)
           half_w <- width / 2
           half_h <- height / 2
           graphics::rect(0, 0, half_w, half_h, col = colors[1], border = NA)
           graphics::rect(half_w, 0, width, half_h, col = colors[2], border = NA)
           graphics::rect(0, half_h, half_w, height, col = colors[3], border = NA)
           graphics::rect(half_w, half_h, width, height, col = colors[4], border = NA)
         },
         "checkerboard" = {
           # Create a checkerboard pattern
           n_cells <- 8  # 8x8 checkerboard by default
           cell_w <- width / n_cells
           cell_h <- height / n_cells
           
           for (i in 0:(n_cells-1)) {
             for (j in 0:(n_cells-1)) {
               if ((i + j) %% 2 == 0) {
                 col_idx <- 1
               } else {
                 col_idx <- 2
               }
               graphics::rect(i * cell_w, j * cell_h, 
                             (i + 1) * cell_w, (j + 1) * cell_h, 
                             col = colors[col_idx], border = NA)
             }
           }
         },
         "circles" = {
           # Create concentric circles or pattern of circles
           if (n_shapes <= 1) {
             # One large circle
             graphics::symbols(width/2, height/2, 
                              circles = min(width, height)/2 * 0.9, 
                              inches = FALSE, add = TRUE, 
                              fg = colors[1], bg = colors[1])
           } else if (n_shapes == 4) {
             # Four circles in grid layout
             centers <- list(
               c(width/4, height/4),
               c(3*width/4, height/4),
               c(width/4, 3*height/4),
               c(3*width/4, 3*height/4)
             )
             radius <- min(width, height) / 5
             
             for (i in 1:4) {
               graphics::symbols(centers[[i]][1], centers[[i]][2], 
                                circles = radius, 
                                inches = FALSE, add = TRUE, 
                                fg = colors[i], bg = colors[i])
             }
           } else {
             # Concentric circles
             max_radius <- min(width, height) / 2 * 0.9
             for (i in seq_len(n_shapes)) {
               radius <- max_radius * (n_shapes - i + 1) / n_shapes
               color_idx <- (i - 1) %% length(colors) + 1
               graphics::symbols(width/2, height/2, 
                                circles = radius, 
                                inches = FALSE, add = TRUE, 
                                fg = colors[color_idx], bg = colors[color_idx])
             }
           }
         },
         "gradient" = {
           # Create a gradient from top to bottom
           # This uses many thin rectangles to simulate a gradient
           n_steps <- 100
           step_height <- height / n_steps
           
           for (i in 0:(n_steps-1)) {
             # Calculate color intensity based on position
             intensity <- i / (n_steps - 1)
             color <- rgb(intensity, 0, 1 - intensity)
             
             graphics::rect(0, i * step_height, 
                           width, (i + 1) * step_height, 
                           col = color, border = NA)
           }
         },
         "stripes" = {
           # Create horizontal stripes
           n_stripes <- length(colors)
           stripe_height <- height / n_stripes
           
           for (i in 0:(n_stripes-1)) {
             color_idx <- i + 1
             graphics::rect(0, i * stripe_height, 
                           width, (i + 1) * stripe_height, 
                           col = colors[color_idx], border = NA)
           }
         },
         "color_bars" = {
           # Create vertical color bars (like a color test pattern)
           n_bars <- length(colors)
           bar_width <- width / n_bars
           
           for (i in 0:(n_bars-1)) {
             color_idx <- i + 1
             graphics::rect(i * bar_width, 0, 
                           (i + 1) * bar_width, height, 
                           col = colors[color_idx], border = NA)
           }
         },
         "grayscale_steps" = {
           # Create grayscale steps
           n_steps <- 10
           step_width <- width / n_steps
           
           for (i in 0:(n_steps-1)) {
             # Calculate gray level
             gray_level <- i / (n_steps - 1)
             color <- rgb(gray_level, gray_level, gray_level)
             
             graphics::rect(i * step_width, 0, 
                           (i + 1) * step_width, height, 
                           col = color, border = NA)
           }
         },
         "bw_pattern" = {
           # Create a black and white pattern (grid, diagonal lines or dots)
           
           # Let's do a more interesting B&W pattern - diagonal lines
           line_spacing <- min(width, height) / 20
           
           # Draw diagonal lines in both directions
           for (i in seq(-height, width + height, by = line_spacing)) {
             graphics::lines(c(i, i + height), c(0, height), 
                            col = colors[1], lwd = 2)
           }
           
           # Add small circles at intersections
           for (x in seq(0, width, by = line_spacing * 2)) {
             for (y in seq(0, height, by = line_spacing * 2)) {
               graphics::symbols(x, y, circles = line_spacing / 2, 
                                inches = FALSE, add = TRUE, 
                                fg = colors[2], bg = colors[2])
             }
           }
         }
  )
  
  dev.off()
  return(img)
} 