

#' @export
img_grid <- function(d, coding = NULL, type = "point",
                     size = 20,
                     defaultColor = NULL,
                     shape = 16,
                     image_w = 300,
                     image_h = 300,
                     padding = NULL) {
  nlevels <- nlevels(d[[3]])
  ncol <- max(d$x)
  nrow <- max(d$y)
  if(length(size) == 1){
    size <- c(size, size)
  }
  if(is.null(coding)){
    coding <- data.frame(id = na.omit(unique(d[[3]])),
                         color = defaultColor %||% ds_colors(nlevels),
                         fill = defaultColor %||% ds_colors(nlevels),
                         stringsAsFactors = FALSE)
  }else{
    if(suppressWarnings(is.null(coding$color))){
      coding$color <- defaultColor %||% ds_colors(nlevels)
    }
    if(is.null(coding$fill)){
      coding$fill <- defaultColor %||% ds_colors(nlevels)
    }
  }
  if(!suppressWarnings(is.null(coding$size))){
    d$size <- coding$size[match(d[[3]], coding$id)]
  }
  if(suppressWarnings(is.null(d$size))){
    d$size <- size[1]
  }
  if(type == "image"){
    # dirty fix for image aspect ratio
    coding$image <- unlist(lapply(coding$image,function(impath){
      im <- image_read(impath) %>% img_scale(image_w,image_h)
      dir <- tempdir()
      path <- file.path(dir,basename(impath))
      image_write(im,path)
      path
    }))
    if(is.null(d$image)){
      if(!is.null(coding$image)){
        d$image <- coding$image[match(d[[3]], coding$id)]
      }else{
        stop("Need an image column")
      }
      # if(is.null(defaultColor)){
      #   coding$color <- NA
      # }
    }
  }
  d$color <- coding$color[match(d[[3]], coding$id)]
  d$fill <- coding$fill[match(d[[3]], coding$id)]
  #str(coding)
  #str(d)
  g <- ggplot(d, aes(x = x,y = y, size = size, color = color, fill = fill))
  if(type == "point"){
    g <- g + geom_point(shape = shape)
    #g <- g + coord_equal(ylim = 0.5 + c(0,nrow),  xlim = 0.5 + c(0,ncol))
    g <- g + coord_fixed(1,ylim = 0.5 + c(0,nrow),  xlim = 0.5 + c(0,ncol))
  }
  if( type == "image"){
    g <- g + coord_fixed(1,ylim = 0.5 + c(0,nrow),  xlim = 0.5 + c(0,ncol))
    if(!is.null(defaultColor)){
      g <- g + geom_image(aes(image=image, size=I(size)/10), color = defaultColor)
    }else{
      g <- g + geom_image(aes(image=image, size=I(size)/10))
    }

  }
  g <- g + scale_y_reverse(breaks = 1:nrow)
  g <- g + scale_x_continuous(breaks = 1:ncol)
  g <- g + scale_color_manual(values = coding$color,guide = FALSE)
  if(type == "point"){
    g <- g + scale_fill_manual(values = coding$fill,guide = FALSE)
    g <- g + scale_size_continuous(range = size, guide = FALSE)
  }
  g <- g + theme_void()
  g + theme_transparent()
}

