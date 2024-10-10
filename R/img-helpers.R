#' @export
img_df_mask <- function(img, threshold = 0.5, negate = FALSE){
  if(image_info(img)$colorspace == "sRGB"){
    img <- img_grayscale(img)
  }
  if(negate) img <- image_negate(img)
  imgdf <- img_to_df(img)
  if(ncol(imgdf) == 3){
    imgdf$mask <- imgdf$I
    imgdf$mask <- ifelse(imgdf$mask < threshold, 1, 0)
  }
  imgdf[c("x","y","mask")]
}

#' @export
mask_proportion <- function(im){
  mask <- img_df_mask(im)
  w <- max(mask$x)
  h <- max(mask$y)
  sum(mask$mask)/(w*h)
}

#' @export
mask_points <- function(im){
  mask <- img_df_mask(im)
  sum(mask$mask)
}


#' @export
img_list <- function(x){
  list.files(x, pattern = "\\.png|\\.jpg", full.names = TRUE)
}

