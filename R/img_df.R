

#' @export
img_to_df <- function(img){

  imgarr <- as.integer(img[[1]])
  imgDm <- dim(imgarr)


  # TODO handle pngs
  # if(imgDm[3] == 4){
  #
  # }
  img_df <- data.frame(
    x = rep(1:imgDm[2], each = imgDm[1]),
    y = rev(rep(imgDm[1]:1, imgDm[2]))
  )

  # TODO handle pngs
  if(imgDm[3] == 1){
    dims <- data.frame(I = as.vector(imgarr[,,1])/255)
  } else if(imgDm[3] == 3){
    dims <- data.frame(
      R = (as.vector(imgarr[,,1]))/255,
      G = (as.vector(imgarr[,,2]))/255,
      B = (as.vector(imgarr[,,3]))/255
    )
  } else if(imgDm[3] == 4){
    dims <- data.frame(
      R = (as.vector(imgarr[,,1]))/255,
      G = (as.vector(imgarr[,,2]))/255,
      B = (as.vector(imgarr[,,3]))/255,
      alpha = (as.vector(imgarr[,,4]))/255
    )
  }
  cbind(img_df, dims)
}

#' @export
df_to_img <- function(d){
  width <- max(d$x)
  height <- max(d$y)
  if(ncol(d) == 3){
    dfimgmtx <- as.matrix(d[3])
    a <- array(dfimgmtx, c(height,width,1))
    img <- image_read(a)
  } else if(all(c("R","G","B") %in% names(d))){
    #dfimg <- imgarr_to_df(img)
    dfimgmtx <- as.matrix(d[c("R","G","B")])
    a <- array(dfimgmtx, c(height,width,3))
    img <- image_read(a)
  }else{
    dfimgmtx <- as.matrix(d[c("R","G","B","alpha")])
    a <- array(dfimgmtx, c(height,width,4))
    img <- image_read(a)
  }
  img
}

