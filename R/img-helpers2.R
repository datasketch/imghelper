
#' @export
merge_images <- function(l){
  Reduce(c, l)
}

#' @export
img_join <- function(l, ncol = ncol){
  idxs <- matrix(1:length(l), ncol = ncol, byrow = TRUE)

  rows <- nrow(idxs)
  lrows <- lapply(1:rows, function(row){
    # join rows
    img_join_by_row(l[idxs[row,1]:idxs[row,ncol]])
  })
  img_join_by_col(lrows)
}

#' @export
img_join_by_row <- function(l){
  imgs <- merge_images(l)
  image_append(imgs)
}

#' @export
img_join_by_col <- function(l){
  imgs <- merge_images(l)
  image_append(imgs, stack = TRUE)
}


#' @export
create_bg_img <- function(bg_color = NULL, width = 300, height = 300){
  #bg_color <- "#FFFF00"
  X <- array(as.vector(col2rgb(bg_color)), c(1, 1, 3))
  bg <- image_read(X/255) %>% image_scale(paste(width, height, sep = "x"))
  bg
}

#' @export
img_kmeans <- function(img, clusters = 5){
  width <- image_info(img)$width
  height <- image_info(img)$height
  imgRGB <- imgarr_to_df(img)
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = clusters)
  kColours <- kMeans$centers[kMeans$cluster,]
  #as.raw(kColours)
  kColours_hex <- apply(kColours,1,function(x){
    toupper(paste0("#",as.raw(x[1]),as.raw(x[2]),as.raw(x[3])))
  })
  # #050309 #5C6279 #992B13 #E29C89 #F8F7F9
  # 127410   41248   32126   94057  195159
  # a <- array(kColours, c(width,height,3))/255
  a <- array(kColours, c(height,width,3))/255
  colors <- kMeans$centers
  color_dist <- table(unname(kColours_hex))
  color_names <- names(color_dist)
  dist <- as.vector(unname(color_dist)/sum(unname(color_dist)))
  colors_dist <- tibble(color = color_names,
                        dist = dist)

  list(
    img = image_read(a),
    colors = colors,
    colors_hex = toupper(paste0("#",as.raw(colors[,1]),as.raw(colors[,2]),as.raw(colors[,3]))),
    colors_dist = colors_dist
  )
}

#' @export
contrast_color <- function(bg_color, brightness_thres = 200){
  #bg_color <- "#FFFF00"
  rgb <- as.vector(col2rgb(bg_color))
  brightness_level <- (rgb[1] * 299 + rgb[2] * 587 + rgb[3] * 114) / 1000
  bright <-  brightness_level > brightness_thres
  ifelse(bright, "#201050", "#f7fcfd")
}


# imgarr_to_df <- function(img){
#   imgarr <- as.integer(img[[1]])
#   imgDm <- dim(imgarr)
#   imgRGB <- data.frame(
#     x = rep(1:imgDm[2], each = imgDm[1]),
#     y = rep(imgDm[1]:1, imgDm[2]),
#     R = as.vector(imgarr[,,1]),
#     G = as.vector(imgarr[,,2]),
#     B = as.vector(imgarr[,,3])
#   )
#   imgRGB
# }

#' @export
wrap_sentence <- function(string, width) {
  if(is.na(string) || nchar(string) == 0) return("")
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}


sample_gravity <- function(){
  sample(c("center","center","center","northwest","north","east","northeast","west","northwest","south",
           "southeast","southwest"),1)
}

