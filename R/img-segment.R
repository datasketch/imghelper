
#' @export
img_segment <- function(img, min_pixels = NULL, segments = NULL,
                        bg_remove = FALSE, bg_fuzz = 12){
  if(is.null(min_pixels) && is.null(segments))
    stop("Need min_pixels or segments")
  if(!is.null(segments)){
    min_pixels <- min_pixels %||% img_pixels(img)/(segments + 1)
  }
  if(bg_remove){
    img <- img_remove_background(img, fuzz = bg_fuzz)
  }
  segs <- image_fuzzycmeans(img, min_pixels = min_pixels) %>% image_split()
  class(segs) <- c(class(segs), "img_segments")
  segs
}

#' @export
img_segment_info <- function(segs, bg = 1, segments = NULL, min_pixels = NULL,
                             bg_remove = FALSE, bg_fuzz = 15){
  if(!"img_segments" %in% class(segs)){
    segs <- img_segment(segs, segments = segments, min_pixels = min_pixels,
                        bg_remove = bg_remove, bg_fuzz = bg_fuzz)
  }
  l <- lapply(segs, function(x){
    cat <- img_bw(x)
    cat <- dsDesigner::img_df_mask(cat, negate = TRUE)
    n_pixels <- sum(cat$mask) # 8035
    df <- img_to_df(x)
    df <- df %>% dplyr::filter(alpha == 1)
    df$color <- farver::encode_colour(df[, c("R", "G", "B")] * 255)
    list(
      color = unique(df$color)[1],
      pixels = nrow(df)
      )
  })
  d <- bind_rows(l)
  d$pixels_dist <- NA
  d$pixels_dist[-bg] <- d$pixels[-bg]/sum(d$pixels[-bg])
  d
}


#' @export
img_pixels <- function(img){
  image_info(img)$width * image_info(img)$height
}



