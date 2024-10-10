
#' @export
img_segments <- function(img, w = NULL, h = NULL){
  img <- img_read(img)

  w <- w %||% dsDesigner::img_size(img)[1]
  img <- dsDesigner::img_scale(img, w = w, h = h)
  #img <- magick::image_threshold(img)

  size <- img_size(img)
  w <- size[1]
  h <- size[2]

  d <- img_to_df(img)

  d <- d |>
    mutate(I = ifelse((R + G + B)/3 > 0.5, 0, 1)) |>
    select(-R,-G,-B)

  d1 <- d |>
    bind_rows(tibble::tibble(x = 0, y = 1:h, I = 0)) |>
    bind_rows(tibble::tibble(x = w + 1, y = 1:h, I = 0)) |>
    arrange(y, x)


  d2 <- d1 |>
    #filter(I == 0) |>
    mutate(segment = y) |>
    group_by(y) |>
    mutate(start = (I - dplyr::lag(I))) |>
    mutate(stop = ( I - dplyr::lead(I) )) |>
    arrange(y)

  d3 <- d2 |>
    mutate(start_stop = (start == 1 | stop == 1)) |>
    filter(start_stop) |>
    filter(start != stop) |>
    dplyr::ungroup()
  d3$segment <- rep(1:(nrow(d3)/2), each = 2)
  d3

  dd <- d3 |>
    select(y, x, segment) |>
    group_by(segment) |>
    summarize(y = y, x_from = min(x), x_to = max(x)) |>
    ungroup() |>
    distinct() |>
    mutate(seg_len = x_to - x_from) |>
    mutate(dist_len = seg_len/sum(seg_len)) |>
    mutate(cum_dist_seg = cumsum(dist_len))

  dd

}
