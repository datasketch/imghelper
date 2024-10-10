
#' @export
img_background <- function (img, fuzz = 12){
  img %>% image_fill("none", fuzz = fuzz, point = "+1+1") %>%
    image_flop() %>% image_fill("none", fuzz = fuzz, point = "+1+1") %>%
    image_flip() %>% image_fill("none", fuzz = fuzz, point = "+1+1") %>%
    image_flop() %>% image_fill("none", fuzz = fuzz, point = "+1+1") %>%
    image_flip()
}
