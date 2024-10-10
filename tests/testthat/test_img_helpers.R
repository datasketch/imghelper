context("datagrid")

test_that("create_matrix",{

  suppressWarnings(library(tidyverse))
  v <- sample(0:3,5, replace = TRUE)
  m <- create_matrix(v, ncol = 2)
  expect_equal(sum(is.na(m)), 1)
  expect_equal(dim(m), c(3,2))
  m <- create_matrix(v, nrow = 5)
  expect_equal(nrow(m),5)
  m <- create_matrix(v, ncol = 3, nrow = 4)
  expect_equal(dim(m), c(4,3))
  m1 <- create_matrix(v, ncol = 3, nrow = 2, byrow = FALSE)
  m2 <- create_matrix(v, ncol = 3, nrow = 2, byrow = TRUE)
  expect_equal(m1[1:2,1], m2[1,1:2])
  m <- create_matrix(v, nrow = 2, ncol = 10)
  expect_true(all(is.na(m[1:2,4:10])))

  # Resize images
  im <- image_read(system.file("imgs/racimo.jpg", package = "dsDesigner"))
  ratio <- img_ratio(im)
  im2 <- img_scale(im, w = 100)
  expect_equal(img_height(im2), round(100*ratio))
  im3 <- img_scale(im, h = 100)
  expect_equal(img_width(im3), round(100/ratio))
  im4 <- img_scale(im, w = 100, h = 100)
  expect_equal(img_size(im4), c(100,100))

})

test_that("image to df",{


})


test_that("masks work",{

  im <- image_read(system.file("imgs/favicon.jpg", package = "dsDesigner"))
  df <- img_to_df(im)

  library(tidyverse)
  whites <- df %>% filter(x < 9, y < 9) %>% pull(I)
  expect_true(all(whites == 1))
  blacks <- df %>% filter(x < 9, y > 9) %>% pull(I)
  expect_true(all(blacks == 0))

  thresh <- 0.3

  dmask <- img_df_mask(im, threshold = thresh)
  expect_true(all(unique(dmask$mask) %in% c(0,1)))

  immask <- df_to_img(dmask)
  blacksdmask <- df %>% filter(x < 9, y < 9) %>% pull(I)
  whitesdmask <- df %>% filter(x < 9, y > 9) %>% pull(I)
  expect_equal(blacks, whitesdmask)
  expect_equal(whites, blacksdmask)


  imgPath <- system.file("imgs",package = "dsDesigner")

  # With bw images
  img <- image_read(file.path(imgPath,"house-bw.jpg")) %>% img_scale(w = 200)
  df_mask <- img_df_mask(img, threshold = 0.4)
  df_to_img(df_mask)

  # With bw images
  im <- image_read(system.file("imgs/racimo.jpg", package = "dsDesigner")) %>%
    img_scale(w = 220, h = 450)
  df_mask <- img_df_mask(im)
  df_to_img(df_mask)

  # With gray images
  img <- image_read(file.path(imgPath,"racimo-gray.jpg"))
  img_channels(img)
  ratio <- img_ratio(img)
  w <- 200
  img <- img_scale(img, w, w*ratio)
  img
  df_mask <- img_df_mask(img, threshold = 0.6)
  df_to_img(df_mask)

  # With color images
  img <- image_read(file.path(imgPath,"manchas.png"))
  ratio <- img_ratio(img)
  w <- 200
  img <- img_scale(img, w, w*ratio)
  img
  df_mask <- img_df_mask(img, threshold = 0.5)
  df_to_img(df_mask)


})



test_that("image_helpers",{


})








