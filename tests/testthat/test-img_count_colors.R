test_that("multiplication works", {

  img <- img_sample("windows")

  expect_equal(img_count_colors(img), 4)


})
