# Tests
test_that("img_to_df handles color images", {
  # Create test color image
  img <- img_blank(100, color = "blue")
  df <- img_to_df(img)

  # Check structure
  expect_true(all(c("x", "y", "R", "G", "B") %in% names(df)))
  expect_equal(nrow(df), 10000)  # 100x100 image

  # Check values (should be blue)
  expect_true(all(df$B == 1))
  expect_true(all(df$R == 0))
  expect_true(all(df$G == 0))
})

test_that("img_to_df handles grayscale images", {
  # Create test grayscale image
  img <- img_blank(10, color = "gray")
  df <- img_to_df(img)

  # Check structure
  expect_true(all(c("x", "y", "I") %in% names(df)))
  expect_equal(nrow(df), 100)  # 10x10 image

  # All values should be equal (gray)
  expect_true(length(unique(df$I)) == 1)
})

test_that("img_to_df handles invalid inputs", {
  expect_error(img_to_df("not_an_image"))
})


test_that("", {

  img <- img_sample("windows")

  img_count_colors(img)

  df_original <- img_to_df(img)
  original_colors <- df_original |>
    dplyr::select(R, G, B) |>
    dplyr::distinct()
  nrow(original_colors)



})


