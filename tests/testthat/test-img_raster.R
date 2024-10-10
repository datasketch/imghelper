test_that("multiplication works", {

  path <- sys_imghelper("ggm.jpeg")
  r <- img_raster(path)
  plot(r)

  path <- sys_imghelper("ggm.png")
  r <- img_raster(path)
  plot(r)

  path <- sys_imghelper("ggm.svg")
  r <- img_raster(path)
  plot(r)



})
