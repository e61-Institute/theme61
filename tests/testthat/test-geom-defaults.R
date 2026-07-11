test_that("ribbon and inherited geom alpha defaults are set correctly", {
  expect_identical(ggplot2::GeomRibbon$default_aes$alpha, 0.1)
  expect_identical(ggplot2::GeomArea$default_aes$alpha, NA)
  expect_false(identical(ggplot2::GeomSmooth$default_aes$alpha, 0.1))
})
