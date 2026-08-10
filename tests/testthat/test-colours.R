test_that("palette_e61 returns palettes of the expected size", {
  expect_equal(palette_e61(1), e61_tealdark1)
  expect_length(palette_e61(4), 4)
  expect_length(palette_e61(12), 12)
})

test_that("palette_e61 reverse option returns reversed palette", {
  pal <- palette_e61(5)
  expect_equal(palette_e61(5, reverse = TRUE), rev(pal))
})

test_that("palette_e61 validates palette size", {
  expect_error(palette_e61(0))
  expect_error(palette_e61(13))
})
