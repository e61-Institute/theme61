test_that("add_e61_logo() produces a usable annotation when magick is available", {
  skip_if_not_installed("magick")

  t61_env$logo <- NULL
  withr::defer(t61_env$logo <- NULL)

  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + add_e61_logo()

  # t61_with_device(), not ggplotGrob() directly: with no device open the
  # latter opens a pdf device and leaves it current, which silently changes
  # text metrics for every later render in the suite.
  devices_before <- grDevices::dev.list()
  expect_no_error(t61_with_device(ggplot2::ggplotGrob(p)))
  expect_identical(grDevices::dev.list(), devices_before)
})

test_that("t61_get_logo() caches the logo for the session", {
  skip_if_not_installed("magick")

  t61_env$logo <- NULL
  withr::defer(t61_env$logo <- NULL)

  first <- t61_get_logo()
  expect_false(is.null(t61_env$logo))
  expect_identical(t61_get_logo(), first)
})
