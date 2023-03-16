test_that("plot_label() and mplot_label() look the same", {
  p1 <-
    ggplot2::ggplot() +
    plot_label("Plot 1", 2, 2, 2, 1) +
    plot_label("Plot 2", 2, 3, 2, 2)

  p2 <-
    ggplot2::ggplot() +
    mplot_label(c("Plot 1", "Plot 2"), c(2, 2), c(2, 3))

  withr::with_tempdir({
    expect_snapshot_file(save_e61("plot-label-1.svg", p1, height = 10))
    expect_snapshot_file(save_e61("plot-label-2.svg", p2, height = 10))
  })

})

test_that("String dates get converted to date dates properly", {

  retval <- plot_label("Test", "2020-01-01", 1, 1, 1)
  retval_m <- mplot_label("Test", "2020-01-01", 1, 1, 1)

  expect_equal(class(retval$data$x), "Date")
  expect_equal(class(retval_m[[1]]$data$x), "Date")

})

test_that("Specifying custom colours works in plot_label() and mplot_label()", {

  # Default colours
  retval <- plot_label("Test", "2020-01-01", 1, 1, 1)
  expect_equal(retval$aes_params$colour, e61_palette(1))

  retval <- mplot_label("Test", "2020-01-01", 1)
  expect_equal(retval[[1]]$aes_params$colour, e61_palette(1))

  # Custom colours
  retval <- plot_label("Test", "2020-01-01", 1, colour = "#000000")
  expect_equal(retval$aes_params$colour, "#000000")

  retval <- mplot_label("Test", "2020-01-01", 1, colour = "#000000")
  expect_equal(retval[[1]]$aes_params$colour, "#000000")

})
