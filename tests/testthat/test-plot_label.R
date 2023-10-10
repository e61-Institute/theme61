test_that("plot_label() and mplot_label() look the same", {
  p1 <-
    ggplot2::ggplot() +
    plot_label(label = "Plot 1", x = 2, y = 2, n_labs = 2, n = 1) +
    plot_label(label = "Plot 2", x = 2, y = 3, n_labs = 2, n = 2)

  p2 <-
    ggplot2::ggplot() +
    mplot_label(label = c("Plot 1", "Plot 2"), x = c(2, 2), y = c(2, 3))

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-1.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-2.svg", p2)))
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
  expect_equal(retval$aes_params$colour, palette_e61(1))

  retval <- mplot_label("Test", "2020-01-01", 1)
  expect_equal(retval[[1]]$aes_params$colour, palette_e61(1))

  # Custom colours
  retval <- plot_label("Test", "2020-01-01", 1, colour = "#000000")
  expect_equal(retval$aes_params$colour, "#000000")

  retval <- mplot_label("Test", "2020-01-01", 1, colour = "#000000")
  expect_equal(retval[[1]]$aes_params$colour, "#000000")

})

test_that("Text and label plot labels work", {
  p1 <- ggplot() +
    plot_label("label", 2, 2, 1, 1, geom = "label")

  p2 <- ggplot() +
    plot_label("text", 2, 2, 1, 1, geom = "text")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-label.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-text.svg", p2)))
  })

})

test_that("Specifying incorrect length of label characteristics fails", {
  expect_error({
    ggplot() + mplot_label(rep("x", 3),
                           x = rep(0, 3),
                           y = rep(0, 3),
                           hjust = rep(0, 2))
  })


  expect_error({
    ggplot() + mplot_label(rep("x", 3),
                           x = rep(0, 3),
                           y = rep(0, 3),
                           angle = rep(0, 2))
  })

})

test_that("Changing horizontal alignment of text works", {

  p1 <- ggplot() +
    plot_label("Left-aligned text", 2, 2, 1, 1, hjust = 0) +
    plot_label("Centre-aligned text", 2, 2.1, 1, 1, hjust = 0.5) +
    plot_label("Right-aligned text", 2, 2.2, 1, 1, hjust = 1)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-horiz-align-change.svg", p1)))
  })
})

test_that("Label rotation works", {

  p1 <- ggplot() +
    plot_label("Normal text", 2, 2, 1, 1, angle = 90) +
    plot_label("Vertical text", 2, 2.3, 1, 1, angle = 0) +
    plot_label("Diagonal text", 2, 2.15, 1, 1, angle = 45) +
    ylim(2, 2.3)

  # Test it works with mplot_label too
  p2 <- ggplot() +
    mplot_label(c("Normal text", "Vertical text", "Diagonal text"),
                x = rep(2, 3),
                y = c(2, 2.3, 2.15),
                angle = c(90, 0, 45)) +
    ylim(2, 2.3)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-rotate.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-rotate-multi.svg", p2)))
  })

})
