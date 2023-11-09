test_that("Single plot label works", {
  p1 <- minimal_plot_label +
    plot_label(label = "Plot 1", x = 2, y = 2)

  withr::with_tempdir({
    suppressWarnings(expect_snapshot_file(save_e61("plot-label-compare-plot.svg", p1)))
  })

})

test_that("String dates get converted to date dates properly", {

  retval <- plot_label("Label", x = "2020-01-01", y = 1)

  expect_equal(class(retval[[1]]$data$x), "Date")

})

test_that("Specifying custom colours works in plot_label()", {

  # Default colours
  retval <- plot_label("Test", "2020-01-01", 1)
  expect_equal(retval[[1]]$aes_params$colour, palette_e61(1))

  # Custom colours
  retval <- plot_label("Test", "2020-01-01", 1, colour = "#000000")
  expect_equal(retval[[1]]$aes_params$colour, "#000000")

})

test_that("Text and label plot labels work", {
  p1 <- minimal_plot_label +
    plot_label("label", 2, 2, geom = "label") +
    scale_y_continuous_e61()

  p2 <- minimal_plot_label +
    plot_label("text", 2, 2, geom = "text")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-label.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-text.svg", p2)))
  })

})

test_that("Specifying incorrect length of label characteristics fails", {
  expect_error({
    ggplot() + plot_label(rep("x", 3),
                           x = rep(0, 3),
                           y = rep(0, 3),
                           hjust = rep(0, 2))
  })


  expect_error({
    ggplot() + plot_label(rep("x", 3),
                           x = rep(0, 3),
                           y = rep(0, 3),
                           angle = rep(0, 2))
  })

})

test_that("Changing horizontal alignment of text works", {

  p1 <- minimal_plot_label +
    plot_label("Left-aligned text", 2, 2, hjust = 0) +
    plot_label("Centre-aligned text", 2, 2.1, hjust = 0.5) +
    plot_label("Right-aligned text", 2, 2.2, hjust = 1)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-horiz-align-change.svg", p1)))
  })
})

test_that("Label rotation works", {

  p1 <- minimal_plot_label +
    plot_label("Normal text", 2, 2, angle = 90) +
    plot_label("Vertical text", 2, 2.3, angle = 0) +
    plot_label("Diagonal text", 2, 2.15, angle = 45) +
    scale_y_continuous_e61(limits = c(2, 2.4))

  # Test it works with plot_label too
  p2 <- minimal_plot_label +
    plot_label(c("Normal text", "Vertical text", "Diagonal text"),
                x = rep(2, 3),
                y = c(2, 2.3, 2.15),
                angle = c(90, 0, 45)) +
    scale_y_continuous_e61(limits = c(2, 2.4))

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-rotate.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("plot-label-rotate-multi.svg", p2)))
  })

})
