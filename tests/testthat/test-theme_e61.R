test_that("square_legend_symbols is working", {
  # Points
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, colour = 1:3),
           aes(x, y, colour = colour)) +
    geom_point() +
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("square-legend-point.svg", p)))
  })

  # Lines
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, colour = 1:3),
           aes(x, y, colour = colour)) +
    geom_line() +
    geom_point(alpha = 0) + # The required "invisible points"
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("square-legend-line.svg", p)))
  })

  # Pointrange (testing ymin/ymax)
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, ymin = 0:2, ymax = 2:4, colour = 1:3),
           aes(x, y, ymin = ymin, ymax = ymax, colour = colour)) +
    geom_pointrange() +
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols(size = 1.2)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("square-legend-pointrange.svg", p)))
  })

  # Column
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, colour = 1:3),
           aes(x, y, fill = factor(colour))) +
    geom_col() +
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("square-legend-col.svg", p)))
  })

})
