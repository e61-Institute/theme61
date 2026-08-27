test_that("Test the function works in isolation", {
  withr::local_options(list(theme61.sec_axis_msg = FALSE))

  expect_equal(sec_rescale_inv(c(10, 20, 30), scale = 0.1),
               c(100, 200, 300))

  expect_equal(sec_rescale_inv(c(10, 20, 30), shift = 10),
               c(20, 30, 40))

  expect_equal(sec_rescale(c(100, 200, 300), scale = 0.1, shift = 0),
               c(10, 20, 30))

  expect_equal(sec_rescale(c(20, 30, 40), scale = 1, shift = 10),
               c(10, 20, 30))
})


test_that("Graphs produced with manipulated secondary axes work", {
  p_scale <- ggplot(data.frame(x = 1, y1 = 10, y2 = 200), aes(x)) +
    geom_point(aes(y = y1), colour = "red") +
    geom_point(aes(y = sec_rescale_inv(y2, scale = 10))) +
    scale_y_continuous_e61(
      limits = c(0, 25, 5),
      sec_axis = sec_axis(~sec_rescale(.), name = "%"),
      rescale_sec = TRUE
    ) +
    labs_e61(y = "%")

  p_shift <- ggplot(data.frame(x = 1, y1 = 10, y2 = 30), aes(x)) +
    geom_point(aes(y = y1), colour = "red") +
    geom_point(aes(y = sec_rescale_inv(y2, shift = -10))) +
    scale_y_continuous_e61(
      limits = c(0, 25, 5),
      sec_axis = sec_axis(~sec_rescale(.), name = "%"),
      rescale_sec = TRUE
    ) +
    labs_e61(y = "%")

  expect_no_error(ggplot_build(p_scale))
  expect_no_error(ggplot_build(p_shift))
})

test_that("sec_rescale()/sec_rescale_inv() edge cases", {
  withr::local_options(list(theme61.sec_axis_msg = FALSE))

  # identity: scale = 1, shift = 0 leaves values unchanged
  expect_equal(sec_rescale_inv(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(sec_rescale(c(1, 2, 3), scale = 1, shift = 0), c(1, 2, 3))

  # sec_rescale_inv() and sec_rescale() are inverses of each other
  vals <- c(5, 10, 15)
  inv <- sec_rescale_inv(vals, scale = 0.5, shift = 2)
  expect_equal(sec_rescale(inv, scale = 0.5, shift = 2), vals)

  # scalar scale/shift recycle across a longer values vector
  expect_equal(sec_rescale_inv(1:4, scale = 2, shift = 1),
               (1:4 + 1) / 2)
  expect_equal(sec_rescale(1:4, scale = 2, shift = 1),
               1:4 * 2 - 1)

  # NA propagates through both functions without erroring
  expect_equal(sec_rescale_inv(c(1, NA, 3), scale = 1, shift = 0), c(1, NA, 3))
  expect_equal(sec_rescale(c(1, NA, 3), scale = 1, shift = 0), c(1, NA, 3))
})

test_that("sec_rescale() falls back to values stashed by sec_rescale_inv() via t61_env", {
  withr::local_options(list(theme61.sec_axis_msg = FALSE))

  # Calling sec_rescale_inv() stashes scale/shift in the package environment,
  # which sec_rescale()'s default arguments then pick up.
  sec_rescale_inv(c(10, 20), scale = 5, shift = 1)

  expect_equal(sec_rescale(c(2, 4)), c(2, 4) * 5 - 1)
})

test_that("a plot with the default secondary axis renders an axis-r grob", {
  # Regression test for NEWS 0.7.1: "Fix issue with secondary y-axis not
  # appearing by default on certain graphs."
  p <- ggplot(data.frame(x = 1:3, y = c(1, 2, 3)), aes(x, y)) +
    geom_point() +
    scale_y_continuous_e61(limits = c(0, 5, 1))

  grobs <- quiet_ggplotGrob(p)

  expect_gt(get_grob_width(grobs, grob_name = "axis-r"), 0)
})

test_that("sec_axis = FALSE suppresses the axis-r grob", {
  p <- ggplot(data.frame(x = 1:3, y = c(1, 2, 3)), aes(x, y)) +
    geom_point() +
    scale_y_continuous_e61(limits = c(0, 5, 1), sec_axis = FALSE)

  grobs <- quiet_ggplotGrob(p)

  width <- get_grob_width(grobs, grob_name = "axis-r")
  expect_true(is.null(width) || width == 0)
})

test_that("a rescaled secondary axis on a real plot also renders an axis-r grob", {
  withr::local_options(list(theme61.sec_axis_msg = FALSE))

  p <- ggplot(data.frame(x = 1, y1 = 10, y2 = 200), aes(x)) +
    geom_point(aes(y = y1)) +
    geom_point(aes(y = sec_rescale_inv(y2, scale = 10))) +
    scale_y_continuous_e61(
      limits = c(0, 25, 5),
      sec_axis = sec_axis(~sec_rescale(.), name = "%"),
      rescale_sec = TRUE
    ) +
    labs_e61(y = "%")

  grobs <- quiet_ggplotGrob(p)

  expect_gt(get_grob_width(grobs, grob_name = "axis-r"), 0)
})

test_that("faceted plots with a secondary axis build and render without error", {
  # Regression test for NEWS 0.7.1: "Fix issue with facet panel spacing when
  # axes do not appear on all panels."
  withr::local_options(list(theme61.sec_axis_msg = FALSE))

  df <- data.frame(
    x = rep(1:3, 2),
    y1 = c(1, 2, 3, 4, 5, 6),
    y2 = c(10, 20, 30, 40, 50, 60),
    g = rep(c("A", "B"), each = 3)
  )

  p <- ggplot(df, aes(x)) +
    geom_point(aes(y = y1)) +
    geom_point(aes(y = sec_rescale_inv(y2, scale = 10))) +
    scale_y_continuous_e61(
      limits = c(0, 10, 2),
      sec_axis = sec_axis(~sec_rescale(.), name = "%"),
      rescale_sec = TRUE
    ) +
    facet_wrap(~g) +
    labs_e61(y = "y", x = "x")

  expect_no_error(ggplot_build(p))
  expect_no_error(quiet_ggplotGrob(p))

  # theme61's facet_wrap() defaults to axes = "all", which should widen the
  # panel spacing set by maybe_adjust_facet_spacing()
  built <- ggplot2::ggplot_build(p)
  th <- built$plot@theme
  expect_equal(grid::convertUnit(th$panel.spacing.x, "lines", valueOnly = TRUE), 2)
  expect_equal(grid::convertUnit(th$panel.spacing.y, "lines", valueOnly = TRUE), 2)
})
