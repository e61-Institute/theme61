# The secondary axis breaks/labels the scale will actually draw with.
sec_axis_of <- function(scale) scale$secondary.axis

test_that("Test the function works in isolation", {
  expect_equal(sec_rescale_inv(c(10, 20, 30), scale = 0.1),
               c(100, 200, 300))

  expect_equal(sec_rescale_inv(c(10, 20, 30), shift = 10),
               c(20, 30, 40))

  expect_equal(sec_rescale(c(100, 200, 300), scale = 0.1, shift = 0),
               c(10, 20, 30))

  expect_equal(sec_rescale(c(20, 30, 40), scale = 1, shift = 10),
               c(10, 20, 30))
})

test_that("sec_rescale()/sec_rescale_inv() edge cases", {
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

test_that("sec_rescale_axis()/sec_rescale_inv() reject invalid scale/shift", {
  expect_error(sec_rescale_axis(scale = 0), "must not be 0")
  expect_error(sec_rescale_axis(scale = c(1, 2)), "single finite number")
  expect_error(sec_rescale_axis(scale = "a"), "single finite number")
  expect_error(sec_rescale_axis(shift = NA_real_), "single finite number")
  expect_error(sec_rescale_axis(shift = Inf), "single finite number")

  expect_error(sec_rescale_inv(1:3, scale = 0), "must not be 0")
  expect_error(sec_rescale_inv(1:3, shift = c(1, 2)), "single finite number")
})

test_that("sec_rescale_axis() returns a usable secondary axis object", {
  ax <- sec_rescale_axis(scale = 0.1, shift = 5, name = "%")

  # Same object type ggplot2::sec_axis() produces, so it can be handed to
  # ggplot2's own scales as well as to scale_y_continuous_e61()
  expect_s3_class(ax, "AxisSecondary")
  expect_identical(ax$name, "%")

  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point() +
    ggplot2::scale_y_continuous(sec.axis = ax)

  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("sec_rescale_axis() lines the secondary breaks up with the primary breaks", {
  sc <- scale_y_continuous_e61(
    limits = c(0, 25, 5),
    sec_axis = sec_rescale_axis(scale = 10, shift = 0, name = "%")
  )

  breaks <- seq(0, 25, 5)

  expect_equal(sec_axis_of(sc)$breaks, sec_rescale(breaks, scale = 10, shift = 0))
  expect_equal(as.numeric(sec_axis_of(sc)$labels),
               sec_rescale(breaks, scale = 10, shift = 0))
  expect_identical(sec_axis_of(sc)$name, "%")

  # shift as well as scale
  sc_shift <- scale_y_continuous_e61(
    limits = c(0, 25, 5),
    sec_axis = sec_rescale_axis(scale = 1, shift = 10)
  )
  expect_equal(sec_axis_of(sc_shift)$breaks, breaks - 10)
})

test_that("sec_rescale_axis() gets the right breaks with no scale/shift stashed (fresh session)", {
  # The bug in #352: scale_y_continuous_e61() used to read the scale/shift out
  # of t61_env at `+` time, before sec_rescale_inv() had ever run, so nothing
  # was there to read on a graph's first build.

  sc <- scale_y_continuous_e61(
    limits = c(0, 50, 10),
    sec_axis = sec_rescale_axis(scale = 0.1, shift = 0, name = "%")
  )

  expect_equal(sec_axis_of(sc)$breaks, seq(0, 50, 10) * 0.1)

  # And a whole graph built exactly once renders that axis
  p <- ggplot(data.frame(x = 1, y1 = 10, y2 = 2), aes(x)) +
    geom_point(aes(y = y1)) +
    geom_point(aes(y = sec_rescale_inv(y2, scale = 0.1, shift = 0))) +
    scale_y_continuous_e61(
      limits = c(0, 50, 10),
      sec_axis = sec_rescale_axis(scale = 0.1, shift = 0, name = "%")
    ) +
    labs_e61(y = "$")

  expect_no_error(ggplot_build(p))
  expect_gt(get_grob_width(quiet_ggplotGrob(p), grob_name = "axis-r"), 0)
})

test_that("two dual-axis graphs in one session each get their own breaks", {
  # The other half of #352: the second graph used to be formatted with the
  # first graph's scale/shift, because that is what was left in t61_env.

  make_plot <- function(scale, shift, limits) {
    ggplot(data.frame(x = 1, y1 = mean(limits[1:2]), y2 = 1), aes(x)) +
      geom_point(aes(y = y1)) +
      geom_point(aes(y = sec_rescale_inv(y2, scale = scale, shift = shift))) +
      scale_y_continuous_e61(
        limits = limits,
        sec_axis = sec_rescale_axis(scale = scale, shift = shift, name = "%")
      )
  }

  # y2 = 1 must land inside the [0, 25] primary limits once rescaled, i.e.
  # (1 + shift) / scale in [0, 25]
  p1 <- make_plot(scale = 10, shift = 0, limits = c(0, 25, 5))
  p2 <- make_plot(scale = 0.1, shift = 0.5, limits = c(0, 25, 5))

  sec_breaks <- function(p) {
    built <- ggplot2::ggplot_build(p)
    built$plot@scales$get_scales("y")$secondary.axis$breaks
  }

  # Build in both orders: neither graph picks up the other's rescaling
  expect_equal(sec_breaks(p1), seq(0, 25, 5) * 10)
  expect_equal(sec_breaks(p2), seq(0, 25, 5) * 0.1 - 0.5)
  expect_equal(sec_breaks(p1), seq(0, 25, 5) * 10)
})

test_that("sec_rescale_axis() objects can be reused across graphs without being mutated", {
  ax <- sec_rescale_axis(scale = 2, shift = 0)

  sc1 <- scale_y_continuous_e61(limits = c(0, 10, 5), sec_axis = ax)
  sc2 <- scale_y_continuous_e61(limits = c(0, 100, 50), sec_axis = ax)

  expect_equal(sec_axis_of(sc1)$breaks, c(0, 5, 10) * 2)
  expect_equal(sec_axis_of(sc2)$breaks, c(0, 50, 100) * 2)

  # The user's own object is untouched by either call
  expect_true(inherits(ax$breaks, "waiver"))
})

test_that("a rescaled secondary axis without explicit break increments still builds", {
  # No length-3 limits means there are no explicit primary breaks to rescale,
  # so ggplot2 derives the secondary breaks from the transform instead.
  p <- ggplot(data.frame(x = 1:3, y1 = c(1, 5, 9), y2 = c(2, 4, 6)), aes(x)) +
    geom_point(aes(y = y1)) +
    geom_point(aes(y = sec_rescale_inv(y2, scale = 0.5, shift = 0))) +
    scale_y_continuous_e61(sec_axis = sec_rescale_axis(scale = 0.5, shift = 0))

  expect_no_error(ggplot_build(p))
})

test_that("faceted plots with a rescaled secondary axis build and render without error", {
  # Regression test for NEWS 0.7.1: "Fix issue with facet panel spacing when
  # axes do not appear on all panels."
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
      sec_axis = sec_rescale_axis(scale = 10, name = "%")
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

test_that("scale_y_continuous_e61() no longer accepts rescale_sec", {
  # Hard-deprecated (not soft-deprecated): the old t61_env-based path was
  # dysfunctional enough that it was removed outright rather than kept
  # around for two releases. Passing it now just becomes a stray `...` arg
  # forwarded to ggplot2::scale_y_continuous(), which errors on it.
  expect_error(
    scale_y_continuous_e61(
      limits = c(0, 25, 5),
      sec_axis = ggplot2::sec_axis(~sec_rescale(., scale = 10, shift = 0)),
      rescale_sec = TRUE
    )
  )
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
