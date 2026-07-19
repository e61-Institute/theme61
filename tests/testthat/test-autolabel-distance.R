# Tests for distance-to-series scoring. Distance is computed
# in physical cm (via t61_mask_units_cm()) so x and y are comparable even
# when their data scales differ substantially.

test_that("t61_point_segment_distance handles interior, endpoint and degenerate cases", {
  # Perpendicular distance to the middle of a horizontal segment
  d <- t61_point_segment_distance(5, 3, 0, 0, 10, 0)
  expect_equal(d$distance, 3)
  expect_equal(d$x, 5)
  expect_equal(d$y, 0)

  # Point beyond the segment's end clamps to the endpoint
  d2 <- t61_point_segment_distance(15, 0, 0, 0, 10, 0)
  expect_equal(d2$distance, 5)
  expect_equal(d2$x, 10)

  # Degenerate segment (A == B) is just point distance
  d3 <- t61_point_segment_distance(3, 4, 0, 0, 0, 0)
  expect_equal(d3$distance, 5)
})

test_that("t61_distance_to_series finds ~0 distance on the line and identifies the closer series", {
  skip_on_cran()

  data <- data.frame(
    x = rep(2000:2020, 2),
    y = c(seq(0, 5, length.out = 21), seq(10, 2, length.out = 21)),
    series = rep(c("A", "B"), each = 21)
  )
  a <- data[data$series == "A", ]
  b <- data[data$series == "B", ]

  p <- ggplot(data, aes(x, y, colour = series)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL)

  mask <- t61_render_mask(p, width_cm = 16, height_cm = 12, px_width = 400)
  units <- t61_mask_units_cm(mask)

  on_line <- t61_distance_to_series(2010, a$y[a$x == 2010], a$x, a$y, "line", units)
  expect_lt(on_line$distance, 1e-6)

  far <- t61_distance_to_series(2010, 9, a$x, a$y, "line", units)
  expect_gt(far$distance, 1)

  # y=3 near x=2010 sits closer to series A (~2.5 there) than series B (~6)
  dist_a <- t61_distance_to_series(2010, 3, a$x, a$y, "line", units)
  dist_b <- t61_distance_to_series(2010, 3, b$x, b$y, "line", units)
  expect_lt(dist_a$distance, dist_b$distance)

  # Nearest point should lie between the bracketing data points
  expect_gte(dist_a$x, 2009)
  expect_lte(dist_a$x, 2011)
})

test_that("t61_distance_to_series 'point' geom type snaps to discrete points, no interpolation", {
  units <- list(x_per_unit_cm = 1, y_per_unit_cm = 1)
  d <- t61_distance_to_series(5, 0, c(0, 10), c(0, 0), "point", units)

  expect_equal(d$distance, 5)
  expect_true(d$x %in% c(0, 10))
})

test_that("t61_distance_to_series rejects unsupported geom types", {
  units <- list(x_per_unit_cm = 1, y_per_unit_cm = 1)
  expect_error(
    t61_distance_to_series(0, 0, c(0, 1), c(0, 1), "bar", units),
    "unsupported geom_type"
  )
})

# t61_box_distance_to_series() measures from a candidate's actual box
# footprint rather than a single anchor point (see its own documentation
# for why); these use a synthetic mask with a 1:1 px:data-unit mapping and
# units of 1 cm per data-unit, so distances below are exactly the
# data-space Euclidean distance and easy to hand-check.
t61_synthetic_mask <- function(range = c(0, 100)) {
  list(
    panel = list(left_px = 0, top_px = 0, width_px = diff(range), height_px = diff(range)),
    x_range = range,
    y_range = range
  )
}

# Converts a data-space box [xmin,xmax] x [ymin,ymax] into the row/col
# ranges t61_box_distance_to_series() expects, under t61_synthetic_mask()'s
# 1:1 mapping (row 0 = top = y_range[2]).
t61_synthetic_box <- function(xmin, xmax, ymin, ymax, range = c(0, 100)) {
  list(col_range = c(xmin, xmax), row_range = c(range[2] - ymax, range[2] - ymin))
}

test_that("t61_box_distance_to_series 'point': exact point-to-box distance, nearest point wins", {
  mask <- t61_synthetic_mask()
  units <- list(x_per_unit_cm = 1, y_per_unit_cm = 1)
  box <- t61_synthetic_box(10, 20, 10, 20)

  series <- list(x = c(25, 90), y = c(15, 90))
  d <- t61_box_distance_to_series(box, mask, series, "point", units)

  expect_equal(d$distance, 5) # box's right edge (x=20) to the point at x=25, same y band
  expect_equal(d$x, 25)
  expect_equal(d$y, 15)
})

test_that("t61_box_distance_to_series 'line': exact box-to-segment distance", {
  mask <- t61_synthetic_mask()
  units <- list(x_per_unit_cm = 1, y_per_unit_cm = 1)
  box <- t61_synthetic_box(10, 20, 10, 20)

  # Horizontal segment at y=50 spanning the box's whole x-range: distance
  # is the vertical gap from the box's top edge (y=20) to the line
  series <- list(x = c(0, 100), y = c(50, 50))
  d <- t61_box_distance_to_series(box, mask, series, "line", units)

  expect_equal(d$distance, 30)
})

test_that("t61_box_distance_to_series 'column': exact rect-to-rect distance, nearest bar wins", {
  mask <- t61_synthetic_mask()
  units <- list(x_per_unit_cm = 1, y_per_unit_cm = 1)
  box <- t61_synthetic_box(10, 20, 10, 20)

  # Bar 1 sits just right of the box with overlapping y-range (gap = 5);
  # bar 2 is far away
  series <- list(xmin = c(25, 80), xmax = c(30, 85), ymin = c(0, 0), ymax = c(15, 15))
  d <- t61_box_distance_to_series(box, mask, series, "column", units)

  expect_equal(d$distance, 5)
})

test_that("t61_box_distance_to_series 'pointbar': each x is an independent vertical segment", {
  mask <- t61_synthetic_mask()
  units <- list(x_per_unit_cm = 1, y_per_unit_cm = 1)
  box <- t61_synthetic_box(10, 20, 10, 20)

  # Error bar 1 at x=25 spans y in [8,15] (overlaps the box's y-range);
  # error bar 2 is far away
  series <- list(x = c(25, 90), y = c(12, 90), ymin = c(8, 85), ymax = c(15, 95))
  d <- t61_box_distance_to_series(box, mask, series, "pointbar", units)

  expect_equal(d$distance, 5)
})

test_that("t61_box_distance_to_series 'area': treats the ymax boundary exactly like a 'line'", {
  mask <- t61_synthetic_mask()
  units <- list(x_per_unit_cm = 1, y_per_unit_cm = 1)
  box <- t61_synthetic_box(10, 20, 10, 20)

  series <- list(x = c(0, 100), ymin = c(0, 0), ymax = c(50, 50))
  d_area <- t61_box_distance_to_series(box, mask, series, "area", units)
  d_line <- t61_box_distance_to_series(box, mask, list(x = series$x, y = series$ymax), "line", units)

  expect_equal(d_area, d_line)
})

test_that("t61_box_distance_to_series rejects unsupported geom types", {
  mask <- t61_synthetic_mask()
  units <- list(x_per_unit_cm = 1, y_per_unit_cm = 1)
  box <- t61_synthetic_box(10, 20, 10, 20)

  expect_error(
    t61_box_distance_to_series(box, mask, list(x = 0, y = 0), "bar", units),
    "unsupported geom_type"
  )
})
