# Tests for distance-to-series scoring (issue #159). Distance is computed
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
