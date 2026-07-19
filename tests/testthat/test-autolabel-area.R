# Tests for area-specific label placement (issue #159): does the inside-
# the-band search find genuinely comfortable spots, degrade correctly to
# the outside-placement fallback when the band is too narrow anywhere, and
# pick a sensible contrast colour against the fill?

test_that("t61_area_interpolate is piecewise-linear between data points", {
  series <- list(x = c(0, 10, 20), ymin = c(0, 0, 0), ymax = c(10, 20, 10))

  mid <- t61_area_interpolate(series, 5)
  expect_equal(mid$ymax, 15) # halfway between 10 and 20

  at_break <- t61_area_interpolate(series, 10)
  expect_equal(at_break$ymax, 20)
})

test_that("t61_area_band_extent finds the tightest band across an interval", {
  series <- list(x = c(0, 10, 20), ymin = c(0, 0, 0), ymax = c(10, 20, 10))

  # Interval straddling the peak: the tightest (smallest) ymax has to hold
  # across the *whole* interval, so it's set by the interval's own edge,
  # not the interior peak -- a plain max() over sampled ymax would wrongly
  # pick the peak and overstate the usable band.
  band <- t61_area_band_extent(series, 2, 18)
  expect_equal(band$ymax, t61_area_interpolate(series, 2)$ymax)
  expect_equal(band$ymin, 0)

  # Interval not fully covered by the series' x range: NULL, not extrapolated
  expect_null(t61_area_band_extent(series, -5, 5))
  expect_null(t61_area_band_extent(series, 15, 25))
})

test_that("t61_contrast_colour picks white for dark fills, black for light fills", {
  expect_equal(t61_contrast_colour("black"), "white")
  expect_equal(t61_contrast_colour("#000033"), "white")
  expect_equal(t61_contrast_colour("white"), "black")
  expect_equal(t61_contrast_colour("#ffffcc"), "black")
})

test_that("t61_blend_with_background blends a fill toward the background as alpha falls", {
  expect_equal(t61_blend_with_background("black", 1), "black") # opaque: unchanged

  fully_transparent <- t61_blend_with_background("black", 0)
  expect_equal(unname(grDevices::col2rgb(fully_transparent)[, 1]), c(255, 255, 255))

  half <- t61_blend_with_background("black", 0.5)
  expect_equal(unname(grDevices::col2rgb(half)[, 1]), c(128, 128, 128), tolerance = 1)
})

test_that("t61_strip_area_layers removes GeomArea/GeomRibbon but keeps other geoms", {
  skip_on_cran()

  data <- data.frame(x = 1:10, y = 1:10)
  p <- ggplot(data, aes(x, y)) +
    geom_area(fill = "#e57200") +
    geom_line(colour = "#1c3144") +
    theme_bw(base_size = 10)

  stripped <- t61_strip_area_layers(p)
  classes <- vapply(stripped@layers, function(ly) class(ly$geom)[1], character(1))

  expect_length(stripped@layers, 1)
  expect_equal(unname(classes), "GeomLine")
})

test_that("t61_place_label_area finds a comfortable, collision-free spot inside a wide band", {
  skip_on_cran()

  data <- data.frame(x = 0:20, y = rep(10, 21))
  p <- ggplot(data, aes(x, y)) +
    geom_area(fill = "#e57200") +
    theme_bw(base_size = 10) +
    coord_cartesian(ylim = c(0, 12))

  # As in t61_autolabel_plot(): the mask used for the inside-the-band
  # search must have area layers stripped out first, or the area's own
  # fill registers as blocking "ink" and no candidate could ever pass the
  # collision check (see t61_strip_area_layers()'s own documentation).
  mask <- t61_render_mask(t61_strip_area_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Wide", size_mm = 3.5, width_cm = 16, height_cm = 12)
  series <- list(x = data$x, ymin = rep(0, 21), ymax = data$y)

  result <- t61_place_label_area(series, mask = mask, label_cm = cm, hjust = 0)

  expect_false(is.null(result))
  box <- t61_text_box_px(result$x, result$y, cm, mask, hjust = 0)
  expect_false(t61_test_collision(mask$occupancy, box$row_range, box$col_range))
})

test_that("t61_place_label_area returns NULL when the band is too narrow everywhere", {
  skip_on_cran()

  data <- data.frame(x = 0:20, y = rep(0.05, 21))
  p <- ggplot(data, aes(x, y)) +
    geom_area(fill = "#e57200") +
    theme_bw(base_size = 10) +
    coord_cartesian(ylim = c(0, 10))

  # Stripped for the same reason as the "wide band" test above -- this
  # test needs the NULL result to come from the band genuinely being too
  # narrow (the slack check), not merely from an unstripped area's own
  # fill blocking every candidate regardless of band thickness.
  mask <- t61_render_mask(t61_strip_area_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Narrow", size_mm = 3.5, width_cm = 16, height_cm = 12)
  series <- list(x = data$x, ymin = rep(0, 21), ymax = data$y)

  result <- t61_place_label_area(series, mask = mask, label_cm = cm, hjust = 0)
  expect_null(result)
})
