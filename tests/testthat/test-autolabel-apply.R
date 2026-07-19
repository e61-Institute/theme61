# Tests for wiring the autolabel engine into plot_label()/save_e61()
# (issue #159): does t61_apply_autolabel() find eligible plot_label()
# labels, match them to their series by colour, and move them -- while
# leaving everything out of v1 scope (opted-out, rotated, unmatched,
# facetted) exactly where the user put it?

autolabel_apply_test_setup <- function(auto_position = TRUE) {
  data <- data.frame(
    x = rep(2000:2020, 2),
    y = c(seq(0, 5, length.out = 21), seq(10, 2, length.out = 21)),
    series = rep(c("A", "B"), each = 21)
  )

  p <- ggplot(data, aes(x, y, colour = series)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL) +
    plot_label(
      c("Series A", "Series B"),
      x = c(2005, 2005), y = c(1, 1), # deliberately bad, same spot
      colour = c("#e57200", "#1c3144"),
      auto_position = auto_position
    )

  p
}

test_that("t61_apply_autolabel moves matching labels away from a bad fallback", {
  skip_on_cran()

  p <- autolabel_apply_test_setup()
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  # Moved away from the shared, deliberately bad fallback position
  expect_false(isTRUE(all.equal(d$x[1], 2005)) && isTRUE(all.equal(d$y[1], 1)))
  expect_false(isTRUE(all.equal(d$x[2], 2005)) && isTRUE(all.equal(d$y[2], 1)))

  # Each label should have moved close to its own series' colour-matched line
  expect_lt(abs(d$y[1] - 2.5), 3) # series A ranges 0-5
  expect_lt(abs(d$y[2] - 6), 5)   # series B ranges 2-10
})

test_that("t61_apply_autolabel leaves auto_position = FALSE labels untouched", {
  skip_on_cran()

  p <- autolabel_apply_test_setup(auto_position = FALSE)
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, c(2005, 2005))
  expect_equal(d$y, c(1, 1))
})

test_that("t61_apply_autolabel skips rotated labels (v1 scope: angle = 0 only)", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2005, y = 1, colour = "#e57200", angle = 45)

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, 2005)
  expect_equal(d$y, 1)
})

test_that("t61_apply_autolabel keeps the fallback when the label colour matches no series", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Unrelated", x = 2005, y = 1, colour = "#123456")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, 2005)
  expect_equal(d$y, 1)
})

test_that("t61_apply_autolabel matches point-geom series too", {
  skip_on_cran()

  data <- data.frame(x = 2000:2010, y = seq(0, 5, length.out = 11))
  p <- ggplot(data, aes(x, y)) +
    geom_point(colour = "#e57200", size = 2) +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2005, y = 1, colour = "#e57200")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(isTRUE(all.equal(d$x, 2005)) && isTRUE(all.equal(d$y, 1)))
})

test_that("t61_apply_autolabel is a no-op when there are no plot_label() layers", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10)

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)
  expect_identical(result, p)
})

test_that("t61_apply_autolabel keeps fallbacks for facetted plots (not v1 scope)", {
  skip_on_cran()

  data <- data.frame(
    x = rep(2000:2010, 2),
    y = seq(0, 5, length.out = 22),
    grp = rep(c("p1", "p2"), each = 11)
  )
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    facet_wrap(~grp) +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2005, y = 1, colour = "#e57200", panel = list(grp = "p1"))

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, 2005)
  expect_equal(d$y, 1)
})

test_that("save_e61() end-to-end repositions eligible plot_label() text", {
  skip_on_cran()

  p <- autolabel_apply_test_setup()

  out <- tempfile(fileext = ".svg")
  expect_no_error(
    save_e61(out, plot = p, preview = TRUE, spell_check = FALSE)
  )
})

test_that("t61_match_label_series matches by colour and returns NULL otherwise", {
  skip_on_cran()

  p <- autolabel_apply_test_setup()
  built_data <- ggplot2::ggplot_build(p)$data

  match_a <- t61_match_label_series(p@layers, built_data, "#e57200")
  expect_equal(match_a$geom_type, "line")
  expect_true(all(match_a$x %in% 2000:2020))

  expect_null(t61_match_label_series(p@layers, built_data, "#ffffff"))
  expect_null(t61_match_label_series(p@layers, built_data, "not-a-colour"))
})

test_that("t61_match_label_series matches a column series by fill, position-adjusted", {
  skip_on_cran()

  data <- data.frame(
    year = rep(2015:2017, 2),
    value = c(1, 2, 3, 4, 5, 6),
    series = rep(c("A", "B"), each = 3)
  )
  p <- ggplot(data, aes(year, value, fill = series)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10)

  built_data <- ggplot2::ggplot_build(p)$data
  match_a <- t61_match_label_series(p@layers, built_data, "#e57200")

  expect_equal(match_a$geom_type, "column")
  expect_length(match_a$xmin, 3)
  expect_true(all(match_a$xmax > match_a$xmin))
})

test_that("t61_match_label_series matches an area series by fill and carries alpha", {
  skip_on_cran()

  data <- data.frame(year = 2000:2010, value = seq(1, 21, 2))
  p <- ggplot(data, aes(year, value)) +
    geom_area(fill = "#e57200", alpha = 0.5) +
    theme_bw(base_size = 10)

  built_data <- ggplot2::ggplot_build(p)$data
  match_a <- t61_match_label_series(p@layers, built_data, "#e57200")

  expect_equal(match_a$geom_type, "area")
  expect_equal(match_a$fill, "#e57200")
  expect_equal(match_a$alpha, 0.5)
  expect_length(match_a$ymax, 11)
})

test_that("t61_match_label_series matches geom_pointbar() by colour, ahead of its point layer", {
  skip_on_cran()

  data <- data.frame(
    x = 1:5, y = c(2, 3, 1, 4, 3),
    ymin = c(1.5, 2.2, 0.5, 3.1, 2.3), ymax = c(2.5, 3.8, 1.5, 4.9, 3.7)
  )
  p <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(colour = "#e57200") +
    theme_bw(base_size = 10)

  built_data <- ggplot2::ggplot_build(p)$data
  match_a <- t61_match_label_series(p@layers, built_data, "#e57200")

  expect_equal(match_a$geom_type, "pointbar")
  expect_equal(match_a$ymin, data$ymin)
  expect_equal(match_a$ymax, data$ymax)
})

test_that("t61_apply_autolabel repositions a column label clear of every bar", {
  skip_on_cran()

  data <- data.frame(
    year = rep(2015:2024, 2),
    value = c(seq(10, 19), seq(15, 24)),
    series = rep(c("A", "B"), each = 10)
  )
  p <- ggplot(data, aes(year, value, fill = series)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10) +
    plot_label(c("A", "B"), x = c(2016, 2016), y = c(1, 1), colour = c("#e57200", "#1c3144"))

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(isTRUE(all.equal(d$x[1], 2016)) && isTRUE(all.equal(d$y[1], 1)))
  expect_false(isTRUE(all.equal(d$x[2], 2016)) && isTRUE(all.equal(d$y[2], 1)))

  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm_a <- t61_measure_label_cm("A", size_mm = 3.5, width_cm = 16, height_cm = 12)
  cm_b <- t61_measure_label_cm("B", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box_a <- t61_text_box_px(d$x[1], d$y[1], cm_a, mask, hjust = 0)
  box_b <- t61_text_box_px(d$x[2], d$y[2], cm_b, mask, hjust = 0)

  expect_false(t61_test_collision(mask$occupancy, box_a$row_range, box_a$col_range))
  expect_false(t61_test_collision(mask$occupancy, box_b$row_range, box_b$col_range))
})

test_that("t61_apply_autolabel places an area label inside its band with a contrasting colour", {
  skip_on_cran()

  data <- data.frame(year = 2000:2020, value = seq(20, 40))
  p <- ggplot(data, aes(year, value)) +
    geom_area(fill = "#1c3144", alpha = 1) + # a dark fill -> expect white text
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2001, y = 1, colour = "#1c3144") # deliberately bad, off the band

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(isTRUE(all.equal(d$x, 2001)) && isTRUE(all.equal(d$y, 1)))
  expect_gt(d$y, 0) # moved up into the visible (growing) band, not left at y=1

  expect_equal(result@layers[[label_layer]]$aes_params$colour, "white")
})

test_that("t61_apply_autolabel repositions a geom_pointbar() label clear of the error bars", {
  skip_on_cran()

  data <- data.frame(x = 2000:2010, y = seq(0, 5, length.out = 11))
  data$ymin <- data$y - 1
  data$ymax <- data$y + 1

  p <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(colour = "#e57200") +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2005, y = -3, colour = "#e57200") # deliberately bad, below the bars

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(isTRUE(all.equal(d$x, 2005)) && isTRUE(all.equal(d$y, -3)))

  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Series A", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box <- t61_text_box_px(d$x, d$y, cm, mask, hjust = 0)

  expect_false(t61_test_collision(mask$occupancy, box$row_range, box$col_range))
})

# x/y are optional when auto_position = TRUE (see ?plot_label): the
# fallback order is (1) a good spot found by the placement algorithm, (2)
# the caller's own x/y if they gave one, (3) any empty space at all if
# they didn't. These tests cover (1) and (3) end-to-end, and (2)'s
# priority over (3) directly.

test_that("t61_apply_autolabel resolves a real position with no x/y supplied at all", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Series A", colour = "#e57200")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(is.na(d$x))
  expect_false(is.na(d$y))
})

test_that("t61_apply_autolabel still resolves a position when no series matches and no x/y was supplied", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Unrelated", colour = "#123456") # matches no series, and no x/y given

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  # Not NA (the old "keep the fallback" behaviour would leave it invisible,
  # since there's no fallback to keep any more), and it shouldn't collide
  # with the line
  expect_false(is.na(d$x))
  expect_false(is.na(d$y))

  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Unrelated", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box <- t61_text_box_px(d$x, d$y, cm, mask, hjust = 0)
  expect_false(t61_test_collision(mask$occupancy, box$row_range, box$col_range))
})

test_that("t61_apply_autolabel prefers the caller's own x/y over random empty space", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2019, y = 4.5, colour = "#e57200")

  # Force the "good spot" algorithm (tier 1) to fail, so the fallback
  # tiers decide the outcome; the mask is otherwise wide open, so an "any
  # empty space" search (tier 3) would trivially succeed too -- the test
  # is which one wins.
  testthat::local_mocked_bindings(t61_place_label = function(...) NULL)

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, 2019)
  expect_equal(d$y, 4.5)
})

test_that("t61_apply_autolabel keeps fallbacks for coord_flip() plots (not v1 scope)", {
  skip_on_cran()

  data <- data.frame(
    category = rep(c("A", "B", "C"), 2),
    value = c(5, 8, 3, 6, 2, 9),
    series = rep(c("X", "Y"), each = 3)
  )
  cols <- c(X = "#e57200", Y = "#1c3144")

  p <- ggplot(data, aes(category, value, colour = series, group = series)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = cols) +
    coord_flip() +
    theme_bw(base_size = 10) +
    plot_label(c("X", "Y"), x = c("C", "C"), y = c(8, 2), colour = unname(cols))

  # Previously, coord_flip()'s screen axes don't match the x/y aesthetics
  # (series matching and box-distance math both assume plain x-aes ->
  # screen-x), so a "resolved" position got written back in the wrong
  # coordinate space entirely -- landing outside the flipped scale's range
  # and silently dropped by ggplot2, rather than erroring or keeping the
  # caller's own position. t61_render_mask() now bails out (NULL) for
  # coord_flip(), same as it already does for facets, so the label keeps
  # exactly the position the caller gave it instead.
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, c("C", "C"))
  expect_equal(d$y, c(8, 2))
})
