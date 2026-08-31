# These tests exercise the internal coordinate-mapping foundation for
# automatic plot_label() positioning directly (t61_render_mask()/
# t61_data_to_px()), rather than through plot_label()/save_e61(), so a
# failure points at the coordinate math itself rather than anything in the
# wiring layer.

mask_test_plot <- function() {
  data <- data.frame(
    x = rep(2000:2020, 2),
    y = c(seq(0, 5, length.out = 21), seq(10, 2, length.out = 21)),
    series = rep(c("A", "B"), each = 21)
  )
  ggplot(data, aes(x, y, colour = series)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10) +
    theme(legend.position = "none") +
    ggplot2::labs(x = NULL, y = NULL)
}

test_that("t61_render_mask locates known series coordinates as ink", {
  skip_on_cran()

  p <- mask_test_plot()
  mask <- t61_render_mask(p, width_cm = 16, height_cm = 12, px_width = 400)

  expect_false(is.null(mask))
  expect_true(is.matrix(mask$occupancy))
  expect_type(mask$occupancy, "logical")

  hits_ink <- function(x, y, win = 3) {
    loc <- t61_data_to_px(x, y, mask)
    r <- round(loc$row); cn <- round(loc$col)
    r_seq <- max(1, r - win):min(nrow(mask$occupancy), r + win)
    c_seq <- max(1, cn - win):min(ncol(mask$occupancy), cn + win)
    any(mask$occupancy[r_seq, c_seq])
  }

  # Series endpoints/midpoint should register as ink
  expect_true(hits_ink(2020, 5))
  expect_true(hits_ink(2000, 0))
  expect_true(hits_ink(2010, 2.5))

  # Empty whitespace should not (also guards against gridline pollution --
  # theme_bw()'s near-white gridlines must not register as "occupied")
  expect_false(hits_ink(2001, 9))
  expect_false(hits_ink(2019, 0.5))
})

test_that("t61_panel_box_cm returns NULL for faceted plots (not v1 scope)", {
  skip_on_cran()

  data <- data.frame(x = 1:4, y = 1:4, f = rep(c("a", "b"), 2))
  p <- ggplot(data, aes(x, y)) + geom_point() + facet_wrap(~f)

  gt <- quiet_ggplotGrob(p)
  expect_null(t61_panel_box_cm(gt, width_cm = 16, height_cm = 12))
})

test_that("t61_render_mask sets flipped = TRUE for coord_flip() plots, using panel_params' already-flipped axes", {
  skip_on_cran()

  data <- data.frame(category = c("A", "B", "C"), value = c(5, 8, 3))
  p <- ggplot(data, aes(category, value)) +
    geom_col() +
    coord_flip()

  mask <- t61_render_mask(p, width_cm = 16, height_cm = 12)

  expect_false(is.null(mask))
  expect_true(mask$flipped)
  # x_range now holds "value" (the screen's horizontal axis post-flip), not
  # "category" -- confirms the mask describes rendered/screen space, not
  # the raw aesthetics.
  expect_equal(mask$y_range[1] < mask$y_range[2], TRUE)
  expect_true(diff(mask$x_range) > 3) # value's range, not category's ~3-level span

  mask_unflipped <- t61_render_mask(ggplot(data, aes(category, value)) + geom_col(), width_cm = 16, height_cm = 12)
  expect_false(mask_unflipped$flipped)
})

test_that("t61_touches_y_gridline detects a box straddling a y-axis break", {
  mask <- list(
    panel = list(left_px = 0, top_px = 0, width_px = 100, height_px = 100),
    x_range = c(0, 10), y_range = c(0, 10),
    y_breaks = c(5)
  )

  # y = 5 sits at the panel's exact vertical midpoint (row 50)
  box_touching <- list(row_range = c(45, 55), col_range = c(10, 20))
  box_clear    <- list(row_range = c(10, 20), col_range = c(10, 20))

  expect_true(t61_touches_y_gridline(box_touching, mask))
  expect_false(t61_touches_y_gridline(box_clear, mask))

  # No y_breaks at all (e.g. x-only gridline theme): never a hit
  mask_no_breaks <- mask
  mask_no_breaks$y_breaks <- numeric(0)
  expect_false(t61_touches_y_gridline(box_touching, mask_no_breaks))
})

test_that("t61_strip_chrome preserves the gtable layout of the real chart", {
  skip_on_cran()

  p <- mask_test_plot()
  gt_real <- quiet_ggplotGrob(p)
  gt_mask <- quiet_ggplotGrob(t61_strip_chrome(p))

  widths_real <- vapply(gt_real$widths, grid::convertWidth, numeric(1), unitTo = "cm", valueOnly = TRUE)
  widths_mask <- vapply(gt_mask$widths, grid::convertWidth, numeric(1), unitTo = "cm", valueOnly = TRUE)

  expect_equal(widths_real, widths_mask)
})
