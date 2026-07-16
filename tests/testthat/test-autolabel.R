# End-to-end tests for the autolabel orchestrator (issue #159): given a
# plot and a set of labels to place, does the whole pipeline (mask,
# collision, distance, line-of-sight, selection) produce sensible,
# non-overlapping placements, and does it degrade gracefully when nothing
# can be placed?

autolabel_test_setup <- function() {
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
    labs(x = NULL, y = NULL)

  a <- data[data$series == "A", ]
  b <- data[data$series == "B", ]

  labels <- data.frame(
    text = c("Series A", "Series B"),
    geom_type = c("line", "line"),
    hjust = c(0, 0),
    size_mm = c(3.5, 3.5),
    fallback_x = c(2005, 2005), # deliberately bad placeholder, same spot
    fallback_y = c(1, 1)
  )
  labels$series <- list(list(x = a$x, y = a$y), list(x = b$x, y = b$y))

  list(plot = p, labels = labels, a = a, b = b)
}

boxes_overlap <- function(b1, b2) {
  !(b1$row_range[2] < b2$row_range[1] || b2$row_range[2] < b1$row_range[1] ||
    b1$col_range[2] < b2$col_range[1] || b2$col_range[2] < b1$col_range[1])
}

test_that("t61_autolabel_plot places both labels near their own series, without collisions", {
  skip_on_cran()

  setup <- autolabel_test_setup()
  result <- t61_autolabel_plot(setup$plot, setup$labels, width_cm = 16, height_cm = 12, px_width = 400)

  expect_true(all(result$placed))

  # Labels should not land on top of the underlying line ink, nor on top
  # of each other
  mask <- t61_render_mask(setup$plot, width_cm = 16, height_cm = 12, px_width = 400)
  cm_a <- t61_measure_label_cm("Series A", size_mm = 3.5, width_cm = 16, height_cm = 12)
  cm_b <- t61_measure_label_cm("Series B", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box_a <- t61_text_box_px(result$x[1], result$y[1], cm_a, mask, hjust = 0)
  box_b <- t61_text_box_px(result$x[2], result$y[2], cm_b, mask, hjust = 0)

  expect_false(t61_test_collision(mask$occupancy, box_a$row_range, box_a$col_range))
  expect_false(t61_test_collision(mask$occupancy, box_b$row_range, box_b$col_range))
  expect_false(boxes_overlap(box_a, box_b))

  # Each label should end up close (in y) to its own series near the x it
  # was placed at, not at some arbitrary unrelated spot
  nearest_a_y <- setup$a$y[which.min(abs(setup$a$x - result$x[1]))]
  nearest_b_y <- setup$b$y[which.min(abs(setup$b$x - result$x[2]))]
  expect_lt(abs(result$y[1] - nearest_a_y), 2)
  expect_lt(abs(result$y[2] - nearest_b_y), 2)
})

test_that("t61_place_label and t61_place_label_fallback return NULL when nothing is placeable", {
  skip_on_cran()

  setup <- autolabel_test_setup()
  mask <- t61_render_mask(setup$plot, width_cm = 16, height_cm = 12, px_width = 400)
  mask$occupancy[, ] <- TRUE # saturate: no clear space anywhere

  cm_a <- t61_measure_label_cm("Series A", size_mm = 3.5, width_cm = 16, height_cm = 12)

  expect_null(t61_place_label_fallback(mask, cm_a, hjust = 0))
  expect_null(t61_place_label(
    series = list(x = setup$a$x, y = setup$a$y), geom_type = "line",
    other_series = list(), mask = mask, label_cm = cm_a, hjust = 0
  ))
})

test_that("t61_autolabel_plot keeps the fallback position when placement fails entirely", {
  skip_on_cran()

  setup <- autolabel_test_setup()

  # Force every mask this call renders to come back fully saturated, so
  # there is truly nowhere to place the label anywhere in the pipeline.
  # (Capture the real implementation *before* mocking -- calling
  # t61_render_mask() from inside its own mock would recurse into itself.)
  orig_render_mask <- t61_render_mask
  testthat::local_mocked_bindings(
    t61_render_mask = function(...) {
      m <- orig_render_mask(...)
      m$occupancy[, ] <- TRUE
      m
    }
  )

  one_label <- setup$labels[1, ]
  result <- t61_autolabel_plot(setup$plot, one_label, width_cm = 16, height_cm = 12, px_width = 400)

  expect_false(result$placed)
  expect_equal(result$x, one_label$fallback_x)
  expect_equal(result$y, one_label$fallback_y)
})
