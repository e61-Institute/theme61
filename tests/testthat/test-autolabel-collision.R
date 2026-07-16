# Tests for collision detection between a candidate label's bounding box
# and the occupancy mask (issue #159).

test_that("t61_measure_label_cm returns a positive, sensible text size", {
  skip_on_cran()

  sz <- t61_measure_label_cm("Source A", size_mm = 3.5)
  expect_gt(sz$width_cm, 0)
  expect_gt(sz$height_cm, 0)

  bigger <- t61_measure_label_cm("Source A", size_mm = 7)
  expect_gt(bigger$width_cm, sz$width_cm)
  expect_gt(bigger$height_cm, sz$height_cm)

  longer <- t61_measure_label_cm("A much longer label string", size_mm = 3.5)
  expect_gt(longer$width_cm, sz$width_cm)
})

test_that("collision detection matches known-occupied and known-clear regions", {
  skip_on_cran()

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

  mask <- t61_render_mask(p, width_cm = 16, height_cm = 12, px_width = 400)
  lab_cm <- t61_measure_label_cm("Source A", size_mm = 3.5, width_cm = 16, height_cm = 12)

  box_on_line <- t61_text_box_px(2010, 2.5, lab_cm, mask, hjust = 0.5, vjust = 0.5)
  expect_true(t61_test_collision(mask$occupancy, box_on_line$row_range, box_on_line$col_range))

  # Clear margin above both lines near the right edge
  box_empty <- t61_text_box_px(2019, 9, lab_cm, mask, hjust = 0, vjust = 0.5)
  expect_false(t61_test_collision(mask$occupancy, box_empty$row_range, box_empty$col_range))

  # Once stamped in, the same box collides with itself
  occ2 <- t61_stamp_box(mask$occupancy, box_empty$row_range, box_empty$col_range)
  expect_true(t61_test_collision(occ2, box_empty$row_range, box_empty$col_range))

  # A box entirely off the raster can't be placed
  expect_true(t61_test_collision(mask$occupancy, c(-100, -50), c(-100, -50)))
})
