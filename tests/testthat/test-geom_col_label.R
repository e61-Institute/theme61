test_that("Single columns show each column's share of the total", {
  data <- data.frame(grp = c("A", "B", "C"), value = c(10, 30, 60))

  p <- ggplot(data, aes(grp, value)) +
    geom_col() +
    geom_col_label()

  built <- ggplot_build(p)
  label_data <- built$data[[2]]

  expect_equal(label_data$label, c("10%", "30%", "60%"))
  # "top" alignment on single columns nudges the label above the bar
  expect_equal(label_data$vjust, c(0, 0, 0))
})

test_that("Stacked columns show each segment's share of its column", {
  data <- data.frame(
    x = rep(c("2023", "2024"), each = 2),
    grp = rep(c("Group 1", "Group 2"), 2),
    value = c(30, 70, 45, 55)
  )

  p <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col() +
    geom_col_label(align = "middle")

  built <- ggplot_build(p)
  label_data <- built$data[[2]]

  expect_setequal(label_data$label, c("30%", "70%", "45%", "55%"))
  expect_true(all(label_data$vjust == 0.5))
})

test_that("align accepts top/middle/bottom and numeric 0-1", {
  data <- data.frame(x = "a", grp = c("g1", "g2"), value = c(25, 75))

  build_align <- function(align) {
    p <- ggplot(data, aes(x, value, fill = grp)) +
      geom_col() +
      geom_col_label(align = align)
    ggplot_build(p)$data[[2]]$y
  }

  expect_equal(build_align("top"), build_align(1))
  expect_equal(build_align("middle"), build_align(0.5))
  expect_equal(build_align("bottom"), build_align(0))
  expect_false(isTRUE(all.equal(build_align("top"), build_align("bottom"))))
})

test_that("Unrecognised align strings raise an error", {
  expect_error(geom_col_label(align = "sideways"))
})

test_that("Numeric align values outside 0-1 are clamped, not errors", {
  data <- data.frame(x = "a", grp = c("g1", "g2"), value = c(25, 75))

  p_clamped <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col() +
    geom_col_label(align = 2)

  p_top <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col() +
    geom_col_label(align = 1)

  expect_equal(ggplot_build(p_clamped)$data[[2]]$y, ggplot_build(p_top)$data[[2]]$y)
})

test_that("accuracy is passed through to the percentage formatter", {
  data <- data.frame(x = "a", grp = c("g1", "g2"), value = c(1, 2))

  p <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col() +
    geom_col_label(accuracy = 0.1)

  built <- ggplot_build(p)
  expect_setequal(built$data[[2]]$label, c("33.3%", "66.7%"))
})

test_that("... arguments pass through to geom_text", {
  data <- data.frame(grp = c("A", "B"), value = c(1, 2))

  p <- ggplot(data, aes(grp, value)) +
    geom_col() +
    geom_col_label(colour = "white", size = 5)

  built <- ggplot_build(p)
  expect_true(all(built$data[[2]]$colour == "white"))
  expect_true(all(built$data[[2]]$size == 5))
})

test_that("Labels and positions are unaffected by coord_flip()", {
  data <- data.frame(grp = c("A", "B", "C"), value = c(10, 30, 60))

  p <- ggplot(data, aes(grp, value)) + geom_col() + geom_col_label()
  p_flipped <- p + coord_flip()

  label_data <- ggplot_build(p)$data[[2]]
  label_data_flipped <- ggplot_build(p_flipped)$data[[2]]

  expect_equal(label_data$label, label_data_flipped$label)
  expect_equal(label_data$y, label_data_flipped$y)
  expect_equal(label_data$vjust, label_data_flipped$vjust)
})

test_that("Headroom is reserved beyond single top-aligned columns", {
  data <- data.frame(grp = c("A", "B", "C"), value = c(10, 30, 60))

  p_top <- ggplot(data, aes(grp, value)) + geom_col() + geom_col_label(align = "top")
  p_middle <- ggplot(data, aes(grp, value)) + geom_col() + geom_col_label(align = "middle")

  range_top <- ggplot_build(p_top)$layout$panel_scales_y[[1]]$get_limits()
  range_middle <- ggplot_build(p_middle)$layout$panel_scales_y[[1]]$get_limits()

  expect_gt(range_top[2], 60)
  expect_equal(range_middle[2], 60)
})

test_that("Headroom is reserved beyond stacked columns at align top/bottom too", {
  # The outermost segment's edge coincides with the panel boundary at
  # align = "top"/"bottom", so it needs the same reserved headroom as a
  # single column - otherwise the (vjust = 0.5) label glyph is clipped by
  # the panel edge, since theme61's y scale has no expansion there.
  data <- data.frame(
    x = rep(c("2023", "2024"), each = 2),
    grp = rep(c("Group 1", "Group 2"), 2),
    value = c(30, 70, 45, 55)
  )

  p_top <- ggplot(data, aes(x, value, fill = grp)) + geom_col() + geom_col_label(align = "top")
  p_bottom <- ggplot(data, aes(x, value, fill = grp)) + geom_col() + geom_col_label(align = "bottom")
  p_middle <- ggplot(data, aes(x, value, fill = grp)) + geom_col() + geom_col_label(align = "middle")

  range_top <- ggplot_build(p_top)$layout$panel_scales_y[[1]]$get_limits()
  range_bottom <- ggplot_build(p_bottom)$layout$panel_scales_y[[1]]$get_limits()
  range_middle <- ggplot_build(p_middle)$layout$panel_scales_y[[1]]$get_limits()

  expect_gt(range_top[2], 100)
  expect_lt(range_bottom[1], 0)
  expect_equal(range_middle, c(0, 100))
})

test_that("Reserved headroom respects an explicit scale_y_continuous_e61(limits = ...)", {
  # The reserved headroom must never push data outside a user-supplied
  # limit - scale_y_continuous_e61() errors if it does, so a tight
  # explicit limit (exactly at the stack total) must not error.
  data <- data.frame(
    x = rep(c("2023", "2024"), each = 2),
    grp = rep(c("Group 1", "Group 2"), 2),
    value = c(30, 70, 45, 55)
  )

  p <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col() +
    geom_col_label(align = "top") +
    scale_y_continuous_e61(limits = c(0, 100, 20))

  expect_no_error(built <- ggplot_build(p))

  range_y <- built$layout$panel_scales_y[[1]]$get_limits()
  expect_equal(range_y[2], 100)
})
