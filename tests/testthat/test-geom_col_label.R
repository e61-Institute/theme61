# Finds the rendered label rows regardless of which internal layer they end
# up in (geom_col_label() splits "interior" vs "edge-floating" labels across
# two geom_text() layers - see R/geom_col_label.R for why).
get_label_data <- function(built) {
  label_layers <- Filter(function(d) "label" %in% names(d) && nrow(d) > 0, built$data)
  do.call(rbind, label_layers)
}

test_that("Single columns show each column's share of the total", {
  data <- data.frame(grp = c("A", "B", "C"), value = c(10, 30, 60))

  p <- ggplot(data, aes(grp, value)) +
    geom_col() +
    geom_col_label()

  label_data <- get_label_data(ggplot_build(p))

  expect_setequal(label_data$label, c("10%", "30%", "60%"))
  # "top" alignment on single columns floats the label above the bar
  expect_equal(label_data$vjust, c(0, 0, 0))
  # ... with a gap left between the column top and the label, rather than
  # the label sitting flush against it
  expect_true(all(label_data$y > data$value))
})

test_that("Single columns at align = bottom sit inside the column, above its base", {
  data <- data.frame(grp = c("A", "B", "C"), value = c(10, 30, 60))

  p <- ggplot(data, aes(grp, value)) +
    geom_col() +
    geom_col_label(align = "bottom")

  label_data <- get_label_data(ggplot_build(p))

  expect_setequal(label_data$label, c("10%", "30%", "60%"))
  # sits just inside the column, above its base - not flush at y = 0, and
  # not floating below it either
  expect_true(all(label_data$y > 0))
  expect_true(all(label_data$y < data$value))
})

test_that("The gap for single columns scales with the data range", {
  gap_for <- function(value, align) {
    d <- data.frame(grp = "a", value = value)
    p <- ggplot(d, aes(grp, value)) + geom_col() + geom_col_label(align = align)
    label_y <- get_label_data(ggplot_build(p))$y
    if (align == "top") label_y - value else label_y
  }

  gap_small_top <- gap_for(2, "top")
  gap_large_top <- gap_for(2000, "top")
  gap_small_bottom <- gap_for(2, "bottom")
  gap_large_bottom <- gap_for(2000, "bottom")

  # positive gap above the column for "top" (label sits above, not flush) ...
  expect_gt(gap_small_top, 0)
  expect_gt(gap_large_top, 0)
  # positive gap above the base for "bottom" (label sits inside, not flush) ...
  expect_gt(gap_small_bottom, 0)
  expect_gt(gap_large_bottom, 0)
  # ... and both proportionately bigger for larger-scale data, not a fixed
  # constant gap
  expect_gt(gap_large_top, gap_small_top * 100)
  expect_gt(gap_large_bottom, gap_small_bottom * 100)
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

  label_data <- get_label_data(ggplot_build(p))

  expect_setequal(label_data$label, c("30%", "70%", "45%", "55%"))
  expect_true(all(label_data$vjust == 0.5))
})

test_that("align accepts top/middle/bottom and numeric 0-1", {
  data <- data.frame(x = "a", grp = c("g1", "g2"), value = c(25, 75))

  build_align <- function(align) {
    p <- ggplot(data, aes(x, value, fill = grp)) +
      geom_col() +
      geom_col_label(align = align)
    get_label_data(ggplot_build(p))$y
  }

  expect_equal(sort(build_align("top")), sort(build_align(1)))
  expect_equal(sort(build_align("middle")), sort(build_align(0.5)))
  expect_equal(sort(build_align("bottom")), sort(build_align(0)))
  expect_false(isTRUE(all.equal(sort(build_align("top")), sort(build_align("bottom")))))
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

  expect_equal(
    sort(get_label_data(ggplot_build(p_clamped))$y),
    sort(get_label_data(ggplot_build(p_top))$y)
  )
})

test_that("accuracy is passed through to the percentage formatter", {
  data <- data.frame(x = "a", grp = c("g1", "g2"), value = c(1, 2))

  p <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col() +
    geom_col_label(accuracy = 0.1)

  label_data <- get_label_data(ggplot_build(p))
  expect_setequal(label_data$label, c("33.3%", "66.7%"))
})

test_that("... arguments pass through to geom_text", {
  data <- data.frame(grp = c("A", "B"), value = c(1, 2))

  p <- ggplot(data, aes(grp, value)) +
    geom_col() +
    geom_col_label(colour = "white", size = 5)

  label_data <- get_label_data(ggplot_build(p))
  expect_true(all(label_data$colour == "white"))
  expect_true(all(label_data$size == 5))
})

test_that("An explicit vjust passed via ... overrides the default exactly once", {
  data <- data.frame(grp = c("A", "B"), value = c(1, 2))

  p <- ggplot(data, aes(grp, value)) +
    geom_col() +
    geom_col_label(vjust = 0.2)

  expect_no_error(built <- ggplot_build(p))
  label_data <- get_label_data(built)
  expect_true(all(label_data$vjust == 0.2))
})

test_that("Labels and positions are unaffected by coord_flip()", {
  data <- data.frame(grp = c("A", "B", "C"), value = c(10, 30, 60))

  p <- ggplot(data, aes(grp, value)) + geom_col() + geom_col_label()
  p_flipped <- p + coord_flip()

  label_data <- get_label_data(ggplot_build(p))
  label_data_flipped <- get_label_data(ggplot_build(p_flipped))

  expect_equal(sort(label_data$label), sort(label_data_flipped$label))
  expect_equal(sort(label_data$y), sort(label_data_flipped$y))
  expect_equal(label_data$vjust, label_data_flipped$vjust)
})

test_that("GeomTextFlipAware swaps hjust/vjust under coord_flip() but not otherwise", {
  # GeomText's own draw_panel() correctly transforms (x, y) for coord_flip()
  # but passes hjust/vjust straight through unswapped, so a label's "float
  # away from its anchor" direction (encoded in vjust pre-flip) silently
  # stops applying to the now-flipped value axis. Regression test for that:
  # geom_col_label()'s labels used to render centred on/straddling the
  # column's edge under coord_flip() instead of offset from it.
  data <- data.frame(grp = c("A", "B"), value = c(10, 30))

  p_flip <- ggplot(data, aes(grp, value)) + geom_col() + geom_col_label() + coord_flip()
  p_normal <- ggplot(data, aes(grp, value)) + geom_col() + geom_col_label()

  built_flip <- ggplot_build(p_flip)
  built_normal <- ggplot_build(p_normal)

  row_flip <- get_label_data(built_flip)[1, ]
  row_normal <- get_label_data(built_normal)[1, ]

  # both start from the same input justification (hjust = 0.5, vjust = 0 -
  # centred on x, floating above y)
  expect_equal(row_flip$hjust, 0.5)
  expect_equal(row_flip$vjust, 0)

  g_flip <- GeomTextFlipAware$draw_panel(
    row_flip, built_flip$layout$panel_params[[1]], ggplot2::coord_flip()
  )
  g_normal <- GeomTextFlipAware$draw_panel(
    row_normal, built_normal$layout$panel_params[[1]], ggplot2::coord_cartesian()
  )

  # swapped under coord_flip() ...
  expect_equal(g_flip$hjust, 0)
  expect_equal(g_flip$vjust, 0.5)
  # ... but left alone otherwise
  expect_equal(g_normal$hjust, 0.5)
  expect_equal(g_normal$vjust, 0)
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

test_that("Headroom below zero is not reserved for single bottom-aligned columns", {
  # The label now sits inside the column (above its base), so it no longer
  # needs headroom below y = 0 the way the old "floats below" design did.
  data <- data.frame(grp = c("A", "B", "C"), value = c(10, 30, 60))

  p <- ggplot(data, aes(grp, value)) + geom_col() + geom_col_label(align = "bottom")

  range_y <- ggplot_build(p)$layout$panel_scales_y[[1]]$get_limits()
  expect_equal(range_y[1], 0)
})

test_that("Stacked columns need no headroom at align top/bottom - the outer segment sits inside instead", {
  # The outermost segment's edge coincides with the panel boundary at
  # align = "top"/"bottom". Rather than reserving headroom for a label that
  # straddles it (vjust = 0.5), that one segment's label is pushed fully
  # inside the column (vjust = 1/0), so the range never needs to extend
  # past the stack total in either direction.
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

  expect_equal(range_top, c(0, 100))
  expect_equal(range_bottom, c(0, 100))
  expect_equal(range_middle, c(0, 100))
})

test_that("Every stacked segment's label sits inside its own edge, not just the outer one", {
  # Three-segment stack: align = "top"/"bottom" means near *each segment's
  # own* top/bottom, not just the outer edge of the stack as a whole - so
  # every segment gets pushed inward with a gap, including the two whose
  # own boundary is an interior one (between two coloured segments).
  data <- data.frame(
    x = "a",
    grp = c("g1", "g2", "g3"),
    value = c(20, 30, 50)
  )

  p_top <- ggplot(data, aes(x, value, fill = grp)) + geom_col() + geom_col_label(align = "top")
  built_top <- ggplot_build(p_top)
  label_data_top <- get_label_data(built_top)
  bar_data_top <- built_top$data[[1]]

  # every label sits below its own segment's top edge (ymax), pushed
  # fully inward (vjust = 1), not centred on it
  expect_true(all(label_data_top$vjust == 1))
  expect_true(all(sort(label_data_top$y) < sort(bar_data_top$ymax)))
  expect_true(all(sort(label_data_top$y) > sort(bar_data_top$ymin)))

  p_bottom <- ggplot(data, aes(x, value, fill = grp)) + geom_col() + geom_col_label(align = "bottom")
  built_bottom <- ggplot_build(p_bottom)
  label_data_bottom <- get_label_data(built_bottom)
  bar_data_bottom <- built_bottom$data[[1]]

  # every label sits above its own segment's bottom edge (ymin), pushed
  # fully inward (vjust = 0), not centred on it
  expect_true(all(label_data_bottom$vjust == 0))
  expect_true(all(sort(label_data_bottom$y) > sort(bar_data_bottom$ymin)))
  expect_true(all(sort(label_data_bottom$y) < sort(bar_data_bottom$ymax)))
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

test_that("position = 'dodge' computes each bar's own share, not a stacked total", {
  data <- data.frame(
    x = rep(c("A", "B"), each = 2), grp = rep(c("g1", "g2"), 2), value = c(10, 20, 30, 5)
  )

  p <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col(position = "dodge") +
    geom_col_label(position = "dodge")

  built <- ggplot_build(p)
  label_data <- get_label_data(built)

  # each bar's share of the panel-wide total (65), not a share of its x's
  # own stack (which position = "stack" would give: 33%/67%, 86%/14%)
  total <- sum(data$value)
  expect_setequal(label_data$label, scales::label_percent(accuracy = 1)(data$value / total))
})

test_that("position = 'dodge' places each label on its own dodged bar, not in the gap between bars", {
  data <- data.frame(
    x = rep(c("A", "B"), each = 2), grp = rep(c("g1", "g2"), 2), value = c(10, 20, 30, 5)
  )

  p <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col(position = "dodge") +
    geom_col_label(position = "dodge")

  built <- ggplot_build(p)
  label_data <- get_label_data(built)
  bar_data <- built$data[[1]]

  # labels land at each bar's own dodged x (matching geom_col's own dodged
  # bars), not all stacked at the shared category x
  expect_equal(sort(label_data$x), sort(bar_data$x))
  expect_equal(length(unique(label_data$x)), 4)

  # "top" (the default align) floats each label just above its own bar's
  # own value, not above the (non-existent) stack total
  expect_true(all(label_data$y > data$value))
  ordered <- label_data[order(label_data$x), ]
  expect_true(all(diff(ordered$y[order(data$value)]) >= 0))
})

test_that("position = 'dodge2' also dodges labels onto their own bar", {
  data <- data.frame(
    x = rep(c("A", "B"), each = 2), grp = rep(c("g1", "g2"), 2), value = c(10, 20, 30, 5)
  )

  p <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col(position = "dodge2") +
    geom_col_label(position = "dodge2")

  built <- ggplot_build(p)
  label_data <- get_label_data(built)

  expect_equal(length(unique(label_data$x)), 4)
  expect_true(all(label_data$y > data$value))
})

test_that("position = 'dodge' respects align = 'middle'/'bottom'", {
  data <- data.frame(
    x = rep(c("A", "B"), each = 2), grp = rep(c("g1", "g2"), 2), value = c(10, 20, 30, 5)
  )

  p_middle <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col(position = "dodge") +
    geom_col_label(position = "dodge", align = "middle")

  label_middle <- get_label_data(ggplot_build(p_middle))
  expect_equal(sort(label_middle$y), sort(data$value * 0.5))

  p_bottom <- ggplot(data, aes(x, value, fill = grp)) +
    geom_col(position = "dodge") +
    geom_col_label(position = "dodge", align = "bottom")

  label_bottom <- get_label_data(ggplot_build(p_bottom))
  expect_true(all(label_bottom$y > 0))
  expect_true(all(label_bottom$y < data$value))
})

test_that("Unrecognised position values raise an error", {
  expect_error(geom_col_label(position = "fill"))
})

test_that("A single column's own gap respects an explicit tight limit too", {
  # Regression test: the "float" layer's label position (not just the
  # spacer's reserved headroom) must also be clamped to a user-supplied
  # limit. A column whose value sits exactly at the limit, plus the usual
  # gap, used to push the label's own y outside that limit and error.
  data <- data.frame(grp = "a", value = 100)

  p_tight <- ggplot(data, aes(grp, value)) +
    geom_col() +
    geom_col_label() +
    scale_y_continuous_e61(limits = c(0, 100, 20))

  expect_no_error(built_tight <- ggplot_build(p_tight))
  label_data_tight <- get_label_data(built_tight)
  expect_equal(label_data_tight$y, 100)

  # ... but the gap is preserved in full when the limit leaves room for it
  p_room <- ggplot(data, aes(grp, value)) +
    geom_col() +
    geom_col_label() +
    scale_y_continuous_e61(limits = c(0, 120, 20))

  label_data_room <- get_label_data(ggplot_build(p_room))
  expect_gt(label_data_room$y, 100)
})
