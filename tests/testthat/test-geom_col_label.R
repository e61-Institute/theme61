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
