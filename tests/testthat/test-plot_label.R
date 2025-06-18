test_that("Single plot label works", {
  p1 <- minimal_plot_label +
    plot_label(label = "Plot 1", x = 2, y = 2)

  withr::with_tempdir({
    suppressWarnings(expect_snapshot_file(save_e61("compare-plot.svg", p1)))
  })

})

test_that("Multi-plot labels work", {

  # aes - colour
  data <- data.frame(
    x = c(0, 1, 0, 1, 0, 1),
    y = c(1.1, 1.1, 2.1, 2.1, 3.1, 3.1),
    group = factor(c(1, 1, 2, 2, 3, 3))
  )

  p <- ggplot(data, aes(x, y, colour = group)) +
    geom_line() +
    plot_label(c("1", "2", "3"),
               rep(0.5, 3),
               c(1.3, 2.3, 3.3))

  withr::with_tempdir({
    suppressWarnings(expect_snapshot_file(save_e61("multi-label-plot-col.svg", p)))
  })

  # aes - fill
  data <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(1, 1, 2, 2),
    group = factor(c(1, 1, 2, 2))
  )

  p <- ggplot(data, aes(x, y, fill = group)) +
    geom_col(position = "dodge") +
    plot_label(c("1", "2"),
               c(-0.25, 0.25),
               c(1.2, 2.2))

  withr::with_tempdir({
    suppressWarnings(expect_snapshot_file(save_e61("multi-label-plot-fill.svg", p)))
  })

  # Works with facets too
  data <- data.frame(
    x = rep(c(0, 1, 0, 1, 0, 1), 2),
    y = rep(c(1.1, 1.1, 2.1, 2.1, 3.1, 3.1), 2),
    group = factor(rep(c(1, 1, 2, 2, 3, 3), 2)),
    facet = c(rep("A", 6), rep("B", 6))
  )

  p <- ggplot(data, aes(x, y, colour = group)) +
    facet_wrap(~facet) +
    geom_line() +
    scale_y_continuous_e61(c(1, 4, 1)) +
    plot_label(c("1", "2", "3"),
               rep(0.5, 3),
               c(1.3, 2.3, 3.3),
               facet_name = "facet", facet_value = "A")

  withr::with_tempdir({
    suppressWarnings(expect_snapshot_file(save_e61("multi-label-facet-plot.svg", p)))
  })

})

test_that("Plots with additional aes still work", {

  data <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(0.5, 0.5, 1.5, 1.5),
    linetype = factor(c(1, 1, 2, 2))
  )

  p <- ggplot(data, aes(x, y, colour = linetype, linetype = linetype)) +
    geom_line() +
    scale_y_continuous_e61(c(0, 2, 1)) +
    plot_label(c("Solid", "Dotted"),
               c(0.25, 0.25),
               c(0.75, 1.75))

  withr::with_tempdir({
    suppressWarnings(expect_snapshot_file(save_e61("label-with-extra-aes.svg", p)))
  })

})

test_that("String dates get converted to date dates properly", {

  retval <- plot_label("Label", x = "2020-01-01", y = 1)

  expect_equal(class(retval$data$x), "Date")

})

test_that("Specifying custom colours works in plot_label()", {

  # Default colours
  retval <- plot_label("Test", 1, 1)
  expect_equal(retval$aes_params$colour, palette_e61(1))

  # Custom colours
  retval <- plot_label("Test", 1, 1, colour = "#000000")
  expect_equal(retval$aes_params$colour, "#000000")

  retval <- plot_label(
    c("Test 1", "Test 2"),
    c(1, 1),
    c(1, 2),
    colour = c("#000000", "#cccccc"))
  expect_equal(retval$aes_params$colour, c("#000000", "#cccccc"))

  p <-
    minimal_plot_label +
    plot_label(
      c("Test 1", "Test 2"),
      c(1, 1),
      c(1, 2),
      colour = c("#000000", "#cccccc"))

  withr::with_tempdir({
    suppressWarnings(expect_snapshot_file(save_e61("label-cust-colours.svg", p)))
  })

})

test_that("Text and label plot labels work", {
  p1 <- minimal_plot_label +
    plot_label("label", 2, 2, geom = "label") +
    scale_y_continuous_e61()

  p2 <- minimal_plot_label +
    plot_label("text", 2, 2, geom = "text")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("label.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("text.svg", p2)))
  })

})

test_that("Specifying incorrect length of label characteristics fails", {
  expect_error({
    ggplot() + plot_label(rep("x", 3),
                           x = rep(0, 3),
                           y = rep(0, 3),
                           hjust = rep(0, 2))
  })


  expect_error({
    ggplot() + plot_label(rep("x", 3),
                           x = rep(0, 3),
                           y = rep(0, 3),
                           angle = rep(0, 2))
  })

})

test_that("Changing horizontal alignment of text works", {

  p1 <- minimal_plot_label +
    plot_label("Left-aligned text", 2, 1.5, hjust = 0) +
    plot_label("Centre-aligned text", 2, 2, hjust = 0.5) +
    plot_label("Right-aligned text", 2, 2.5, hjust = 1)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("horiz-align-change.svg", p1)))
  })
})

test_that("Label rotation works", {

  # Separate plot_labels work
  p1 <- minimal_plot_label +
    plot_label("Normal text", 0.5, 1.5, angle = 90) +
    plot_label("Vertical text", 1.5, 1.5, angle = 0) +
    plot_label("Diagonal text", 2.5, 1.5, angle = 45) +
    scale_y_continuous_e61(limits = c(0, 3))

  # plot_label with multiple angles works
  p2 <- minimal_plot_label +
    plot_label(c("Normal text", "Vertical text", "Diagonal text"),
                x = rep(2, 3),
                y = c(2.1, 2.3, 2.15),
                angle = c(0, 90, 45)) +
    scale_y_continuous_e61(c(0, 3))

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("rotate.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("rotate-multi.svg", p2)))
  })

})

test_that("Labels work on facets", {

  data <- data.frame(
    x = rep(c(1, 2), 2),
    y = rep(c(1, 2), 2),
    f_var = factor(c(1, 1, 2, 2)),
    group = factor(c(1, 2, 1, 2))
  )

  # Place labels on 1 facet only
  p1 <- ggplot(data, aes(x, y, colour = group)) +
    facet_wrap(~f_var) +
    geom_point() +
    scale_y_continuous_e61(c(0, 3, 1)) +
    plot_label(
      label = c("Lab 1", "Lab 2"),
      x = c(1.25, 1.75),
      y = c(1, 2),
      facet_name = "f_var",
      facet_value = "1"
    )

  # Place 1 labels on 1 facet and the other label on the other
  p2 <- ggplot(data, aes(x, y, colour = group)) +
    facet_wrap(~f_var) +
    geom_point() +
    scale_y_continuous_e61(c(0, 3, 1)) +
    plot_label(
      label = c("Lab 1", "Lab 2"),
      x = c(1.25, 1.75),
      y = c(1, 2),
      facet_name = "f_var",
      facet_value = c("1", "2")
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("facets.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("alternating-facets.svg", p2)))
  })

})

test_that("Labels work on facets", {

  data <- data.frame(
    x = rep(c(1, 2), 2),
    y = rep(c(1, 2), 2),
    f_var = factor(c(1, 1, 2, 2)),
    group = factor(c(1, 2, 1, 2))
  )

  # Place labels on 1 facet only
  p1 <- ggplot(data, aes(x, y, colour = group)) +
    facet_wrap(~f_var) +
    geom_point() +
    scale_y_continuous_e61(c(0, 3, 1)) +
    plot_label(
      label = c("Lab 1", "Lab 2"),
      x = c(1.25, 1.75),
      y = c(1, 2),
      facet_name = "f_var",
      facet_value = "1"
    )

  # Place 1 labels on 1 facet and the other label on the other
  p2 <- ggplot(data, aes(x, y, colour = group)) +
    facet_wrap(~f_var) +
    geom_point() +
    scale_y_continuous_e61(c(0, 3, 1)) +
    plot_label(
      label = c("Lab 1", "Lab 2"),
      x = c(1.25, 1.75),
      y = c(1, 2),
      facet_name = "f_var",
      facet_value = c("1", "2")
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("facets.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("alternating-facets.svg", p2)))
  })

})
