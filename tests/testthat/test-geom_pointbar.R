test_that("geom_pointbar visual tests", {

  # Test 1: Simple single regression coefficient ----
  test_that("Single regression coefficient", {
    withr::local_seed(42)

    # Single coefficient data (like a regression output)
    data <- data.frame(
      term = "Treatment Effect",
      estimate = 2.5,
      conf.low = 1.8,
      conf.high = 3.2,
      x = 1
    )

    p <- ggplot(data, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_pointbar() +
      labs_e61(title = "Single Regression Coefficient",
               y = "%")

    withr::with_tempdir({
      expect_snapshot_file(suppressWarnings(save_e61("pointbar-single-coef.svg", p)))
    })
  })

  # Test 2: Multiple point bars grouped by colour ----
  test_that("Multiple point bars grouped by colour", {
    withr::local_seed(42)

    # Multiple groups with different treatments
    data <- data.frame(
      treatment = rep(c("Control", "Treatment A", "Treatment B"), each = 3),
      time = rep(c("Week 1", "Week 2", "Week 3"), 3),
      mean_score = c(5.2, 6.1, 6.8, 7.3, 8.2, 9.1, 6.8, 7.9, 8.5),
      lower_ci = c(4.5, 5.3, 6.0, 6.5, 7.4, 8.3, 6.0, 7.1, 7.7),
      upper_ci = c(5.9, 6.9, 7.6, 8.1, 9.0, 9.9, 7.6, 8.7, 9.3),
      x_pos = rep(1:3, 3)
    )

    p <- ggplot(data, aes(x = x_pos, y = mean_score,
                          ymin = lower_ci, ymax = upper_ci,
                          colour = treatment)) +
      geom_pointbar(position = position_dodge(width = 0.3)) +
      scale_y_continuous_e61() +
      scale_x_continuous_e61() +
      scale_colour_e61() +
      labs_e61(title = "Multiple Groups by Colour",
               y = "score") +
      scale_x_continuous(breaks = 1:3, labels = c("Week 1", "Week 2", "Week 3")) +
      theme_e61(legend = "bottom")

    withr::with_tempdir({
      expect_snapshot_file(suppressWarnings(save_e61("pointbar-multiple-groups.svg", p)))
    })
  })

  # Test 3: Custom point sizes and error bar widths ----
  test_that("Custom point sizes and error bar widths", {
    withr::local_seed(42)

    # Data with varying effect sizes (different point sizes make sense)
    data <- data.frame(
      study = paste("Study", 1:5),
      effect_size = c(0.3, 0.8, 1.2, 0.5, 0.9),
      lower_bound = c(0.1, 0.5, 0.9, 0.2, 0.6),
      upper_bound = c(0.5, 1.1, 1.5, 0.8, 1.2),
      sample_size = c(50, 120, 200, 80, 150),
      x_pos = 1:5
    )

    p <- ggplot(data, aes(x = x_pos, y = effect_size,
                          ymin = lower_bound, ymax = upper_bound)) +
      geom_pointbar(
        point.size = 4,           # Large points
        errorbar.width = 0.4,     # Wide error bars
        errorbar.linewidth = 1.2 # Thick error bar lines
      ) +
      labs_e61(title = "Custom Point Sizes and Error Bar Widths",
               subtitle = "Large points (size=4), wide error bars (width=0.4), thick lines",
               y = "%") +
      scale_x_continuous(breaks = 1:5, labels = data$study)

          withr::with_tempdir({
      expect_snapshot_file(suppressWarnings(save_e61("pointbar-custom-sizes.svg", p)))
    })
  })

  # Test 4: Manual colour specification by group identifier ----
  test_that("Manual colour specification by group", {
    withr::local_seed(42)

    # Comparison of different methods/approaches
    data <- data.frame(
      method = rep(c("Method A", "Method B", "Method C", "Method D"), each = 2),
      metric = rep(c("Accuracy", "Precision"), 4),
      value = c(0.85, 0.82, 0.78, 0.85, 0.92, 0.88, 0.75, 0.79),
      lower = c(0.80, 0.77, 0.72, 0.80, 0.88, 0.83, 0.68, 0.73),
      upper = c(0.90, 0.87, 0.84, 0.90, 0.96, 0.93, 0.82, 0.85),
      x_pos = rep(c(1, 2), 4)
    )

    # Custom color palette using e61 colours
    method_colors <- c("Method A" = e61_coraldark,
                       "Method B" = e61_bluedark,
                       "Method C" = e61_tealdark,
                       "Method D" = e61_orangedark)

    p <- ggplot(data, aes(x = x_pos, y = value,
                          ymin = lower, ymax = upper,
                          colour = method)) +
      geom_pointbar(
        position = position_dodge(width = 0.4),
        point.size = 3,
        errorbar.width = 0.2
      ) +
      scale_colour_manual(values = method_colors) +
      scale_y_continuous_e61() +
      scale_x_continuous_e61() +
      labs_e61(title = "Manual Colour Specification by Group",
               subtitle = "Four methods with custom e61 color palette",
               x = "Metric Type", y = "score") +
      scale_x_continuous(breaks = 1:2, labels = c("Accuracy", "Precision")) +
      theme_e61(legend = "bottom")

    withr::with_tempdir({
      expect_snapshot_file(suppressWarnings(save_e61("pointbar-manual-colours.svg", p)))
    })
  })

  # Test 5: Edge case - Missing data handling ----
  test_that("Missing data handling", {
    withr::local_seed(42)

    # Data with some missing values
    data <- data.frame(
      x = 1:5,
      y = c(2.5, NA, 4.1, 3.8, NA),
      ymin = c(2.0, NA, 3.5, 3.2, NA),
      ymax = c(3.0, NA, 4.7, 4.4, NA)
    )

    p <- ggplot(data, aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
      geom_pointbar(na.rm = TRUE, colour = e61_coraldark) +
      scale_y_continuous_e61() +
      scale_x_continuous_e61() +
      labs_e61(title = "Missing Data Handling (na.rm = TRUE)",
               y = "%")

    withr::with_tempdir({
      expect_snapshot_file(suppressWarnings(save_e61("pointbar-missing-data.svg", p)))
    })
  })

  # Test 6: Different shapes and styling ----
  test_that("Different shapes and styling", {
    withr::local_seed(42)

    # Data for showing different aesthetic options
    data <- data.frame(
      category = c("A", "B", "C", "D"),
      value = c(3.2, 4.5, 2.8, 5.1),
      lower = c(2.5, 3.8, 2.1, 4.3),
      upper = c(3.9, 5.2, 3.5, 5.9),
      x_pos = 1:4
    )

    p <- ggplot(data, aes(x = x_pos, y = value, ymin = lower, ymax = upper)) +
      geom_pointbar(
        shape = 21,                    # Fillable circles
        fill = e61_skylight,          # Fill color using e61 palette
        colour = e61_skydark,         # Border color using e61 palette
        point.size = 5,               # Large points
        errorbar.width = 0.3,         # Error bar width
        errorbar.linewidth = 1.5,     # Thick error bars
        linetype = "dashed",          # Dashed error bars
        alpha = 0.8                   # Transparency
      ) +
      scale_y_continuous_e61() +
      scale_x_continuous_e61() +
      labs_e61(title = "Different Shapes and Styling",
               subtitle = "Shape 21, filled circles, dashed error bars with e61 colors",
               y = "n") +
      scale_x_continuous(breaks = 1:4, labels = data$category)

    withr::with_tempdir({
      expect_snapshot_file(suppressWarnings(save_e61("pointbar-shapes-styling.svg", p)))
    })
  })
})

# Additional unit tests for functionality (non-visual) ----

test_that("Required aesthetics validation", {
  data <- data.frame(x = 1:3, y = 1:3)

  # Should work with all required aesthetics
  p1 <- ggplot(data, aes(x = x, y = y, ymin = y - 0.5, ymax = y + 0.5)) +
    geom_pointbar()
  expect_s3_class(p1, "ggplot")

  # Should handle missing ymin/ymax gracefully in build
  p2 <- ggplot(data, aes(x = x, y = y)) +
    geom_pointbar()
  expect_s3_class(p2, "ggplot")
})

test_that("Parameter passing works correctly", {
  data <- data.frame(
    x = 1:3,
    y = 1:3,
    ymin = c(0.5, 1.5, 2.5),
    ymax = c(1.5, 2.5, 3.5)
  )

  # Test parameter creation
  p <- geom_pointbar(
    point.size = 5,
    errorbar.width = 0.5,
    errorbar.linewidth = 2
  )

  # Check that it returns a list of layers
  expect_type(p, "list")
  expect_length(p, 2)
  expect_s3_class(p[[1]], "Layer")
  expect_s3_class(p[[2]], "Layer")
})

test_that("Works with e61 theme functions", {
  data <- data.frame(
    x = 1:3,
    y = 1:3,
    ymin = c(0.5, 1.5, 2.5),
    ymax = c(1.5, 2.5, 3.5)
  )

  # Test integration with theme61 functions
  p <- ggplot(data, aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(colour = e61_bluedark) +
    scale_y_continuous_e61() +
    labs_e61(title = "Test") +
    theme_e61()

  expect_s3_class(p, "ggplot")
})
