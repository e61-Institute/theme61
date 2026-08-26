test_that("geom_pointbar() creates errorbar + point layers in correct order", {
  d <- data.frame(
    x = 1:3,
    y = c(2.5, 3.0, 1.5),
    ymin = c(2.0, 2.6, 1.2),
    ymax = c(3.0, 3.4, 1.8)
  )

  p <- ggplot(d, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar()

  expect_equal(length(p@layers), 2)
  expect_true(inherits(p@layers[[1]]$geom, "GeomErrorbar"))
  expect_true(inherits(p@layers[[2]]$geom, "GeomPoint"))

  # Point layer must not carry ymin / ymax
  expect_null(p@layers[[2]]$mapping$ymin)
  expect_null(p@layers[[2]]$mapping$ymax)
})

test_that("geom_pointbar() forwards styling arguments to the correct geom", {
  d <- data.frame(
    x = 1:2,
    y = c(1, 2),
    ymin = c(0.5, 1.5),
    ymax = c(1.5, 2.5)
  )

  p <- ggplot(d, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(
      point.size = 4,
      errorbar.width = 0.4,
      errorbar.linewidth = 1.2,
      linetype = "dashed",
      alpha = 0.8,
      shape = 21,
      fill = e61_skylight,
      colour = e61_skydark
    )

  eb <- p@layers[[1]]
  pt <- p@layers[[2]]

  # Errorbar params
  expect_equal(eb$aes_params$width, 0.4)
  expect_equal(eb$aes_params$linewidth, 1.2)
  expect_equal(eb$aes_params$linetype, "dashed")
  expect_equal(eb$aes_params$alpha, 0.8)

  # Point params (stored inconsistently across ggplot2 versions)
  getp <- function(layer, nm) {
    if (!is.null(layer$aes_params[[nm]])) layer$aes_params[[nm]] else layer$params[[nm]]
  }

  expect_equal(getp(pt, "size"), 4)
  expect_equal(getp(pt, "shape"), 21)
  expect_equal(getp(pt, "fill"), e61_skylight)
  expect_equal(getp(pt, "colour"), e61_skydark)
  expect_equal(getp(pt, "alpha"), 0.8)
})

test_that("geom_pointbar() supports grouping and position_dodge", {
  d <- data.frame(
    group = rep(c("A", "B"), each = 2),
    x = rep(1:2, 2),
    y = c(5, 6, 7, 8),
    ymin = c(4, 5, 6, 7),
    ymax = c(6, 7, 8, 9)
  )

  pos <- position_dodge(width = 0.3)

  p <- ggplot(d, aes(x, y, ymin = ymin, ymax = ymax, colour = group)) +
    geom_pointbar(position = pos)

  expect_true(inherits(p@layers[[1]]$position, "PositionDodge"))
  expect_true(inherits(p@layers[[2]]$position, "PositionDodge"))
})

test_that("geom_pointbar() na.rm = TRUE suppresses missing-value warnings", {
  d <- data.frame(
    x = 1:5,
    y = c(2.5, NA, 4.1, 3.8, NA),
    ymin = c(2.0, NA, 3.5, 3.2, NA),
    ymax = c(3.0, NA, 4.7, 4.4, NA)
  )

  p <- ggplot(d, ggplot2::aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(na.rm = TRUE)

  # If na.rm is correctly passed through, building should not warn about removed rows
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("geom_pointbar() preserves missingness in built data", {
  d <- data.frame(
    x = 1:5,
    y = c(2.5, NA, 4.1, 3.8, NA),
    ymin = c(2.0, NA, 3.5, 3.2, NA),
    ymax = c(3.0, NA, 4.7, 4.4, NA)
  )

  p <- ggplot(d, ggplot2::aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(na.rm = TRUE)

  b <- ggplot_build(p)

  # Errorbar layer: missing y/ymin/ymax should still be missing in those rows
  expect_equal(sum(!is.finite(b$data[[1]]$y)), 2)
  expect_equal(sum(!is.finite(b$data[[2]]$y)), 2)
})


test_that("geom_pointbar() works with manual colour scales", {
  d <- data.frame(
    x = factor(c("A", "B")),
    y = c(1, 2),
    ymin = c(0.7, 1.6),
    ymax = c(1.3, 2.4),
    g = factor(c("G1", "G2"))
  )

  p <- ggplot(d, aes(x, y, ymin = ymin, ymax = ymax, colour = g)) +
    geom_pointbar() +
    scale_colour_manual(values = c(G1 = "red", G2 = "blue"))

  expect_no_error(b <- ggplot_build(p))
  expect_false(is.null(b$plot$scales$get_scales("colour")))
})

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

test_that("geom_pointbar visual test", {
  skip_on_os(c("mac", "windows"))

  set.seed(1)

  d <- data.frame(
    term = factor(rep(c("A", "B", "C", "D"), each = 2), levels = c("A", "B", "C", "D")),
    group = factor(rep(c("Control", "Treat"), times = 4), levels = c("Control", "Treat")),
    estimate = c(0.10, 0.25, -0.05, 0.15, 0.00, 0.20, 0.30, NA),  # includes NA
    se = c(0.05, 0.07, 0.06, 0.05, 0.04, 0.06, 0.08, 0.09),
    panel = factor(rep(c("P1", "P2"), each = 4), levels = c("P1", "P2"))
  )

  d$ymin <- d$estimate - 1.96 * d$se
  d$ymax <- d$estimate + 1.96 * d$se

  pos <- position_dodge(width = 0.55)

  p <- ggplot(
    d,
    aes(
      x = term,
      y = estimate,
      ymin = ymin,
      ymax = ymax,
      colour = group,
      fill = group
    )
  ) +
    geom_pointbar(
      position = pos,
      na.rm = TRUE,              # exercises missing handling
      point.size = 3.5,          # point styling
      shape = 21,                # enables fill + colour
      alpha = 0.85,              # applied to both geoms
      errorbar.width = 0.25,     # errorbar styling
      errorbar.linewidth = 1.0,
      linetype = "dashed"
    ) +
    geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.5) +
    facet_wrap(~ panel, nrow = 1) +
    scale_colour_manual(values = c("Control" = e61_skydark, "Treat" = e61_orangedark)) +
    scale_fill_manual(values = c("Control" = e61_skylight, "Treat" = e61_orangelight)) +
    theme_e61(legend = "top", legend_title = TRUE) +
    labs_e61(
      title = "geom_pointbar kitchen sink",
      subtitle = "dodge + manual colour/fill + shape21 + linetype/alpha + na.rm + facets",
      y = "Estimate"
    )

  # Snapshot: keep as SVG so diffs are stable and file size stays smaller than PNG.
  withr::with_tempdir({
    suppressWarnings(save_e61("geom-pointbar-kitchen-sink.svg", p, spell_check = FALSE))
    expect_snapshot_file("geom-pointbar-kitchen-sink.svg")
  })
})
