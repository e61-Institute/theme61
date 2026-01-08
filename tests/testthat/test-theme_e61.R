test_that("square_legend_symbols is working", {
  # Points
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, colour = 1:3),
           aes(x, y, colour = colour)) +
    geom_point() +
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("square-legend-point.svg", p)))
  })

  # Lines
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, colour = 1:3),
           aes(x, y, colour = colour)) +
    geom_line() +
    geom_point(alpha = 0) + # The required "invisible points"
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("square-legend-line.svg", p)))
  })

  # Pointrange (testing ymin/ymax)
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, ymin = 0:2, ymax = 2:4, colour = 1:3),
           aes(x, y, ymin = ymin, ymax = ymax, colour = colour)) +
    geom_pointrange() +
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols(size = 1.2)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("square-legend-pointrange.svg", p)))
  })

  # Column
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, colour = 1:3),
           aes(x, y, fill = factor(colour))) +
    geom_col() +
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("square-legend-col.svg", p)))
  })

})

test_that("Legend position can be adjusted", {
  p <-
    ggplot(data.frame(x = 1:3, y = 1:3, colour = 1:3),
           aes(x, y, colour = colour)) +
    geom_point()

  p1 <- p +
    theme_e61(legend = "top")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("legend-chk-1.svg", p1)))
  })

  p1 <- p +
    theme_e61(legend = "bottom")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("legend-chk-2.svg", p1)))
  })

  expect_error({p + theme_e61(legend = "inside")})
  expect_error({p + theme_e61(legend = "inside", legend_position = c(20, 245))})
  expect_error({p + theme_e61(legend = "inside", legend_position = 20)})
  expect_error({p + theme_e61(legend = "inside", legend_position = c("a", "b"))})

  p1 <- p +
    theme_e61(legend = "inside", legend_position = c(0.9, 0.85))

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("legend-chk-3.svg", p1)))
  })

})

test_that("Aspect ratio can be changed", {
  p <-
    ggplot(data.frame(x = 1:10, y = (1:10)^2),
           aes(x, y)) +
    geom_point() +
    theme_e61()

  p1 <- p +
    theme_e61(aspect_ratio = 1)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("aspect-ratio-1.svg", p1, chart_type = "custom")))
  })

  p2 <- p +
    theme_e61(aspect_ratio = 0.5)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("aspect-ratio-5.svg", p2, chart_type = "custom")))
  })

  p3 <- p +
    theme_e61(aspect_ratio = 3)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("aspect-ratio-3.svg", p3, chart_type = "custom")))
  })

})

test_that("Spatial plots preserve the original aspect ratios", {

  skip_if_not_installed("strayr")
  skip_if_not_installed("sf")

  sa4 <- strayr::read_absmap("sa42021")

  bbox <- sf::st_bbox(c(xmin = 145, xmax = 155, ymin = -35, ymax = -30),
                      crs = sf::st_crs(4326))
  sa4_syd <- sf::st_crop(sa4, bbox)

  plotdata <- rbind(
    transform(sa4_syd, property_type = "House"),
    transform(sa4_syd, property_type = "Unit")
  )

  p1 <- ggplot(plotdata) +
      geom_sf(fill = "grey90", colour = "grey40", linewidth = 0.1) +
      coord_sf(xlim = c(150.95, 151.30), ylim = c(-34.05, -33.70)) +
      facet_wrap(~ property_type) +
      theme_e61_spatial() +
      labs_e61(
        title = "MWE: faceted ABS SA4 map (strayr::read_absmaps)",
        footnotes = "Constant fill; geometry from ABS via strayr."
      )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("map-aspect-ratio-1.svg", p1)))
  })

  # See if it preserves weirdly wide graphs
  p2 <- p1 +
    coord_sf(xlim = c(150.5, 151.30), ylim = c(-34.05, -33.70))

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("map-aspect-ratio-2.svg", p2)))
  })

})
