test_that("square_legend_symbols sets guide override.aes correctly", {

  p <- ggplot(
    data.frame(x = 1:3, y = 1:3, grp = factor(1:3)),
    aes(x, y, colour = grp)
  ) +
    geom_point() +
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols(size = 1.2)

  g <- p@guides$guides$colour
  expect_s3_class(g, "GuideLegend")
  expect_equal(g$params$override.aes$shape, 15)
  expect_equal(g$params$override.aes$alpha, 1)
  expect_equal(g$params$override.aes$size, 1.2)

  p2 <- ggplot(
    data.frame(x = 1:3, y = 1:3, grp = factor(1:3)),
    aes(x, y, fill = grp)
  ) +
    geom_col() +
    theme_e61(legend = "top", legend_title = TRUE) +
    square_legend_symbols()

  g2 <- p2@guides$guides$fill
  expect_s3_class(g2, "GuideLegend")
  expect_equal(g2$params$override.aes$shape, 15)
  expect_equal(g2$params$override.aes$size, 6)
})

test_that("Legend position can be adjusted", {

  p <- ggplot(
    data.frame(x = 1:3, y = 1:3, colour = 1:3),
    aes(x, y, colour = colour)
  ) +
    geom_point()

  p_top <- p + theme_e61(legend = "top")
  expect_equal(p_top@theme$legend.position, "top")

  p_bottom <- p + theme_e61(legend = "bottom")
  expect_equal(p_bottom@theme$legend.position, "bottom")

  expect_error({ p + theme_e61(legend = "inside") })
  expect_error({ p + theme_e61(legend = "inside", legend_position = c(20, 245)) })
  expect_error({ p + theme_e61(legend = "inside", legend_position = 20) })
  expect_error({ p + theme_e61(legend = "inside", legend_position = c("a", "b")) })

  p_inside <- p + theme_e61(legend = "inside", legend_position = c(0.9, 0.85))
  expect_equal(p_inside@theme$legend.position, "inside")
  expect_equal(p_inside@theme$legend.position.inside, c(0.9, 0.85))
})

test_that("Aspect ratio can be changed", {

  p <- ggplot(
    data.frame(x = 1:10, y = (1:10)^2),
    aes(x, y)
  ) +
    geom_point() +
    theme_e61()

  p1 <- p + theme_e61(aspect_ratio = 1)
  expect_equal(p1@theme$aspect.ratio, 1)

  p2 <- p + theme_e61(aspect_ratio = 0.5)
  expect_equal(p2@theme$aspect.ratio, 0.5)

  p3 <- p + theme_e61(aspect_ratio = 3)
  expect_equal(p3@theme$aspect.ratio, 3)
})

test_that("Spatial plots preserve aspect ratios", {

  skip_if_not_installed("sf")

  # A single rectangle spanning both windows tested below (inland VIC-sized
  # extent) is enough to exercise coord_sf()'s aspect calculation - it only
  # depends on the requested xlim/ylim and CRS, not the drawn geometry, so
  # there's no need for real ABS boundaries here.
  sa4_vic <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(142.0, -38.0,
          145.0, -38.0,
          145.0, -37.0,
          142.0, -37.0,
          142.0, -38.0),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  plotdata <- rbind(
    transform(sa4_vic, property_type = "House"),
    transform(sa4_vic, property_type = "Unit")
  )

  p_base <- ggplot(plotdata) +
    geom_sf(fill = "grey90", colour = "grey40", linewidth = 0.1) +
    facet_wrap(~ property_type)

  # inland VIC windows (avoid ocean cropping)
  xlim1 <- c(143.55, 144.15)
  ylim1 <- c(-37.65, -37.15)
  p1 <- p_base + coord_sf(xlim = xlim1, ylim = ylim1)

  # wider window, same height -> aspect should increase
  xlim2 <- c(142.95, 144.15)
  ylim2 <- c(-37.65, -37.15)
  p2 <- p_base + coord_sf(xlim = xlim2, ylim = ylim2)

  get_coord_sf_aspect <- function(p) {
    b <- ggplot_build(p)

    coord <- b@layout$coord          # ggproto (use $ below)
    pp <- b@layout$panel_params[[1]] # list of panel params (S7 build object uses @)

    a <- coord$aspect(pp)

    if (!is.numeric(a) || length(a) != 1L || !is.finite(a)) NA_real_ else a
  }

  a1 <- get_coord_sf_aspect(p1)
  a2 <- get_coord_sf_aspect(p2)

  expect_true(is.finite(a1))
  expect_true(is.finite(a2))
  expect_gt(a1, a2)
})
