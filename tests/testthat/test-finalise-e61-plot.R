test_that("finalise_e61_plot() realises a plot's theme_e61() spec into a real theme", {

  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() +
    theme_e61(legend = "bottom")

  # Before realisation the spec is just stashed as an attribute - the
  # underlying ggplot2 theme hasn't actually been touched yet.
  expect_false(is.null(attr(p, "e61_theme_spec", exact = TRUE)))
  expect_null(p@theme$legend.position)

  out <- finalise_e61_plot(p)

  expect_true(inherits(out, "e61_plot"))
  expect_identical(out@theme$legend.position, "bottom")

  # Spec attribute is cleared once realised, so re-running is a no-op
  expect_null(attr(out, "e61_theme_spec", exact = TRUE))
  out2 <- finalise_e61_plot(out)
  expect_identical(out2@theme$legend.position, "bottom")
})

test_that("finalise_e61_plot() classifies maps before realising, so the spatial theme is used", {

  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() +
    theme_e61(legend = "bottom")

  # Emulate a spatial layer via the GeomSf heuristic used by classify_e61_map()
  p$layers <- c(p$layers, list(list(geom = structure(list(), class = "GeomSf"))))

  out <- finalise_e61_plot(p)

  expect_true(inherits(out, "e61_map"))
  # theme_e61_spatial()'s legend.direction rule differs from the non-map theme
  # for "bottom"/"top", but the legend position itself should still come
  # through from the spec.
  expect_identical(out@theme$legend.position, "bottom")
})

test_that("save_e61() realises the theme for every panel in a multi-panel save, not just single panels", {

  # This is a regression test: previously theme_e61() was only realised for
  # the length(plots) == 1 branch of save_e61(), so multi-panel saves quietly
  # kept ggplot2's default theme (e.g. legend shown on the right) instead of
  # the theme_e61() spec attached to each panel.
  p1 <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point() +
    theme_e61(legend = "bottom")

  p2 <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_line() +
    theme_e61(legend = "bottom")

  plots <- lapply(list(p1, p2), finalise_e61_plot)

  expect_true(all(vapply(
    plots, function(pl) identical(pl@theme$legend.position, "bottom"), logical(1)
  )))

  withr::with_tempdir({
    expect_no_error(suppressWarnings(save_e61("multi-theme-realise.svg", p1, p2)))
  })
})
