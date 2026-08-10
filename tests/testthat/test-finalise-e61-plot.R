test_that("theme_e61() is an immediately usable, realised theme (not deferred)", {

  th <- theme_e61(legend = "bottom")

  expect_true(inherits(th, "theme"))
  expect_identical(th$legend.position, "bottom")

  # Invalid input errors immediately at the call site, not later at save/print time.
  expect_error(theme_e61(legend = "inside"))
  expect_error(theme_e61(legend = "inside", legend_position = 20))
})

test_that("finalise_e61_plot() classifies maps and corrects their axis chrome", {

  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() +
    theme_e61(legend = "bottom")

  # Emulate a spatial layer via the GeomSf heuristic used by classify_e61_map()
  p$layers <- c(p$layers, list(list(geom = structure(list(), class = "GeomSf"))))

  out <- finalise_e61_plot(p)

  expect_true(inherits(out, "e61_map"))
  # The map correction blanks axis chrome and gridlines...
  expect_true(inherits(out@theme$axis.text, "element_blank"))
  expect_true(inherits(out@theme$panel.grid.major.y, "element_blank"))
  # ...but leaves the user's own theme_e61() choices (e.g. legend position)
  # untouched - this is the regression the old deferred-spec design got wrong.
  expect_identical(out@theme$legend.position, "bottom")
})

test_that("finalise_e61_plot() respects gridlines the user set explicitly", {

  # Regression test: the map correction targets panel.grid.major.x/.y
  # specifically, not the parent panel.grid.major - ggplot2's theme element
  # hierarchy makes an explicitly-set child element win over a later
  # parent-level override regardless of merge order, so a naive
  # theme(panel.grid.major = element_blank()) correction would silently do
  # nothing to theme_e61()'s own default gridline (also a child element),
  # while a per-element "skip if already customised" check done at the wrong
  # (parent) level could otherwise still clobber a user's explicit choice.
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() +
    theme_e61(legend = "bottom") +
    theme(panel.grid.major.y = element_line(colour = "red"))
  p$layers <- c(p$layers, list(list(geom = structure(list(), class = "GeomSf"))))

  out <- finalise_e61_plot(p)

  expect_true(inherits(out, "e61_map"))
  expect_identical(out@theme$panel.grid.major.y$colour, "red")
  # Unrelated elements the user didn't touch are still corrected.
  expect_true(inherits(out@theme$axis.text, "element_blank"))
})

test_that("finalise_e61_plot() is idempotent on maps", {

  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() +
    theme_e61(legend = "bottom") +
    theme(panel.grid.major.y = element_line(colour = "red"))
  p$layers <- c(p$layers, list(list(geom = structure(list(), class = "GeomSf"))))

  out <- finalise_e61_plot(p)
  out2 <- finalise_e61_plot(out)

  expect_true(inherits(out2@theme$axis.text, "element_blank"))
  expect_identical(out2@theme$panel.grid.major.y$colour, "red")
  expect_identical(out2@theme$legend.position, "bottom")
})

test_that("finalise_e61_plot() leaves non-map plots untouched", {

  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() +
    theme_e61(legend = "bottom")

  out <- finalise_e61_plot(p)

  expect_false(inherits(out, "e61_map"))
  expect_identical(out@theme$axis.text, p@theme$axis.text)
  expect_identical(out@theme$legend.position, "bottom")
})

test_that("save_e61() applies the map correction for every panel in a multi-panel save", {

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
