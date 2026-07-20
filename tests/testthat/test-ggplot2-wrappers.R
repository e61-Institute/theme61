test_that("ggplot2 functions are masked by theme61", {
  withr::local_options(list(quiet_wrap = FALSE))

  # Check if labs() throws a msg
  suppressWarnings(expect_message(save_e61(withr::local_tempfile(fileext = ".svg"), minimal_plot + labs()), "Your function.*"))

  # Check if ggsave() throws a msg
  suppressWarnings(expect_message(ggsave(withr::local_tempfile(fileext = ".svg"), minimal_plot), "Your function.*"))
})

test_that("Automatic secondary y-axis scales work", {

  # Use a dataset with clearly different ranges so axis behaviour is visible
  data <- data.frame(
    v1 = 1:10,
    v2 = seq(10, 100, length.out = 10),
    v3 = seq(200, 400, length.out = 10)
  )

  # 1) mappings in ggplot() aes, simple geom_point
  p1 <- ggplot(data, aes(x = v1, y = v2)) +
    geom_point()

  # 2) data in ggplot(), mappings in geom_point()
  p2 <- ggplot(data) +
    geom_point(aes(x = v1, y = v2))

  # 3) data and mappings both in geom_point()
  p3 <- ggplot() +
    geom_point(data = data, aes(x = v1, y = v2))

  # 4) two geoms with two different y mappings
  #    (ensure we still get the default y scale behaviour, and plot renders cleanly)
  p4 <- ggplot(data) +
    geom_point(aes(x = v1, y = v2)) +
    geom_point(aes(x = v1, y = v3), shape = 1)

  # 5) transformed mapping in plot-level aes (previously broken: y = v2 + v1)
  p5 <- ggplot(data, aes(x = v1, y = v2 + v1)) +
    geom_point()

  withr::with_tempdir({
    expect_snapshot_file(
      suppressWarnings(save_e61("auto-sec-axis-1-mapping-in-ggplot.svg", p1))
    )
    expect_snapshot_file(
      suppressWarnings(save_e61("auto-sec-axis-2-mapping-in-geom.svg", p2))
    )
    expect_snapshot_file(
      suppressWarnings(save_e61("auto-sec-axis-3-data-and-mapping-in-geom.svg", p3))
    )
    expect_snapshot_file(
      suppressWarnings(save_e61("auto-sec-axis-4-two-y-series.svg", p4))
    )
    expect_snapshot_file(
      suppressWarnings(save_e61("auto-sec-axis-5-transformed-y.svg", p5))
    )
  })
})

test_that("Auto y-axis functionality does not apply if you override with ggplot2 scale functions", {

  data <- data.frame(
    v1 = 1:10,
    v2 = seq(10, 100, length.out = 10)
  )

  # Baseline: should include duplicated secondary axis
  p_auto <- ggplot(data) +
    geom_point(aes(x = v1, y = v2))

  # Override with ggplot2 scale: should *not* show duplicated secondary axis
  # (and should not have theme61 forcibly re-adding scale_y_continuous_e61)
  p_override <- ggplot(data) +
    geom_point(aes(x = v1, y = v2)) +
    scale_y_continuous(
      breaks = c(0, 50, 100),
      limits = c(0, 110)
    )

  # Also test an override added *before* the geom
  p_override2 <- ggplot(data) +
    scale_y_continuous(
      breaks = c(0, 50, 100),
      limits = c(0, 110)
    ) +
    geom_point(aes(x = v1, y = v2))

  withr::with_tempdir({
    expect_snapshot_file(
      suppressWarnings(save_e61("override-y-scale-1-auto.svg", p_auto))
    )
    expect_snapshot_file(
      suppressWarnings(save_e61("override-y-scale-2-ggplot2-scale-after.svg", p_override))
    )
    expect_snapshot_file(
      suppressWarnings(save_e61("override-y-scale-3-ggplot2-scale-before.svg", p_override2))
    )
  })
})

test_that("User-supplied colour/fill scales are not overridden by defaults", {

  n <- 20L

  df <- data.frame(
    x = rep(1:10, length.out = n),
    y = seq_len(n),
    grp = factor(paste0("g", seq_len(n)))
  )

  # ---- Colour override
  # Make a manual colour scale where every level maps to black.
  # If theme61 overrides it, colours will not all be black.
  manual_cols <- stats::setNames(rep("black", n), levels(df$grp))

  p_col_user <- ggplot(df, aes(x, y, colour = grp)) +
    geom_point() +
    scale_colour_manual(values = manual_cols)

  expect_no_error(b_col_user <- ggplot_build(p_col_user))
  expect_equal(unique(b_col_user$data[[1]]$colour), "black")

  # ---- Fill override
  manual_fills <- stats::setNames(rep("black", n), levels(df$grp))

  df2 <- data.frame(
    x = df$grp,
    y = 1,
    grp = df$grp
  )

  p_fill_user <- ggplot(df2, aes(x, y, fill = grp)) +
    geom_col() +
    scale_fill_manual(values = manual_fills)

  expect_no_error(b_fill_user <- ggplot_build(p_fill_user))
  expect_equal(unique(b_fill_user$data[[1]]$fill), "black")

})

test_that("Error when discrete colour mapping exceeds supported palette size", {

  n <- 20L  # deliberately > 12

  df <- data.frame(
    x = rep(1:10, length.out = n),
    y = seq_len(n),
    grp = factor(paste0("g", seq_len(n)))
  )

  p <- ggplot(df, aes(x, y, colour = grp)) +
    geom_point()

  expect_error(
    ggplot_build(p),
    regexp = "theme61.*support.*12|more than 12 colours",
    fixed = FALSE
  )

})

test_that("A column named 'source' mapped to colour does not resolve to base::source()", {

  df <- data.frame(
    x = 1:10,
    y = 1:10,
    source = factor(rep(c("Source A", "Source B"), each = 5))
  )

  p <- ggplot(df, ggplot2::aes(x, y, colour = source)) +
    geom_line()

  # This is the regression: infer logic accidentally evaluates `source` as base::source (a closure)
  # and then crashes when it tries to treat it like a vector.
  expect_no_error(b <- ggplot_build(p))

  # Sanity: should map two discrete groups -> two colours in built data
  expect_gte(length(unique(b$data[[1]]$colour)), 2L)
})


test_that("Inference works when plot-level data is NULL and mapping comes from layer data", {

  df_layer <- data.frame(
    x = 1:10,
    y = 1:10,
    source = factor(rep(c("Source A", "Source B"), each = 5))
  )

  # Plot has no data; layer supplies data + mapping
  p <- ggplot() +
    geom_line(
      data = df_layer,
      mapping = aes(x, y, colour = source)
    )

  expect_no_error(b <- ggplot_build(p))
  expect_gte(length(unique(b$data[[1]]$colour)), 2L)
})

test_that("facet spacing depends on theme61 facet axes setting", {

  old <- options(quiet_wrap = TRUE)
  on.exit(options(old), add = TRUE)

  df <- data.frame(
    gcc = rep(c("A", "B", "C", "D"), each = 5),
    x = rep(letters[1:5], times = 4),
    y = rnorm(20)
  )

  # axes = "margins" -> tight (0.5 lines)
  p_margins <-
    ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~gcc, axes = "margins")

  b_margins <- ggplot_build(p_margins)
  th_margins <- b_margins@plot@theme

  expect_equal(th_margins$panel.spacing.x, grid::unit(0.5, "lines"))
  expect_equal(th_margins$panel.spacing.y, grid::unit(0.5, "lines"))

  # axes = "all" -> roomy (2 lines)
  p_all <-
    ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~gcc, axes = "all")

  b_all <- ggplot_build(p_all)
  th_all <- b_all@plot@theme

  expect_equal(th_all$panel.spacing.x, grid::unit(2, "lines"))
  expect_equal(th_all$panel.spacing.y, grid::unit(2, "lines"))
})

test_that("user-specified panel.spacing is not overridden", {

  old <- options(quiet_wrap = TRUE)
  on.exit(options(old), add = TRUE)

  df <- data.frame(
    gcc = rep(c("A", "B"), each = 5),
    x = rep(letters[1:5], times = 2),
    y = rnorm(10)
  )

  user_spacing_x <- grid::unit(9, "mm")
  user_spacing_y <- grid::unit(7, "mm")

  p <-
    ggplot(df, ggplot2::aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~gcc, axes = "margins") +
    theme(
      panel.spacing.x = user_spacing_x,
      panel.spacing.y = user_spacing_y
    )

  b <- ggplot_build(p)
  th <- b@plot@theme

  expect_equal(th$panel.spacing.x, user_spacing_x)
  expect_equal(th$panel.spacing.y, user_spacing_y)
})

test_that("categorical y-axis text is left-aligned by default (#298)", {

  df <- data.frame(
    category = c("Short", "A much longer category label"),
    value = c(1, 2)
  )

  p <- ggplot(df, aes(x = value, y = category)) +
    geom_col()

  b <- ggplot_build(p)
  th <- b@plot@theme

  expect_equal(th$axis.text.y$hjust, 0)
  expect_equal(th$axis.text.y.right$hjust, 0)
})

test_that("continuous y-axis text alignment is untouched (#298)", {

  df <- data.frame(x = 1:5, y = (1:5)^2)

  p <- ggplot(df, aes(x, y)) +
    geom_point()

  b <- ggplot_build(p)
  th <- b@plot@theme

  expect_null(th$axis.text.y$hjust)
  expect_null(th$axis.text.y.right$hjust)
})

test_that("user-specified y-axis text alignment is not overridden (#298)", {

  df <- data.frame(
    category = c("Short", "A much longer category label"),
    value = c(1, 2)
  )

  p <- ggplot(df, aes(x = value, y = category)) +
    geom_col() +
    theme(axis.text.y = element_text(hjust = 1))

  b <- ggplot_build(p)
  th <- b@plot@theme

  expect_equal(th$axis.text.y$hjust, 1)
})

testthat::test_that("ggplot2::facet_wrap is not auto-adjusted (facet not tagged)", {

  old <- options(quiet_wrap = TRUE)
  on.exit(options(old), add = TRUE)

  df <- data.frame(
    gcc = rep(c("A", "B", "C"), each = 5),
    x = rep(letters[1:5], times = 3),
    y = rnorm(15)
  )

  # Bypass theme61 wrapper: facet is untagged, so spacing should NOT be injected.
  p <-
    ggplot(df, ggplot2::aes(x = x, y = y)) +
    geom_point() +
    ggplot2::facet_wrap(~gcc)

  b <- ggplot_build(p)
  th <- b@plot@theme

  # Don't assume ggplot2 defaults; just assert we didn't inject our canonical values.
  expect_false(identical(th$panel.spacing.x, grid::unit(2, "lines")))
  expect_false(identical(th$panel.spacing.y, grid::unit(2, "lines")))
  expect_false(identical(th$panel.spacing.x, grid::unit(0.5, "lines")))
  expect_false(identical(th$panel.spacing.y, grid::unit(0.5, "lines")))
})

test_that("theme61::facet_wrap tags facet with t61_axes", {

  df <- data.frame(gcc = rep(c("A", "B"), each = 2), x = 1:4, y = 1:4)

  p <-
    ggplot(df, ggplot2::aes(x, y)) +
    geom_point() +
    facet_wrap(~gcc, axes = "margins")

  expect_equal(attr(p@facet, "t61_axes", exact = TRUE), "margins")
})
