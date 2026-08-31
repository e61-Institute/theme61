test_that("ggplot2 functions are masked by theme61", {
  withr::local_options(list(quiet_mask = FALSE))

  # Check if labs() throws a msg
  suppressWarnings(expect_message(save_e61(withr::local_tempfile(fileext = ".svg"), minimal_plot + labs()), "Your function.*"))

  # Check if ggsave() throws a msg
  suppressWarnings(expect_message(ggsave(withr::local_tempfile(fileext = ".svg"), minimal_plot), "Your function.*"))
})

test_that("Automatic secondary y-axis scales work", {

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

  # 4) two geoms with two different y mappings (should still render cleanly)
  p4 <- ggplot(data) +
    geom_point(aes(x = v1, y = v2)) +
    geom_point(aes(x = v1, y = v3), shape = 1)

  # 5) transformed mapping in plot-level aes (previously broken: y = v2 + v1)
  p5 <- ggplot(data, aes(x = v1, y = v2 + v1)) +
    geom_point()

  plots <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5)

  for (nm in names(plots)) {
    expect_no_error(b <- ggplot_build(plots[[nm]]))

    # theme61 should have added a y scale when none is present
    ysc <- b@plot@scales$get_scales("y")
    expect_false(is.null(ysc), info = paste("Missing y scale for", nm))

    # default behaviour should include a duplicated secondary axis (not waiver())
    expect_false(inherits(ysc$secondary.axis, "waiver") || is.null(ysc$secondary.axis),
                 info = paste("Expected secondary axis for", nm))

    # and it should actually manifest as a right axis grob with non-zero width
    g <- quiet_ggplotGrob(b@plot)
    axis_r_w <- theme61:::get_grob_width(g, grob_name = "axis-r")
    expect_false(is.null(axis_r_w) || axis_r_w == 0, info = paste("No right axis grob for", nm))
  }
})

test_that("Auto y-axis does not apply if you override with ggplot2 scale functions", {

  data <- data.frame(
    v1 = 1:10,
    v2 = seq(10, 100, length.out = 10)
  )

  # Baseline: should include duplicated secondary axis
  p_auto <- ggplot(data) +
    geom_point(aes(x = v1, y = v2))

  # Override with ggplot2 scale: should not show duplicated secondary axis
  p_override <- ggplot(data) +
    geom_point(aes(x = v1, y = v2)) +
    scale_y_continuous(
      breaks = c(0, 50, 100),
      limits = c(0, 110)
    )

  # Also test an override added before the geom
  p_override2 <- ggplot(data) +
    scale_y_continuous(
      breaks = c(0, 50, 100),
      limits = c(0, 110)
    ) +
    geom_point(aes(x = v1, y = v2))

  # ---- Baseline: theme61 should inject its y scale with a dup secondary axis
  expect_no_error(b_auto <- ggplot_build(p_auto))
  y_auto <- b_auto@plot@scales$get_scales("y")
  expect_false(is.null(y_auto))
  expect_false(inherits(y_auto$secondary.axis, "waiver") || is.null(y_auto$secondary.axis))

  g_auto <- quiet_ggplotGrob(b_auto@plot)
  axis_r_w_auto <- theme61:::get_grob_width(g_auto, grob_name = "axis-r")
  expect_false(is.null(axis_r_w_auto) || axis_r_w_auto == 0)

  # ---- Overrides: should keep the user ggplot2 scale (secondary axis should be waiver / absent)
  for (p in list(p_override, p_override2)) {
    expect_no_error(b <- ggplot_build(p))
    ysc <- b@plot@scales$get_scales("y")
    expect_false(is.null(ysc))

    # ggplot2::scale_y_continuous default secondary axis is waiver()
    expect_true(inherits(ysc$secondary.axis, "waiver") || is.null(ysc$secondary.axis))

    g <- quiet_ggplotGrob(b@plot)
    axis_r_w <- theme61:::get_grob_width(g, grob_name = "axis-r")
    expect_true(is.null(axis_r_w) || axis_r_w == 0)
  }
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

  old <- options(quiet_mask = TRUE)
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

  old <- options(quiet_mask = TRUE)
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

test_that("continuous y-axis text alignment is untouched", {
  df <- data.frame(x = 1:5, y = (1:5)^2)
  p <- ggplot(df, aes(x, y)) + geom_point()

  built <- maybe_add_default_scales(p)
  built <- maybe_adjust_facet_spacing(built)

  expect_identical(maybe_leftalign_discrete_y_text(built), built)
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

  old <- options(quiet_mask = TRUE)
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

test_that("quiet_mask suppresses the ggsave()/labs() masking messages", {
  withr::local_options(list(quiet_mask = TRUE))

  expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), minimal_plot + labs()))
  expect_no_message(ggsave(withr::local_tempfile(fileext = ".svg"), minimal_plot))
})

test_that("theme61.iterate_mode makes ggsave() pass through to ggplot2::ggsave()", {
  withr::local_options(list(theme61.iterate_mode = TRUE, quiet_mask = FALSE))

  # No masking message, even though quiet_mask is off. Explicit width/height
  # avoids ggplot2::ggsave()'s own unrelated "Saving WxH in image" message.
  expect_no_message(
    ggsave(withr::local_tempfile(fileext = ".png"), minimal_plot, width = 5, height = 5),
    message = "Your function.*"
  )
})

test_that("theme61.iterate_mode makes labs() pass through to ggplot2::labs()", {
  withr::local_options(list(theme61.iterate_mode = TRUE, quiet_mask = FALSE))

  # No masking message, even though quiet_mask is off
  expect_no_message(l <- labs(title = "A title", x = "x-axis"))

  # Identical output to calling ggplot2::labs() directly - no labs_e61()
  # formatting/wrapping applied
  expect_identical(l, ggplot2::labs(title = "A title", x = "x-axis"))
})

test_that("theme61.iterate_mode makes facet_wrap()/facet_grid() pass through to ggplot2 defaults", {
  withr::local_options(list(theme61.iterate_mode = TRUE))

  df <- data.frame(gcc = rep(c("A", "B"), each = 2), x = 1:4, y = 1:4)

  # No axes supplied: should use ggplot2's own default (margins), not
  # theme61's "all" default, and should not be tagged with t61_axes
  p_wrap <- ggplot(df, ggplot2::aes(x, y)) + geom_point() + facet_wrap(~gcc)
  expect_null(attr(p_wrap@facet, "t61_axes", exact = TRUE))

  p_grid <- ggplot(df, ggplot2::aes(x, y)) + geom_point() + facet_grid(~gcc)
  expect_null(attr(p_grid@facet, "t61_axes", exact = TRUE))

  # Explicit axes is still honoured, just without the t61_axes tag
  p_explicit <- ggplot(df, ggplot2::aes(x, y)) + geom_point() + facet_wrap(~gcc, axes = "all")
  expect_null(attr(p_explicit@facet, "t61_axes", exact = TRUE))
})

test_that("set_t61_options rejects the unnamespaced quiet_mask option", {
  expect_error(set_t61_options(list(quiet_mask = TRUE)), "Invalid options supplied")
})

# ---- Upstream ggplot2 signature-drift safety net (issue #336) ------------
# Catches a future ggplot2 release silently breaking the masks by
# renaming/removing an argument they rely on by name (happened once, on
# ggplot2 4.0).

test_that("ggplot2::ggplot()'s formals still cover what theme61::ggplot() forwards by name", {

  t61_named_args <- c("data", "mapping", "environment")
  upstream_formals <- names(formals(ggplot2::ggplot))

  # theme61::ggplot() forwards these by name; if ggplot2 drops/renames one,
  # it needs updating.
  expect_true(
    all(t61_named_args %in% upstream_formals),
    info = paste0(
      "ggplot2::ggplot() no longer has formal(s): ",
      paste(setdiff(t61_named_args, upstream_formals), collapse = ", ")
    )
  )

  expect_identical(
    names(formals(theme61::ggplot)),
    c("data", "mapping", "...", "environment")
  )

  # Informational only: new upstream args are still absorbed by `...`.
  extra_upstream_args <- setdiff(upstream_formals, c(t61_named_args, "..."))
  if (length(extra_upstream_args) > 0) {
    warning(
      "ggplot2::ggplot() has gained new formal(s): ",
      paste(extra_upstream_args, collapse = ", ")
    )
  }
})

test_that("ggplot2::facet_wrap()/facet_grid() still have an 'axes' formal", {
  # theme61's wrappers default `axes` to "all"; if upstream ever
  # renamed/removed it, the wrapper would silently stop working.
  expect_true("axes" %in% names(formals(ggplot2::facet_wrap)))
  expect_true("axes" %in% names(formals(ggplot2::facet_grid)))
})

test_that("ggplot2::labs() and ggplot2::ggsave() still exist as functions", {
  # theme61's versions take pure ... and forward elsewhere, but
  # theme61.iterate_mode calls these directly.
  expect_true(is.function(ggplot2::labs))
  expect_true(is.function(ggplot2::ggsave))
})

# ---- Behavioural smoke test for the e61_plot / iterate_mode contract -----

test_that("theme61::ggplot() tags plots as e61_plot, and iterate_mode bypasses the automatic e61 additions at build time", {

  df <- data.frame(x = 1:10, y = seq(10, 100, length.out = 10))

  # Normal mode: tagged as e61_plot, and building it injects a secondary y-axis
  p <- ggplot(df, aes(x, y)) + geom_point()
  expect_true(inherits(p, "e61_plot"))

  b <- ggplot_build(p)
  ysc <- b@plot@scales$get_scales("y")
  expect_false(is.null(ysc))
  expect_false(inherits(ysc$secondary.axis, "waiver") || is.null(ysc$secondary.axis))

  # iterate_mode: still tagged e61_plot, but ggplot_build.e61_plot() strips
  # the tag and skips automatic styling before building
  withr::local_options(list(theme61.iterate_mode = TRUE))

  p_iter <- ggplot(df, aes(x, y)) + geom_point()
  expect_true(inherits(p_iter, "e61_plot"))

  b_iter <- ggplot_build(p_iter)
  expect_false(inherits(b_iter@plot, "e61_plot"))

  ysc_iter <- b_iter@plot@scales$get_scales("y")
  expect_true(is.null(ysc_iter) ||
                inherits(ysc_iter$secondary.axis, "waiver") ||
                is.null(ysc_iter$secondary.axis))
})
