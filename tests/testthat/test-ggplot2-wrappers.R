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

test_that("Auto colour/fill scales handle many discrete levels (>12) without error", {

  n <- 20L

  df <- data.frame(
    x = rep(1:10, length.out = n),
    y = seq_len(n),
    grp = factor(paste0("g", seq_len(n)))
  )

  # Colour (line)
  p_col <- ggplot(df, aes(x, y, colour = grp)) +
    geom_line()

  expect_no_error(b_col <- ggplot_build(p_col))
  # For a line plot, colour is recorded in built layer data
  expect_gte(length(unique(b_col$data[[1]]$colour)), n)

  # Fill (bars)
  # Use geom_col so fill is clearly mapped and appears in built data.
  df2 <- data.frame(
    x = df$grp,
    y = 1,
    grp = df$grp
  )

  p_fill <- ggplot(df2, aes(x, y, fill = grp)) +
    geom_col()

  expect_no_error(b_fill <- ggplot_build(p_fill))
  expect_gte(length(unique(b_fill$data[[1]]$fill)), n)

})

test_that("User-supplied colour/fill scales are not overridden by theme61 defaults", {

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
