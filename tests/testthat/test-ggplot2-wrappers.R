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
    ggplot2::scale_y_continuous(
      breaks = c(0, 50, 100),
      limits = c(0, 110)
    )

  # Also test an override added *before* the geom
  p_override2 <- ggplot(data) +
    ggplot2::scale_y_continuous(
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
