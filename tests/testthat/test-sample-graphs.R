test_that("Single-panel graph examples", {

  withr::local_seed(42)

  ## Cont-y var with values from 0-20 ----
  data <- data.frame(x = factor(1:10), y = runif(10, 0, 20))

  p <- ggplot(data, aes(x, y)) +
    geom_col()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-cont-y.svg", p)))
  })

  ## Cont-y var with small values from 0-1 ----
  data <- data.frame(x = factor(1:10), y = runif(10, 0, 1))

  p <- ggplot(data, aes(x, y)) +
    geom_col()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-cont-y-sml-val.svg", p)))
  })

  ## Cont-y var with negative values from -20 to 0 ----
  data <- data.frame(x = factor(1:10), y = runif(10, -20, 0))

  p <- ggplot(data, aes(x, y)) +
    geom_col()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-cont-y-neg-val.svg", p)))
  })

  ## Cont-y var with large values from -1000 to +1000 ----
  data <- data.frame(x = factor(1:10), y = runif(10, -1000, 1000))

  p <- ggplot(data, aes(x, y)) +
    geom_col()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-cont-y-lg-val.svg", p)))
  })

  ## Cont x and y vars ----
  data <- data.frame(x = runif(10, -1, 1), y = runif(10, -1, 1))

  p <- ggplot(data, aes(x, y)) +
    geom_point()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-cont-x-y.svg", p)))
  })

  ## Discrete x and y vars ----
  data <- data.table::CJ(x = factor(1:10), y_var = factor(1:10))
  data[, fill := runif(100, 0, 100)]

  p <- ggplot(data, aes(x, y = y_var, fill = fill)) +
    geom_tile()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-disc-x-y.svg", p)))
  })

  ## Date x var ----
  data <- data.frame(
    x = seq.Date(as.Date("2011-01-01"), by = "1 year", length.out = 10),
    y = runif(10, -1, 1)
  )

  p <- ggplot(data, aes(x, y)) +
    geom_line()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-date-x.svg", p)))
  })

  ## Date x var and colours ----
  data <- data.frame(
    x = rep(seq.Date(as.Date("2011-01-01"), by = "1 year", length.out = 10), 2),
    y = runif(20, -1, 1),
    colour = c(rep("A", 10), rep("B", 10))
  )

  p <- ggplot(data, aes(x, y, colour = colour)) +
    geom_line()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-date-x-colour.svg", p)))
  })

  ## Flipped coord with discrete x var, cont y var ----
  data <- data.frame(x = factor(1:10), y = runif(10, 0, 10))

  p <- ggplot(data, aes(x, y)) +
    geom_col() +
    coord_flip()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-cont-flip.svg", p)))
  })

  ## Date x-var, ribbon y-var ----
  data <- data.frame(
    x = seq.Date(as.Date("2011-01-01"), by = "1 year", length.out = 10),
    y = runif(10, -1, 1),
    ymin = runif(10, -2, -1.1),
    ymax = runif(10, 1.1, 2)
  )

  p <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_line() +
    geom_ribbon(alpha = 0.1)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-cont-ymin-max.svg", p)))
  })

  ## geom_histogram graph ----
  data <- data.frame(x = rnorm(1000))

  p <- ggplot(data, aes(x)) +
    geom_histogram(bins = 20) +
    scale_y_continuous_e61()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-hist.svg", p)))
  })

  ## geom_density graph ----
  p <- ggplot(data, aes(x)) +
    geom_density() +
    labs_e61(y = "dens") +
    scale_y_continuous_e61()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-cont-dens.svg", p)))
  })

  ## Horizontal time series ----
  p <- ggplot(data.frame(x = 1:2, y = rep(100, 2)), aes(x, y)) +
    geom_line()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-straight-line.svg", p)))
  })

  ## Plot with geom_rect ----
  p <- ggplot(data.frame(x = 1:3, y = c(90, 100, 110)), aes(x, y)) +
    geom_line() +
    geom_rect(xmin = 1.25, xmax = 1.75, ymin = 90, ymax = 110,
              fill = e61_greydark, alpha = 0.1)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-geom_rect.svg", p)))
  })
})

test_that("Multi-panel graph examples", {

  skip_if_not(interactive())

  withr::local_seed(42)

  # Graphs to use in the panels

  # Cont-y var with small values from 0-1
  data <- data.frame(x = factor(1:10), y = runif(10, 0, 1))

  p1 <- ggplot(data, aes(x, y)) +
    geom_col()

  p1_t <- p1 +
    labs_e61(title = "Panel graph title text",
             subtitle = "Panel graph subtitle text",
             y = "ppt")

  # Graph with date x var and colour
  data <- data.frame(
    x = rep(seq.Date(as.Date("2011-01-01"), by = "1 year", length.out = 10), 2),
    y = runif(20, -1, 1),
    colour = c(rep("A", 10), rep("B", 10))
  )

  p2 <- ggplot(data, aes(x, y, colour = colour)) +
    geom_line()

  p2_t <- p2 +
    labs_e61(title = "Panel graph title text",
             subtitle = "Panel graph subtitle text",
             y = "'000")

  # Cont-y var with large values from -1000 to +1000
  data <- data.frame(x = factor(1:10), y = runif(10, -1000, 1000))

  p3 <- ggplot(data, aes(x, y)) +
    geom_col()

  p3_t <- p3 +
    labs_e61(title = "Panel graph title text",
             subtitle = "Panel graph subtitle text",
             y = "value")

  # Date x-var, ribbon y-var
  data <- data.frame(
    x = seq.Date(as.Date("2011-01-01"), by = "1 year", length.out = 10),
    y = runif(10, -1, 1),
    ymin = runif(10, -2, -1.1),
    ymax = runif(10, 1.1, 2)
  )

  p4 <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_line() +
    geom_ribbon(alpha = 0.1)

  p4_t <- p4 +
    labs_e61(title = "Panel graph title text",
             subtitle = "Panel graph subtitle text")

  ## 1x2 graph ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-1x2.svg", p1, p2)))
  })

  ## 2x1 graph ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-2x1.svg", p1, p2, ncol = 1)))
  })

  ## 2x2 graph ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-2x2.svg", p1_t, p2_t, p3_t, p4_t)))
  })

  ## 2x3 graph ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-2x3.svg", p1_t, p2_t, p3_t, p4_t, p1_t, p2_t, ncol = 3)))
  })

  ## 3x2 graph ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-3x2.svg", p1_t, p2_t, p3_t, p4_t, p1_t, p2_t, ncol = 2)))
  })

  ## 1x2 graph with long common footnotes + sources ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-long-footer.svg", p1, p2,
               title = "Multi-panel graph title text",
               subtitle = "Multi-panel graph subtitle text",
               footnotes = "Long sentence about footnotes that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on...",
               sources = c("Sources", "Sauces"))))
  })

  ## 1x2 graph with long title ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-long-title.svg", p1, p2,
               title = "Multi-panel graph title text that goes on and on and on and on and on and on and on and on and on and on and on",
               subtitle = "Multi-panel graph subtitle text",
               footnotes = "Long sentence about footnotes that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on...",
               sources = c("Sources", "Sauces"))))
  })

  ## 1x2 graph with long panel titles and subtitles ----
  p1_lt <- p1 +
    labs_e61(title = "Really long panel title title title title title title title title",
             subtitle = "Really long panel title title title title title")

  p2_lt <- p2 +
    labs_e61(title = "Really long panel title title title title title",
             subtitle = "Really long panel title title title title title")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-long-panel-title.svg", p1_lt, p2_lt,
               title = "Multi-panel graph title text",
               subtitle = "Multi-panel graph subtitle text",
               footnotes = "Long sentence about footnotes that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on...",
               sources = c("Sources", "Sauces"))))
  })

  ## 1x2 graph with 1 long panel title and subtitles ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-1-long-panel-title.svg", p1_lt, p2_t,
               title = "Multi-panel graph title text",
               subtitle = "Multi-panel graph subtitle text",
               footnotes = "Long sentence about footnotes that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on...",
               sources = c("Sources", "Sauces"))))
  })

  ## Test pad_width > 0 values ----
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-1-long-panel-title-padwidth.svg", p1_t, p2_t, pad_width = 3,
               title = "Multi-panel graph title text",
               subtitle = "Multi-panel graph subtitle text",
               footnotes = "Long sentence about footnotes that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on...",
               sources = c("Sources", "Sauces"))))
  })

  ## Check spacing with multi-panel title, no subtitle, and panel subtitles ----

  p1_t <- p1 +
    labs_e61(subtitle = "Panel graph subtitle",
             y = "ppt")

  p2_t <- p2 +
    labs_e61(subtitle = "Panel graph subtitle",
             y = "ppt")


  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-1-title-no subtitle.svg", p1_t, p2_t,
               title = "Multi-panel graph title text")))
  })

})

test_that("Map examples", {

  skip_if_not(interactive())

  skip_if_not_installed("sf")
  skip_if_not_installed("strayr")
  library(sf)

  sa4_shp <- strayr::read_absmap("sa42016")

  sydney_map <- dplyr::filter(sa4_shp, gcc_code_2016 == "1GSYD")

  ## Simple map with title and subtitle ----
  p <- ggplot(data = sydney_map) +
    geom_sf(colour = "black") +
    labs_e61(title = "Map of Greater Sydney", subtitle = "Sydney SA4s") +
    theme_e61_spatial()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-simple-map.svg", p)))
  })

  ## Map with legends ----
  p <- ggplot(data = sydney_map) +
    geom_sf(aes(fill = as.numeric(sa4_code_2016)), colour = "black") +
    labs_e61(title = "Map of Greater Sydney", subtitle = "Sydney SA4s",
             fill = "SA4 code") +
    theme_e61_spatial(legend = "right", legend_title = T)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-legend-map.svg", p)))
  })
})
