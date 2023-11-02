# Tests for individual features -------------------------------------------

test_that("Dimensioning functions", {

  # Test custom dimensions work
  withr::with_tempdir({

    plot <- minimal_plot

    suppressWarnings(save_e61("custom-dim.png", plot, dim = list(width = 10, height = 10)))
    g_info <- magick::image_info(magick::image_read("custom-dim.png"))
    expect_equal(g_info$width, 358)
    expect_equal(g_info$height, 354)

    suppressWarnings(save_e61("custom-dim.png", plot, dim = list(width = 10, height = 5)))
    g_info <- magick::image_info(magick::image_read("custom-dim.png"))
    expect_equal(g_info$width, 358)
    expect_equal(g_info$height, 177)

    suppressWarnings(save_e61("custom-dim.png", plot, dim = list(width = 5, height = 10)))
    g_info <- magick::image_info(magick::image_read("custom-dim.png"))
    expect_equal(g_info$width, 181)
    expect_equal(g_info$height, 354)

    # Don't have to specify both?
    suppressWarnings(save_e61("custom-dim.png", plot, dim = list(height = 10)))
    g_info <- magick::image_info(magick::image_read("custom-dim.png"))
    expect_equal(g_info$width, 333)
    expect_equal(g_info$height, 354)

    suppressWarnings(save_e61("custom-dim.png", plot, dim = list(width = 10)))
    g_info <- magick::image_info(magick::image_read("custom-dim.png"))
    expect_equal(g_info$width, 358)
    expect_equal(g_info$height, 269)

  })

})

test_that("Flipped coord formatting", {
  # save_e61() should automatically apply format_flip() to flipped coord graphs

  p1 <-
    minimal_plot +
    coord_flip() +
    labs_e61(title = "Test")

  p2 <-
    minimal_plot +
    coord_flip() +
    format_flip() +
    labs_e61(title = "Test")

  withr::with_tempdir({
    suppressWarnings(save_e61("gg.svg", p1))
    suppressWarnings(save_e61("gg2.svg", p2))

    expect_true(compare_file_binary("gg.svg", "gg2.svg"))
  })

})

test_that("Y-axis label messages", {

  # y-axis text missing or too long
  p1 <- minimal_plot + labs_e61(y = "")
  p2 <- minimal_plot + labs_e61(y = "too long label")

  suppressWarnings(suppressMessages(
    expect_message(
      save_e61(withr::local_tempfile(fileext = ".svg"), p1, p2),
      class = "cliMessage"),
    classes = c("message", "cliMessage")))

  # No message if you do it right
  gg <- minimal_plot

  suppressWarnings(expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), gg)),
                   classes = c("messages", "warning"))

  # No message if y_top is FALSE
  p <- minimal_plot +
    labs_e61(y = "Long y-axis label") +
    theme_e61(y_top = FALSE)

  suppressWarnings(expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), p)),
                   classes = c("messages", "warning"))

  # No message if non-theme61 scale functions are used.
  p <- minimal_plot +
    labs_e61(y = "Long y-axis label that goes on the side") +
    ggplot2::scale_y_continuous()

  suppressWarnings(expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), p)),
                   classes = c("messages", "warning"))

  # No message if session option is set
  withr::with_options(list(no_advisory = TRUE), {
    p <- minimal_plot +
      labs_e61(y = "Long y-axis label")

    suppressWarnings(expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), p)),
                     classes = c("messages", "warning"))
  })
})

test_that("Directory existence checker", {
  p <- minimal_plot

  withr::with_tempdir({

    dir.create("temp_directory")
    dir.create("temp_directory/temp_dir")

    expect_no_error(suppressWarnings(save_e61("plot.svg", p)))
    expect_no_error(suppressWarnings(save_e61("temp_directory/plot.svg", p)))
    expect_no_error(suppressWarnings(save_e61("temp_directory/temp_dir/plot.svg", p)))
    expect_error(suppressWarnings(save_e61("faketemp_directory/plot.svg", p)))
  })

})

test_that("Different file formats", {

  g <- minimal_plot

  withr::with_tempdir({

    # No support for some file formats
    expect_error(suppressWarnings(save_e61("text.jpg")))

    # Having svg in the file name (but not format) should still trip the file format error
    expect_error(suppressWarnings(save_e61("svg-text.jpg")))

    # Make sure the slightly fiddlier PNG saving method works
    suppressWarnings(save_e61("test-png.png", g), classes = c("warning", "message"))
    expect_false(file.exists("test-png.svg"))
    expect_true(file.exists("test-png.png"))

    # Test other supported file types
    expect_no_error(suppressWarnings(save_e61("test.svg", g), classes = c("warning", "message")))
    expect_no_error(suppressWarnings(save_e61("test.pdf", g), classes = c("warning", "message")))
    expect_no_error(suppressWarnings(save_e61("test.eps", g), classes = c("warning", "message")))

  })

})

test_that("Multiple file saving", {
  g <- minimal_plot

  # Test 3 formats
  withr::with_tempdir({
    suppressWarnings(save_e61("test_file", g, format = c("svg", "pdf", "eps")))

    expect_setequal(list.files(pattern = "test_file.*"),
                    c("test_file.eps", "test_file.pdf", "test_file.svg"))

  })

  # Test providing file format in file path
  withr::with_tempdir({
    suppressWarnings(save_e61("test_file.svg", g))

    expect_setequal(list.files(pattern = "test_file.*"),
                    c("test_file.svg"))
  })

  # Test if providing format in path overrules format argument
  withr::with_tempdir({
    suppressWarnings(save_e61("test_file.svg", g, format = "pdf"))

    expect_setequal(list.files(pattern = "test_file.*"),
                    c("test_file.svg"))
  })

  # Test what happens if nothing is provided (do the defaults do what you expect?)
  withr::with_tempdir({
    suppressWarnings(save_e61("test_file", g))

    expect_setequal(list.files(pattern = "test_file.*"),
                    c("test_file.svg", "test_file.pdf", "test_file.eps", "test_file.png"))
  })

  # Error if invalid filename used
  withr::with_tempdir({
    expect_error(suppressWarnings(save_e61("test_file", g, format = "mp3")))
  })

})

test_that("Does save_data work", {

  gg <- minimal_plot

  withr::with_tempdir({
    expect_no_error(suppressWarnings(save_e61("graph.svg", gg, save_data = TRUE)))
    expect_no_error(suppressWarnings(save_e61("graph", gg, format = "svg", save_data = TRUE)))
  })

  # This should leave the $data container empty
  gg <- ggplot() +
    geom_point(data = data, aes(x, y)) +
    geom_point(data = data, aes(x, y))

  withr::with_tempdir({
    expect_error(suppressWarnings(save_e61("graph.svg", save_data = TRUE)))
  })
})

test_that("Change background colour", {

  p <- minimal_plot

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-bg-col-pink.svg", p, bg_colour = "pink")))
    expect_snapshot_file(suppressWarnings(save_e61("plot-bg-col-box.svg", p, bg_colour = e61_skylight8)))
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-bg-col-box.svg", plotlist = list(p, p), bg_colour = e61_skylight8)))
  })
})

# Check whole-graph generation consistency --------------------------------

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
    geom_histogram()

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-single-hist.svg", p)))
  })

  ## geom_density graph ----
  p <- ggplot(data, aes(x)) +
    geom_density()

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
    geom_rect(xmin = 1.25, xmax = 1.75, ymin = -Inf, ymax = Inf,
              fill = e61_greydark, alpha = 0.1)

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-geom_rect.svg", p)))
  })
})

test_that("Multi-panel graph examples", {

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

  # 1x2 graph
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-1x2.svg", p1, p2)))
  })

  # 2x1 graph
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-2x1.svg", p1, p2, ncol = 1)))
  })

  # 2x2 graph
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-2x2.svg", p1_t, p2_t, p3_t, p4_t)))
  })

  # 2x3 graph
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-2x3.svg", p1_t, p2_t, p3_t, p4_t, p1_t, p2_t, ncol = 3)))
  })

  # 3x2 graph
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-3x2.svg", p1_t, p2_t, p3_t, p4_t, p1_t, p2_t, ncol = 2)))
  })

  # 1x2 graph with long common footnotes + sources
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-long-footer.svg", p1, p2,
               title = "Multi-panel graph title text",
               subtitle = "Multi-panel graph subtitle text",
               footnotes = "Long sentence about footnotes that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on...",
               sources = c("Sources", "Sauces"))))
  })

  # 1x2 graph with long title
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-long-title.svg", p1, p2,
               title = "Multi-panel graph title text that goes on and on and on and on and on and on and on and on and on and on and on",
               subtitle = "Multi-panel graph subtitle text",
               footnotes = "Long sentence about footnotes that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on...",
               sources = c("Sources", "Sauces"))))
  })

  # 1x2 graph with long panel titles and subtitles
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

  # 1x2 graph with 1 long panel title and subtitles
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61("plot-multi-1x2-1-long-panel-title.svg", p1_lt, p2_t,
               title = "Multi-panel graph title text",
               subtitle = "Multi-panel graph subtitle text",
               footnotes = "Long sentence about footnotes that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on...",
               sources = c("Sources", "Sauces"))))
  })

})
