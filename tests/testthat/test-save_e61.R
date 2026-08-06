# Tests for individual features -------------------------------------------

test_that("Dimensioning functions", {

  # Test custom dimensions work
  withr::with_tempdir({

    plot <- minimal_plot

    suppressWarnings(save_e61("custom-dim.png", plot, dim = list(width = 10, height = 10)))
    g_info1 <- magick::image_info(magick::image_read("custom-dim.png"))

    suppressWarnings(save_e61("custom-dim.png", plot, dim = list(width = 10, height = 5)))
    g_info2 <- magick::image_info(magick::image_read("custom-dim.png"))

    suppressWarnings(save_e61("custom-dim.png", plot, dim = list(width = 5, height = 10)))
    g_info3 <- magick::image_info(magick::image_read("custom-dim.png"))

    expect_equal(g_info1$width, g_info2$width, tolerance = 1)
    expect_equal(g_info1$width, g_info3$width * 2, tolerance = 10)

    expect_equal(g_info1$height, g_info3$height, tolerance = 1)
    expect_equal(g_info1$height, g_info2$height * 2, tolerance = 10)

    # Don't have to specify both?
    suppressWarnings(expect_no_error(save_e61("custom-dim.png", plot, dim = list(height = 10))))
    suppressWarnings(expect_no_error(save_e61("custom-dim.png", plot, dim = list(width = 10))))

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

  # No message if you do it right
  gg <- minimal_plot

  suppressWarnings(expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), gg)),
                   classes = c("messages", "warning"))

  # No message if session option is set
  withr::with_options(list(no_advisory = TRUE), {
    p <- minimal_plot +
      labs_e61(y = "Long y-axis label")

    suppressWarnings(expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), p)),
                     classes = c("messages", "warning"))
  })
})

test_that("Y-axis customisation options", {
  p <- minimal_plot

  # Limits, sec_axis
  p1 <- p +
    scale_y_continuous_e61(limits = c(0, 1.5, 0.5)) +
    labs_e61(title = "Y-scale testing")

  # Limits, no sec_axis
  p2 <- p +
    scale_y_continuous_e61(limits = c(0, 1.5, 0.5), sec_axis = FALSE) +
    labs_e61(title = "Y-scale testing")

  # No limits, sec_axis
  p3 <- p + labs_e61(title = "Y-scale testing")

  # No limits, no sec_axis
  p4 <- p +
    scale_y_continuous_e61(sec_axis = FALSE) +
    labs_e61(title = "Y-scale testing")

  # Flipped graph
  p5 <- p +
    theme_e61() +
    coord_flip() +
    labs_e61(title = "Flipped graph",
             y = "Long y-axis text")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("y-scale-test1.svg", p1)))
    expect_snapshot_file(suppressWarnings(save_e61("y-scale-test2.svg", p2)))
    expect_snapshot_file(suppressWarnings(save_e61("y-scale-test3.svg", p3)))
    expect_snapshot_file(suppressWarnings(save_e61("y-scale-test4.svg", p4)))
    expect_snapshot_file(suppressWarnings(save_e61("y-scale-test5.svg", p5)))
  })

})

test_that("save_e61 autoscaling preserves custom y breaks", {
  set.seed(1)
  df <- data.frame(
    x = 1:10,
    y = rnorm(10)
  )

  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_y_continuous_e61(limits = c(-30, 30, 2))

  p_scaled <- update_scales(p, auto_scale = TRUE)

  breaks <- ggplot_build(p_scaled)$layout$panel_scales_y[[1]]$breaks

  expect_equal(breaks, seq(-30, 30, 2))
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
    expect_error(suppressWarnings(save_e61("text.tif")))

    # Having svg in the file name (but not format) should still trip the file format error
    expect_error(suppressWarnings(save_e61("svg-text.tif")))

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
                    c("test_file.svg", "test_file.pdf", "test_file.eps", "test_file.png", "test_file.jpg"))
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

    # The data file should include the .csv extension (#313)
    expect_true(file.exists("graph.csv"))
  })

  # Each panel gets its own "<graph name> <panel number>.csv", even when
  # panels use different data frames
  df1 <- data.frame(x = c(0, 1), y = c(0, 1))
  df2 <- data.frame(x = c(0, 1), y = c(1, 4))

  p1 <- ggplot(df1, aes(x, y)) + geom_point()
  p2 <- ggplot(df2, aes(x, y)) + geom_point()

  withr::with_tempdir({
    expect_no_error(suppressWarnings(save_e61("multi-data.svg", plotlist = list(p1, p2), save_data = TRUE)))

    expect_equal(data.table::fread("multi-data 1.csv")$y, df1$y)
    expect_equal(data.table::fread("multi-data 2.csv")$y, df2$y)
  })

  # Data supplied per-geom rather than to ggplot() itself leaves $data empty,
  # so save_data should error instead of writing something useless
  gg <- ggplot() +
    geom_point(data = data, aes(x, y)) +
    geom_point(data = data, aes(x, y))

  withr::with_tempdir({
    expect_error(suppressWarnings(save_e61("graph.svg", save_data = TRUE)))
  })

  # The validation should catch a bad panel anywhere, not just plots[[1]]
  withr::with_tempdir({
    expect_error(suppressWarnings(save_e61("multi-bad.svg", plotlist = list(p1, gg), save_data = TRUE)))
  })
})

test_that("return_plot_obj returns the composed multi-panel object without saving", {

  df1 <- data.frame(x = c(0, 1), y = c(0, 1))
  df2 <- data.frame(x = c(0, 1), y = c(1, 4))

  p1 <- ggplot(df1, aes(x, y)) + geom_point()
  p2 <- ggplot(df2, aes(x, y)) + geom_point()

  withr::with_tempdir({
    obj <- save_e61(plotlist = list(p1, p2), title = "Combined", return_plot_obj = TRUE)

    expect_s3_class(obj, "patchwork")
    expect_length(list.files(), 0)
  })
})

test_that("return_plot_obj errors for a single plot", {
  expect_error(
    save_e61(plotlist = list(minimal_plot), return_plot_obj = TRUE),
    "only supported for multi-panel graphs"
  )
})

test_that("return_plot_obj doesn't require a filename", {
  df1 <- data.frame(x = c(0, 1), y = c(0, 1))
  df2 <- data.frame(x = c(0, 1), y = c(1, 4))

  p1 <- ggplot(df1, aes(x, y)) + geom_point()
  p2 <- ggplot(df2, aes(x, y)) + geom_point()

  expect_no_error(save_e61(plotlist = list(p1, p2), return_plot_obj = TRUE))
})

test_that("resolve_aspect_ratio respects user theme customisations", {

  p <- minimal_plot # already carries theme_e61()'s default aspect.ratio = 0.75

  # No user customisation - chart_type still controls the aspect ratio
  expect_equal(resolve_aspect_ratio(p, "normal")@theme$aspect.ratio, 0.75)
  expect_equal(resolve_aspect_ratio(p, "wide")@theme$aspect.ratio, 0.5)
  expect_equal(resolve_aspect_ratio(p, "square")@theme$aspect.ratio, 1)

  # User has customised the aspect ratio away from the theme_e61() default -
  # chart_type should no longer override it
  p_custom <- p + theme(aspect.ratio = 0.6)

  expect_equal(resolve_aspect_ratio(p_custom, "normal")@theme$aspect.ratio, 0.6)
  expect_equal(resolve_aspect_ratio(p_custom, "wide")@theme$aspect.ratio, 0.6)
})

test_that("update_margins leaves blanked elements alone but still sizes the rest", {

  # A blanked element should be skipped entirely, not overridden and reverted
  blanked_theme <- (minimal_plot + theme(axis.text.y = element_blank()))@theme
  margins_theme <- update_margins(blanked_theme, base_size = 10, legend_title = element_blank())

  expect_null(margins_theme$axis.text.y)

  # Non-blanked elements should still get a base_size-scaled margin - the
  # blank-skipping logic must not neuter update_margins()'s actual job
  default_theme <- minimal_plot@theme
  margins_10 <- update_margins(default_theme, base_size = 10, legend_title = element_blank())
  margins_20 <- update_margins(default_theme, base_size = 20, legend_title = element_blank())

  expect_equal(as.numeric(margins_10$axis.text.y$margin)[2], 10 / 5)
  expect_equal(as.numeric(margins_20$axis.text.y$margin)[2], 20 / 5)
})

test_that("resolve_text_size respects user theme customisations", {

  p <- minimal_plot # already carries theme_e61()'s default text size (10)

  # No user customisation - the base_size argument still controls the size
  resolved <- resolve_text_size(p, 14)
  expect_equal(resolved$plot@theme$text$size, 14)
  expect_equal(resolved$base_size, 14)

  # User has customised the text size away from the theme_e61() default -
  # base_size should no longer override it
  p_custom <- p + theme(text = element_text(size = 18))
  resolved_custom <- resolve_text_size(p_custom, 14)

  expect_equal(resolved_custom$plot@theme$text$size, 18)
  expect_equal(resolved_custom$base_size, 18)
})

test_that("format_flip skips elements the user has customised", {

  p <- minimal_plot +
    theme(
      panel.grid.major.x = element_line(colour = "red"),
      panel.grid.major.y = element_line(colour = "blue")
    )

  flipped <- p + format_flip(current_theme = p@theme)

  # The user's customisations should survive
  expect_equal(flipped@theme$panel.grid.major.x$colour, "red")
  expect_equal(flipped@theme$panel.grid.major.y$colour, "blue")

  # Elements the user never touched should still be applied
  expect_true(inherits(flipped@theme$axis.text.x.top, "element_blank"))
  expect_equal(flipped@theme$axis.title.x.bottom$angle, 0)
})

test_that("save_e61 does not overwrite user theme changes", {

  p <- minimal_plot +
    theme(
      axis.text.y = element_blank(),
      aspect.ratio = 0.5
    )

  result <- save_single(
    filename = "theme-overwrite-test",
    plot = p,
    chart_type = NULL,
    auto_scale = TRUE,
    width = NULL,
    height = NULL,
    max_height = NULL,
    format = "svg",
    base_size = 10,
    pad_width = 0,
    pad_height = 0,
    bg_colour = "white"
  )

  expect_equal(result$graph@theme$aspect.ratio, 0.5)
  expect_true(inherits(result$graph@theme$axis.text.y, "element_blank"))
})

test_that("save_e61 preserves a custom text size and flipped-coord theme changes", {

  p <- minimal_plot +
    theme(text = element_text(size = 20)) +
    coord_flip() +
    theme(panel.grid.major.x = element_line(colour = "red"))

  result <- save_single(
    filename = "size-flip-test",
    plot = p,
    chart_type = NULL,
    auto_scale = TRUE,
    width = NULL,
    height = NULL,
    max_height = NULL,
    format = "svg",
    base_size = 10,
    pad_width = 0,
    pad_height = 0,
    bg_colour = "white"
  )

  expect_equal(result$graph@theme$text$size, 20)
  expect_equal(result$graph@theme$panel.grid.major.x$colour, "red")
})

test_that("Change background colour", {

  p <- minimal_plot

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(save_e61("plot-bg-col-pink.svg", p, bg_colour = "pink")))
    expect_snapshot_file(suppressWarnings(save_e61("plot-bg-col-box.svg", p, bg_colour = e61_skylight8)))
    expect_snapshot_file(suppressWarnings(save_e61("plot-multi-bg-col-box.svg", plotlist = list(p, p), bg_colour = e61_skylight8)))
  })
})

test_that("PNG resolution changer works", {

  plot <- minimal_plot

  withr::with_tempdir({
    suppressWarnings(save_e61("png-1.png", plot))
    suppressWarnings(save_e61("png-2.png", plot, res = 2))
    g_info1 <- magick::image_info(magick::image_read("png-1.png"))
    g_info2 <- magick::image_info(magick::image_read("png-2.png"))
    expect_equal(g_info1$width * 2, g_info2$width, tolerance = 0.1)
    expect_equal(g_info1$height * 2, g_info2$height, tolerance = 0.1)

    expect_snapshot_file(suppressWarnings(save_e61("png-1.png", plot)))
    expect_snapshot_file(suppressWarnings(save_e61("png-2.png", plot, res = 2)))
  })
})

test_that("Preview mode works", {
  p <- minimal_plot

  withr::with_tempdir({

    # Check that the file is saved if preview is FALSE
    suppressWarnings(save_e61("plot.svg", p, preview = FALSE))
    expect_true(file.exists("plot.svg"))
    unlink("plot.svg")

    # Check that no file is saved in preview mode
    expect_message(save_e61("plot.svg", p, preview = TRUE),
                   ".*Preview mode is activated.*")
    expect_false(file.exists("plot.svg"))

    # It is not possible to check if the graph appears in the Viewer pane
    # automatically, so run the below code to manually check functionality if
    # required.

    # save_e61(p, preview = TRUE)
  })
})

test_that("set_format works", {
  p <- minimal_plot

  withr::with_tempdir({

    set_format(c("pdf", "jpg"))

    # Check filename extension is not overridden by set_format
    suppressWarnings(save_e61("plot1.svg", p))
    expect_true(file.exists("plot1.svg"))

    # Check formats are used if file extension is not provided
    suppressWarnings(save_e61("plot2", p))
    expect_true(file.exists("plot2.pdf"))
    expect_true(file.exists("plot2.jpg"))

    # Check formats are used if file formats are provided in save_e61
    suppressWarnings(save_e61("plot3", p, format = c("svg", "png")))
    expect_true(file.exists("plot3.svg"))
    expect_true(file.exists("plot3.png"))

    # Check unset formatting works
    unset_format()

    suppressWarnings(save_e61("plot4", p))
    expect_true(file.exists("plot4.svg"))
    expect_true(file.exists("plot4.pdf"))
    expect_true(file.exists("plot4.eps"))
    expect_true(file.exists("plot4.jpg"))
    expect_true(file.exists("plot4.png"))

    # Check you can change the format again
    set_format(c("pdf", "jpg"))
    set_format(c("png", "svg"))

    suppressWarnings(save_e61("plot5", p))
    expect_true(file.exists("plot5.png"))
    expect_true(file.exists("plot5.svg"))

  })


})

test_that("Spell checker works", {
  # Typo in various places
  plots <- list()
  plots[["title"]] <- minimal_plot + labs_e61(title = "Opertaing expenses")
  plots[["subtitle"]] <- minimal_plot + labs_e61(subtitle = "Problmatic subtitle text")
  plots[["footnote"]] <- minimal_plot + labs_e61(footnotes = "Opertaing sektor mistkaes")
  plots[["sources"]] <- minimal_plot + labs_e61(sources = c("Governmment", "Treasury", "Institute"))
  plots[["everywhere"]] <- minimal_plot + labs_e61(
    title = "Opertaing",
    subtitle = "Wrnog speeling",
    footnotes = "Opertaing sektor mistkaes",
    sources = c("Governmment", "Treasury", "Institute"))

  suppressWarnings(suppressMessages(
      expect_message(
        save_e61(withr::local_tempfile(fileext = ".svg"), plots[["title"]]),
        class = "cliMessage"),
      classes = c("message", "cliMessage")))

  suppressWarnings(suppressMessages(
    expect_message(
      save_e61(withr::local_tempfile(fileext = ".svg"), plots[["subtitle"]]),
      class = "cliMessage"),
    classes = c("message", "cliMessage")))

  suppressWarnings(suppressMessages(
    expect_message(
      save_e61(withr::local_tempfile(fileext = ".svg"), plots[["footnote"]]),
      class = "cliMessage"),
    classes = c("message", "cliMessage")))

  suppressWarnings(suppressMessages(
    expect_message(
      save_e61(withr::local_tempfile(fileext = ".svg"), plots[["sources"]]),
      class = "cliMessage"),
    classes = c("message", "cliMessage")))

  suppressWarnings(suppressMessages(
    expect_message(
      save_e61(withr::local_tempfile(fileext = ".svg"), plots[["everywhere"]]),
      class = "cliMessage"),
    classes = c("message", "cliMessage")))


  # No message if no typo
  p <- minimal_plot + labs_e61(title = "Operating")

  suppressWarnings(expect_no_message(
    save_e61(withr::local_tempfile(fileext = ".svg"), p)),
    classes = c("messages", "warning"))

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
