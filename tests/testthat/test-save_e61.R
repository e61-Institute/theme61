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

