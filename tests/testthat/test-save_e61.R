# Tests for individual features -------------------------------------------

test_that("Dimensioning functions", {

  # Turn off messages
  withr::local_options(list(test_save = TRUE,
                            quiet_wrap = TRUE))

  # Test custom dimensions work
  withr::with_tempdir({

    plot <- minimal_plot

    suppressWarnings(save_e61("custom-dim.svg", plot, width = 10, height = 10))
    g_info <- magick::image_info(magick::image_read("custom-dim.svg"))
    expect_equal(g_info$width, 378)
    expect_equal(g_info$height, 378)

    suppressWarnings(save_e61("custom-dim.svg", plot, width = 10, height = 5))
    g_info <- magick::image_info(magick::image_read("custom-dim.svg"))
    expect_equal(g_info$width, 378)
    expect_equal(g_info$height, 189)

    suppressWarnings(save_e61("custom-dim.svg", plot, width = 5, height = 10))
    g_info <- magick::image_info(magick::image_read("custom-dim.svg"))
    expect_equal(g_info$width, 189)
    expect_equal(g_info$height, 378)

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

  withr::local_options(list(test_save = TRUE,
                            quiet_wrap = TRUE))

  # y-axis text missing or too long
  withr::with_tempdir({

    p1 <- minimal_plot + labs_e61(y = "")
    p2 <- minimal_plot + labs_e61(y = "too long label")

    suppressWarnings(suppressMessages(
      expect_message(save_e61("test.svg", p1, p2), class = "cliMessage"),
      classes = c("message", "cliMessage")))

  })

  # No message if you do it right
  gg <- minimal_plot

  suppressWarnings(expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), gg)),
                   classes = c("messages", "warning"))

  # No message if y_top is FALSE
  p <- minimal_plot +
    labs_e61(y = "Long y-axis label that goes on the side") +
    theme_e61(y_top = FALSE)

  suppressWarnings(expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), p)),
                   classes = c("messages", "warning"))

})

test_that("Directory existence checker", {
  p <- minimal_plot

  withr::with_tempdir({

    dir.create("temp_directory")
    dir.create("temp_directory/temp_dir")

    expect_no_error(save_e61("plot.svg", p))
    expect_no_error(save_e61("temp_directory/plot.svg", p))
    expect_no_error(save_e61("temp_directory/temp_dir/plot.svg", p))
    expect_error(save_e61("faketemp_directory/plot.svg", p))
  })

})

test_that("Different file formats", {

  withr::local_options(list(test_save = TRUE,
                            quiet_wrap = TRUE))

  g <- minimal_plot

  withr::with_tempdir({

    # No support for some file formats
    expect_error(suppressWarnings(save_e61("text.jpg")))

    # Having svg in the file name (but not format) should still trip the file format error
    expect_error(suppressWarnings(save_e61("svg-text.jpg")))

    # Make sure the slightly fiddlier PNG saving method works
    suppressWarnings(save_e61("test.png", g), classes = c("warning", "message"))
    expect_false(file.exists("test.svg"))
    expect_true(file.exists("test.png"))

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
    suppressMessages(save_e61("test_file", g, format = c("svg", "pdf", "eps")))

    expect_setequal(list.files(pattern = "test_file.*"),
                    c("test_file.eps", "test_file.pdf", "test_file.svg"))

  })

  # Test providing file format in file path
  withr::with_tempdir({
    suppressMessages(save_e61("test_file.svg", g))

    expect_setequal(list.files(pattern = "test_file.*"),
                    c("test_file.svg"))
  })

  # Test if providing format in path overrules format argument
  withr::with_tempdir({
    suppressMessages(save_e61("test_file.svg", g, format = "pdf"))

    expect_setequal(list.files(pattern = "test_file.*"),
                    c("test_file.svg"))
  })

  # Test what happens if nothing is provided (do the defaults do what you expect?)
  withr::with_tempdir({
    suppressMessages(save_e61("test_file", g))

    expect_setequal(list.files(pattern = "test_file.*"),
                    c("test_file.svg", "test_file.pdf", "test_file.eps", "test_file.png"))
  })

  # Error if invalid filename used
  withr::with_tempdir({
    expect_error(suppressMessages(save_e61("test_file", g, format = "mp3")))
  })

})

test_that("Does save_data work", {
  gg <- minimal_plot

  withr::with_tempdir({
    expect_no_error(suppressMessages(save_e61("graph.svg", gg, save_data = TRUE)))
    expect_no_error(suppressMessages(save_e61("graph", gg, format = "svg", save_data = TRUE)))
  })

  # This should leave the $data container empty
  gg <- ggplot() +
    geom_point(data = data, aes(x, y)) +
    geom_point(data = data, aes(x, y))

  withr::with_tempdir({
    expect_error(suppressMessages(save_e61("graph.svg", save_data = TRUE)))
  })
})

# Check whole-graph generation consistency --------------------------------

test_that("Single-panel graph examples", {

  # Put a comprehensive series of graph examples here

  # Graph with cont-y var with values from 0-20

  # Graph with cont-y var with small values from 0-1

  # Graph with cont-y var with large values from -1000-1000

  # Graph with cont x and y vars

  # Graph with discrete x and y vars

  # Graph with date x var

  # Flipped coord graph discrete x var, cont y var

  # Date x-var, ribbon y-var

  # geom_histogram graph

  # geom_density graph

})

test_that("Multi-panel graph examples", {

  # 1x2 graph

  # 2x1 graph

  # 2x2 graph

  # 2x3 graph

  # 3x2 graph

  # 1x2 graph with long common footnotes + sources

  # 1x2 graph with long title

  # 1x2 graph with long panel titles and subtitles

})

test_that("Output graphs have sensible dimensions", {

  # This test should be run manually and the results inspected by hand
  skip("This 'test' is only for interactive purposes")

  # Generate some data
  graph_data <- data.frame(x = runif(100, 1, 49), y = runif(100, 1, 49),
                           xcol = 1:100)

  # Graph
  graph <- ggplot(graph_data, aes(x, y)) +
    geom_point() +
    theme_e61() +
    scale_y_continuous_e61(limits = c(0, 50, 10)) +
    scale_colour_e61(1) +
    labs_e61(
      title = "Fairly Lengthy Graph Title With A Lot of Words To Take Up Space",
      subtitle = "Fairly Lengthy Graph Subtitle With A Lot of Words To Take Up Space",
      footnotes = "Really long footnote to test that part of the code blah blah blah blah blah blah",
      sources = c("e61 Institute"),
      x = NULL, y = "units"
    )

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), graph))

  # Graph 2
  graph_2 <- ggplot(graph_data, aes(xcol, y)) +
    geom_col() +
    theme_e61() +
    scale_y_continuous_e61(limits = c(0, 50, 10)) +
    scale_colour_e61(1) +
    labs_e61(
      title = "Fairly Lengthy Graph Title With A Lot of Words To Take Up Space",
      subtitle = "Fairly Lengthy Graph Subtitle With A Lot of Words To Take Up Space",
      footnotes = "Really long footnote to test that part of the code blah blah blah blah blah blah",
      sources = c("e61 Institute"),
      x = NULL, y = "units"
    )

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), graph_2))

})

test_that("Test saving of multi-panel graphs", {

  # This test should be run manually and the results inspected by hand
  skip("This 'test' is only for interactive purposes")

  # Generate some data
  graph_data <- data.frame(x = runif(100, 1, 49), y = runif(100, 1, 49),
                           xcol = 1:100)

  # Graph
  graph <- ggplot(graph_data, aes(x, y)) +
    geom_point() +
    theme_e61() +
    scale_y_continuous_e61(limits = c(0, 50, 10)) +
    scale_colour_e61(1) +
    labs_e61(
      title = "Graph",
      footnotes = "Really long footnote to test that part of the code blah blah blah blah blah blah",
      sources = c("e61 Institute"),
      x = NULL, y = "units"
    )

  # No title or subtitle
  mp <- mpanel_e61(graph, graph)

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), mp))

  # Title, no subtitle
  mp <- mpanel_e61(graph, graph,
                   title = "Title")

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), mp, height = 8))

  # No title, with subtitle
  mp <- mpanel_e61(graph, graph,
                   subtitle = "Graph subtitle")

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), mp, height = 8))

  # Title, caption
  mp <- mpanel_e61(graph, graph,
                   title = "Title",
                   sources = "Source 1")

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), mp))

  # No title, no subtitle, caption
  mp <- mpanel_e61(graph, graph,
                   sources = "Source 1")

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), mp))

  # No title, no subtitle, footnotes, sources
  mp <- mpanel_e61(graph, graph,
                   footnotes = "Footnote 1",
                   sources = "Source 1")

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), mp, height = 8.3))

  # Title, subtitle, footnotes, sources
  mp <- mpanel_e61(graph, graph,
                   title = "Title",
                   subtitle = "Graph subtitle",
                   footnotes = "Footnote 1",
                   sources = "Source 1")

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), mp))

  # 2x2 with title, subtitle, footnotes, sources
  mp <- mpanel_e61(graph, graph, graph, graph,
                   ncol = 2,
                   title = "Title",
                   subtitle = "Graph subtitle",
                   footnotes = "Footnote 1",
                   sources = "Source 1",
                   show_height = TRUE)

  rstudioapi::viewer(save_e61(withr::local_tempfile(fileext = ".svg"), mp))

})

