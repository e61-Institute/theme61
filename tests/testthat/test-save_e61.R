test_that("Test dimensioning functions", {

  # Test default height messages
  withr::with_tempdir({

    temp_file <- "default-height.png"

    expect_message(save_e61(temp_file, ggplot()))

  })

  # Test custom dimensions work
  withr::with_tempdir({

    suppressMessages(save_e61("custom-dim.svg", ggplot(), width = 10, height = 10))
    g_info <- magick::image_info(magick::image_read("custom-dim.svg"))
    expect_equal(g_info$width, 378)
    expect_equal(g_info$height, 378)

    suppressMessages(save_e61("custom-dim.svg", ggplot(), width = 10, height = 5))
    g_info <- magick::image_info(magick::image_read("custom-dim.svg"))
    expect_equal(g_info$width, 378)
    expect_equal(g_info$height, 189)

    suppressMessages(save_e61("custom-dim.svg", ggplot(), width = 5, height = 10))
    g_info <- magick::image_info(magick::image_read("custom-dim.svg"))
    expect_equal(g_info$width, 189)
    expect_equal(g_info$height, 378)

  })

  # Test horizontal graph detection code
  withr::with_tempdir({
    plot <- ggplot() + labs(title = "Test")
    plot_h <- ggplot() + coord_flip() + labs(title = "Test")

    suppressMessages(save_e61("gg.svg", plot))
    g_info <- magick::image_info(magick::image_read("gg.svg"))
    expect_equal(g_info$width, 321)
    expect_equal(g_info$height, 268)

    suppressMessages(save_e61("gg.svg", plot_h))
    g_info <- magick::image_info(magick::image_read("gg.svg"))
    expect_equal(g_info$width, 643)
    expect_equal(g_info$height, 454)
  })

})

test_that("Test resizing feature for PNGs", {

  # Create a graph that will be written to disk (and deleted afterwards)
  g <- ggplot() + labs(title = "Test")

  withr::with_tempdir({
    temp_file <- "test.png"

    # Test PNG with default scaling
    suppressMessages(save_e61(temp_file))

    disk_file <- magick::image_read(temp_file)
    deets <- magick::image_info(disk_file)

    expected_deets <-
      tibble::tibble(
        format = "PNG",
        width = 240,
        height = 201
      )

    lapply(c("format", "width", "height"), function(x) {
      expect_equal(deets[[x]], expected_deets[[x]])
    })

    # Test resized PNG
    suppressMessages(save_e61(temp_file, resize = 2))

    disk_file <- magick::image_read(temp_file)
    deets <- magick::image_info(disk_file)

    expected_deets <-
      tibble::tibble(
        format = "PNG",
        width = 481,
        height = 402
      )

    lapply(c("format", "width", "height"), function(x) {
      expect_equal(deets[[x]], expected_deets[[x]])
    })

  })

})

test_that("Test support for different file formats", {

  # Create a graph that will be written to disk (and deleted afterwards)
  g <- ggplot() + labs(title = "Test")

  withr::with_tempdir({
    temp_file <- "test.svg"

    suppressMessages(save_e61(temp_file))

    disk_file <- magick::image_read(temp_file)
    deets <- magick::image_info(disk_file)

    expected_deets <-
      tibble::tibble(
        format = "SVG",
        width = 321,
        height = 268
      )

    lapply(c("format", "width", "height"), function(x) {
      expect_equal(deets[[x]], expected_deets[[x]])
    })

    ## These should fail

    # SVGs should fail if user tries to resize them
    expect_error(suppressMessages(save_e61(temp_file, resize = 2)))

    # No support for non-SVG/PNG files
    expect_error(suppressMessages(save_e61(paste0(tempdir(), "\\text.jpg"))))

    # Having png or svg in the name should still trip the file format error
    expect_error(suppressMessages(save_e61(paste0(tempdir(), "\\png-text.jpg"))))

  })

  expect_no_error(suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), g), classes = c("warning", "message")))
  expect_no_error(suppressWarnings(save_e61(withr::local_tempfile(fileext = ".pdf"), g), classes = c("warning", "message")))
  expect_no_error(suppressWarnings(save_e61(withr::local_tempfile(fileext = ".png"), g), classes = c("warning", "message")))

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

test_that("Test whether save_data works", {
  data <- data.frame(x = 1, y = 1)

  dir <- tempdir()

  gg <- ggplot(data, aes(x, y)) +
    geom_point()

  expect_no_error(suppressMessages(save_e61(file.path(dir, "graph.svg"), save_data = TRUE)))
  expect_no_error(suppressMessages(save_e61(file.path(dir, "graph"), format = "svg", save_data = TRUE)))

  # This should leave the $data container empty
  gg <- ggplot() +
    geom_point(data = data, aes(x, y)) +
    geom_point(data = data, aes(x, y))

  expect_error(suppressMessages(save_e61(file.path(dir, "graph.svg"), save_data = TRUE)))
})

test_that("Test advisory messages", {
  # Ensure option is not set
  options(no_t61_style_msg = FALSE)

  # No theming, no y-axis
  gg <- ggplot()

  expect_snapshot(
    suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
  )

  # No colour palette
  gg <- ggplot(data.frame(x = LETTERS[1:3], y = 1:3), aes(x, y, fill = x)) +
    geom_col()

  expect_snapshot(
    suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
  )

  # y-axis text missing
  gg <- ggplot(data.frame(x = LETTERS[1:3], y = 1:3), aes(x, y, fill = x)) +
    geom_col() +
    scale_y_continuous_e61(c(0, 4)) +
    scale_fill_e61(3) +
    theme_e61() +
    labs_e61(y = NULL)

  expect_snapshot(
    suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
  )

  # y-axis text too long
  gg <- ggplot(data.frame(x = LETTERS[1:3], y = 1:3), aes(x, y, fill = x)) +
    geom_col() +
    scale_y_continuous_e61(c(0, 4)) +
    scale_fill_e61(3) +
    theme_e61() +
    labs_e61(y = "Really long y-axis label")

  expect_snapshot(
    suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
  )

  # No message if you do it right
  gg <- ggplot(data.frame(x = LETTERS[1:3], y = 1:3), aes(x, y, fill = x)) +
    geom_col() +
    scale_y_continuous_e61(c(0, 4)) +
    scale_fill_e61(3) +
    theme_e61()

  expect_no_message(
    suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
  )

  # No messages for multipanels
  gg <- mpanel_e61(gg)

  expect_no_message(
    suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
  )
})

test_that("Test multiple file format saving features", {
  g <- ggplot()

  # Test 3 formats
  withr::with_tempdir({
    suppressMessages(save_e61("test_file", g, format = c("svg", "pdf", "eps")))

    expect_setequal(list.files(pattern = "test_file.*"),
                 c("test_file.eps", "test_file.pdf", "test_file.svg"))

  })

  # Test if PNG breaks everything (it shouldn't)
  withr::with_tempdir({
    suppressMessages(save_e61("test_file", g, format = c("svg", "png")))

    expect_setequal(list.files(pattern = "test_file.*"),
                 c("test_file.svg", "test_file.png"))
  })

  # Test providing file format in file path
  withr::with_tempdir({
    suppressMessages(save_e61("test_file.svg", g))

    expect_setequal(list.files(pattern = "test_file.*"),
                 c("test_file.svg"))
  })

  # Test if providing format in path overrules format argument
  withr::with_tempdir({
    suppressMessages(save_e61("test_file.svg", g, format = "png"))

    expect_setequal(list.files(pattern = "test_file.*"),
                 c("test_file.svg"))
  })

  # Test what happens if nothing is provided (do the defaults do what you expect?)
  withr::with_tempdir({
    suppressMessages(save_e61("test_file", g))

    expect_setequal(list.files(pattern = "test_file.*"),
                 c("test_file.svg", "test_file.png", "test_file.pdf", "test_file.eps"))
  })

  # Error if invalid filename used
  withr::with_tempdir({
    expect_error(suppressMessages(save_e61("test_file", g, format = "mp3")))
  })
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

  debug(mpanel_e61)
  debug(save_e61)
  undebug(mpanel_e61)
  undebug(save_e61)
  debugonce(mpanel_e61)
  debugonce(save_e61)

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
