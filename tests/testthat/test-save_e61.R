test_that("Test dimensioning functions", {

  # Test default height messages
  withr::with_tempdir({

    temp_file <- "default-height.png"

    expect_message(save_e61(temp_file, ggplot()))

  })

  # Test custom dimensions work
  withr::with_tempdir({
    expect_snapshot_file(save_e61("custom-dim-1.svg", ggplot(), width = 10, height = 10))
    expect_snapshot_file(save_e61("custom-dim-2.svg", ggplot(), width = 10, height = 5))
    expect_snapshot_file(save_e61("custom-dim-3.svg", ggplot(), width = 5, height = 10))
  })

  # Test horizontal graph detection code
  withr::with_tempdir({
    plot <- ggplot()
    plot_h <- ggplot() + coord_flip()

    expect_snapshot_file(suppressMessages(save_e61("plot-norm.svg", plot)))
    expect_snapshot_file(suppressMessages(save_e61("plot-flip.svg", plot_h)))

  })

})

test_that("Test resizing feature for PNGs", {

  # Create a graph that will be written to disk (and deleted afterwards)
  g <- ggplot()

  withr::with_tempdir({
    temp_file <- "test.png"

    # Test PNG with default scaling
    suppressMessages(save_e61(temp_file))

    disk_file <- magick::image_read(temp_file)
    deets <- magick::image_info(disk_file)

    expected_deets <-
      tibble::tibble(
        format = "PNG",
        width = 334,
        height = 354
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
        width = 669,
        height = 708
      )

    lapply(c("format", "width", "height"), function(x) {
      expect_equal(deets[[x]], expected_deets[[x]])
    })

  })

})

test_that("Test support for different file formats", {

  # Create a graph that will be written to disk (and deleted afterwards)
  g <- ggplot()

  withr::with_tempdir({
    temp_file <- "test.svg"

    suppressMessages(save_e61(temp_file))

    disk_file <- magick::image_read(temp_file)
    deets <- magick::image_info(disk_file)

    expected_deets <-
      tibble::tibble(
        format = "SVG",
        width = 321,
        height = 340
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
