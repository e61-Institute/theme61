test_that("Test resizing feature for PNGs", {

  # Create a graph that will be written to disk (and deleted afterwards)
  g <- ggplot()
  temp_file <- paste0(tempdir(), "\\test.png")

  # Test PNG with default scaling
  e61_save(temp_file)

  disk_file <- magick::image_read(temp_file)
  deets <- magick::image_info(disk_file)

  expected_deets <-
    tibble::tibble(
      format = "PNG",
      width = 800,
      height = 600
    )

  lapply(c("format", "width", "height"), function(x) {
    expect_equal(deets[[x]], expected_deets[[x]])
    })

  # Test resized PNG
  e61_save(temp_file, resize = 2)

  disk_file <- magick::image_read(temp_file)
  deets <- magick::image_info(disk_file)

  expected_deets <-
    tibble::tibble(
      format = "PNG",
      width = 1600,
      height = 1200
    )

  lapply(c("format", "width", "height"), function(x) {
    expect_equal(deets[[x]], expected_deets[[x]])
  })

  # Cleanup
  unlink(temp_file)
})

test_that("Test support for different file formats", {

  # Create a graph that will be written to disk (and deleted afterwards)
  g <- ggplot()

  temp_file <- paste0(tempdir(), "\\test.svg")

  e61_save(temp_file)

  disk_file <- magick::image_read(temp_file)
  deets <- magick::image_info(disk_file)

  expected_deets <-
    tibble::tibble(
      format = "SVG",
      width = 768,
      height = 576
    )

  lapply(c("format", "width", "height"), function(x) {
    expect_equal(deets[[x]], expected_deets[[x]])
  })

  # Cleanup
  unlink(temp_file)

  ## These should fail

  # SVGs should fail if user tries to resize them
  expect_error(e61_save(temp_file, resize = 2))

  # No support for non-SVG/PNG files
  expect_error(e61_save(paste0(tempdir(), "\\text.jpg")))

  # Having png or svg in the name should still trip the file format error
  expect_error(e61_save(paste0(tempdir(), "\\png-text.jpg")))

})
