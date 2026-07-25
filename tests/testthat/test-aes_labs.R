# Tests of the internal text-wrapping helpers in aes_labs.R -----------------

test_that("get_lines fills each line as tightly as possible without overflowing", {
  text <- paste(
    "This is a very long footnote that is designed to wrap across several",
    "lines so we can check that the text wrapping algorithm fills each line",
    "as much as possible without letting any words spill past the right edge",
    "of the chart, which was the original bug that was reported and",
    "subsequently fixed."
  )

  font_size <- 8
  plot_width <- 18.98 # cm, representative of a real two-panel chart width

  lines <- get_lines(text, font_size, plot_width)

  for (i in seq_len(nrow(lines))) {

    line_text <- lines$collapsed_text[i]
    line_width <- get_text_width(line_text, font_size = font_size)

    # No line should ever exceed the available width (no clipping)
    expect_lte(line_width, plot_width)

    # Every line except the last should be as full as possible: the first
    # word of the next line must not have fit on this line, otherwise the
    # wrap is happening too early and wasting available width
    if (i < nrow(lines)) {
      next_word <- strsplit(lines$collapsed_text[i + 1], " ")[[1]][1]
      width_with_next_word <- get_text_width(paste(line_text, next_word), font_size = font_size)

      expect_gt(width_with_next_word, plot_width)
    }
  }
})

test_that("get_lines does not split a single line of text that already fits", {
  text <- "A short subtitle"
  lines <- get_lines(text, font_size = 10, plot_width = 100)

  expect_equal(nrow(lines), 1)
  expect_equal(lines$collapsed_text, text)
})
