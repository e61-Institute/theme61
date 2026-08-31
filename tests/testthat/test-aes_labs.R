# Tests of the internal text-wrapping helpers in aes_labs.R -----------------

test_that("get_lines fills each line as tightly as possible without overflowing", {
  text <- paste(
    "This is a very long footnote that is designed to wrap across several",
    "lines so we can check that the text wrapping algorithm fills each line",
    "as much as possible without letting any words spill past the right edge",
    "of the chart."
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

# Tests of update_plot_label() (#356) ----------------------------------------

test_that("update_plot_label() rescales a default-sized plot_label() layer", {
  p <- minimal_plot_label +
    plot_label(label = "A", x = 1, y = 1, auto_position = FALSE)

  base_size <- 20
  updated <- theme61:::update_plot_label(p, chart_type = "normal", base_size = base_size)

  last_layer <- updated@layers[[length(updated@layers)]]
  expect_equal(last_layer$aes_params$size, 3.5 * base_size / 10)
})

test_that("update_plot_label() leaves an explicitly-sized plot_label() layer alone", {
  p <- minimal_plot_label +
    plot_label(label = "A", x = 1, y = 1, size = 6, auto_position = FALSE)

  base_size <- 20
  updated <- theme61:::update_plot_label(p, chart_type = "normal", base_size = base_size)

  last_layer <- updated@layers[[length(updated@layers)]]
  expect_equal(last_layer$aes_params$size, 6)
})

# Tests of update_labs() (#358) -----------------------------------------------

test_that("update_labs() doesn't drop labels it doesn't itself manage", {
  p <- minimal_plot_label +
    ggplot2::labs(
      title = "My title", subtitle = "My sub", caption = "My cap",
      x = "X axis", y = "Y axis", tag = "A", alt = "accessibility text"
    )

  updated <- update_labs(p, plot_width = 10)

  # title/subtitle/caption are the fields update_labs() itself computes
  expect_false(is.null(updated@labels$title))
  expect_false(is.null(updated@labels$subtitle))
  expect_false(is.null(updated@labels$caption))

  # everything else should pass through untouched
  expect_equal(updated@labels$x, "X axis")
  expect_equal(updated@labels$y, "Y axis")
  expect_equal(updated@labels$tag, "A")
  expect_equal(updated@labels$alt, "accessibility text")
})

test_that("update_labs() doesn't introduce a stray fill entry when fill isn't set", {
  p <- minimal_plot_label + ggplot2::labs(title = "T")

  updated <- update_labs(p, plot_width = 10)

  expect_false("fill" %in% names(updated@labels))
})
