test_that("Title stays NULL when not supplied, so no space is reserved for it", {
  lab <- labs_e61(subtitle = "Subtitle")
  expect_null(lab$title)

  lab <- labs_e61(title = NULL, subtitle = "Subtitle", title_wrap = 50L)
  expect_null(lab$title)
})

test_that("y-axis title (y_top = TRUE) doesn't get an empty leading line when no subtitle is supplied", {
  lab <- labs_e61(y = "Y label")
  expect_equal(lab$subtitle, "<span style='font-size:9pt'>Y label</span>", ignore_attr = TRUE)
  expect_false(grepl("^<span[^>]*></span><br>", lab$subtitle))

  # subtitle actually supplied - should still combine subtitle + y-axis title
  lab <- labs_e61(subtitle = "Subtitle", y = "Y label")
  expect_equal(
    lab$subtitle,
    "<span style='font-size:10pt'>Subtitle</span><br><span style='font-size:9pt'>Y label</span>",
    ignore_attr = TRUE
  )
})

test_that("Users should not be able to supply a caption if footnotes or sources are supplied", {
  expect_error(
    labs_e61(title = "Something", footnotes = "Test", caption = "Fail")
  )
})

test_that("Multiple footnotes are formatted correctly", {
  lab <- labs_e61(title = "Test", footnotes = c("Footnote 1", "Footnote 2"))
  expect_equal(lab$caption, "* Footnote 1\n** Footnote 2", ignore_attr = TRUE)
})

test_that("Single and multiple sources are formatted differently", {
  lab <- labs_e61(title = "Test", sources = "Source A")
  expect_equal(lab$caption, "Source: Source A", ignore_attr = TRUE)

  lab <- labs_e61(title = "Test", sources = c("Source A", "Source B"))
  expect_equal(lab$caption, "Sources: Source A; Source B", ignore_attr = TRUE)

  lab <- labs_e61(title = "Test", sources = c("Test", "Alphabeticalisation"))
  expect_equal(lab$caption, "Sources: Alphabeticalisation; Test", ignore_attr = TRUE)
})

test_that("Footnotes and sources can be provided together", {
  lab <- labs_e61(title = "Test", footnotes = "Footnote 1", sources = "Source A")
  expect_equal(lab$caption, "* Footnote 1\nSource: Source A", ignore_attr = TRUE)
})

test_that("Other labels (x, y, fill) are passed through correctly", {
  lab <- labs_e61(title = "Test", x = "X", y = "Y", fill = "Fill", y_top = FALSE)

  compare_lab <- list(
    x = "X",
    y = "Y",
    fill = "Fill",
    title = "Test",
    subtitle = "<span style='font-size:10pt'></span>",
    caption = NULL
  )

  class(compare_lab) <- c("ggplot2::labels", "gg", "S7_object")

  expect_equal(
    lab,
    compare_lab,
    ignore_attr = TRUE
  )
})

test_that("y_top moves y-axis title into the subtitle", {
  lab <- labs_e61(title = "Test", subtitle = "Sub", y = "Y title", y_top = TRUE)

  expect_null(lab$y)
  expect_true(grepl("Y title", lab$subtitle, fixed = TRUE))
})

test_that("Caption is NULL when no footnotes or sources are provided", {
  lab <- labs_e61(title = "Test", subtitle = "Sub")
  expect_null(lab$caption)
})

test_that("Footnote and source text wrapping works sensibly", {
  lab <- labs_e61(
    title = "Test",
    footnote_wrap = 90L,
    footnotes = "A really really really really really really really really really really really really really long footnote",
    sources = "A really really really really really really really really really really really long source"
  )

  expect_equal(
    lab$caption,
    "* A really really really really really really really really really really really really\nreally long footnote\nSource: A really really really really really really really really really really really\nlong source",
    ignore_attr = TRUE
  )
})

test_that("Footnote and source wrapping responds to different max widths", {
  lab <- labs_e61(
    title = "Test",
    footnote_wrap = 10L,
    footnotes = "A really really footnote",
    sources = "A really really long source"
  )

  expect_equal(
    lab$caption,
    "* A really\nreally\nfootnote\nSource: A\nreally\nreally\nlong\nsource",
    ignore_attr = TRUE
  )
})

test_that("Title and subtitle wrapping works correctly", {
  lab <- labs_e61(
    title = "A really really really really really really really really really really really really long title",
    subtitle = "A really really really really really really really really really really really really long title",
    title_wrap = 65L,
    subtitle_wrap = 75L
  )

  expect_equal(
    lab$title,
    "A really really really really really really really really really\nreally really really long title",
    ignore_attr = TRUE
  )

  expect_equal(
    lab$subtitle,
    "<span style='font-size:10pt'>A really really really really really really really really really really
really really long title</span>",
    ignore_attr = TRUE
  )
})

test_that("Non-string titles fail", {
  expect_error(labs_e61(title = 123))
  expect_error(labs_e61(subtitle = TRUE))
  expect_error(labs_e61(y = list(1,2,3)))
})

# Structured label parts (#364) -----------------------------------------------
#
# labs_e61() keeps the footnotes/sources and the subtitle/y-axis title as
# structured values, and the draw-time re-wrap (rescale_text(), once the final
# plot width is known) re-renders from those values instead of splitting the
# generated text back apart. The tests below use a very wide plot_width so the
# re-wrap is a round trip: anything that differs is a parsing failure rather
# than wrapping.

test_that("A footnote containing the word 'Source' isn't mistaken for the sources line", {
  lab <- labs_e61(
    footnotes = "Source data are unavailable before 2001",
    sources = "ABS"
  )

  expect_equal(
    lab$caption,
    "* Source data are unavailable before 2001\nSource: ABS",
    ignore_attr = TRUE
  )

  expect_equal(
    rescale_text(lab$caption, "caption", font_size = 8, plot_width = 100),
    "* Source data are unavailable before 2001\nSource: ABS"
  )
})

test_that("A footnote that looks like a sources line survives the re-wrap", {
  lab <- labs_e61(
    footnotes = c("Sources: as listed in the appendix", "Second footnote"),
    sources = c("ABS", "e61")
  )

  expect_equal(
    rescale_text(lab$caption, "caption", font_size = 8, plot_width = 100),
    "* Sources: as listed in the appendix\n** Second footnote\nSources: ABS; e61"
  )
})

test_that("Colons in the footnotes, title, subtitle and y-axis title don't break the re-wrap", {
  lab <- labs_e61(
    title = "Wages: a puzzle",
    subtitle = "Growth: 2001 to 2020",
    y = "Index: 2001 = 100",
    footnotes = "Note: figures are seasonally adjusted",
    sources = "ABS"
  )

  expect_equal(
    rescale_text(lab$title, "title", font_size = 14, plot_width = 100),
    "Wages: a puzzle"
  )

  expect_equal(
    rescale_text(lab$subtitle, "subtitle", font_size = 10, plot_width = 100),
    paste0(
      "<span style='font-size:10pt'>Growth: 2001 to 2020</span><br>",
      "<span style='font-size:9pt'>Index: 2001 = 100</span>"
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    rescale_text(lab$caption, "caption", font_size = 8, plot_width = 100),
    "* Note: figures are seasonally adjusted\nSource: ABS"
  )
})

test_that("A full set of labels round trips through the draw-time re-wrap", {
  lab <- labs_e61(
    title = "A title",
    subtitle = "A subtitle",
    y = "A y-axis title",
    footnotes = c("First footnote", "Second footnote"),
    sources = c("e61", "ABS")
  )

  expect_equal(
    rescale_text(lab$title, "title", font_size = 14, plot_width = 100),
    "A title"
  )

  expect_equal(
    rescale_text(lab$subtitle, "subtitle", font_size = 10, plot_width = 100),
    paste0(
      "<span style='font-size:10pt'>A subtitle</span><br>",
      "<span style='font-size:9pt'>A y-axis title</span>"
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    rescale_text(lab$caption, "caption", font_size = 8, plot_width = 100),
    "* First footnote\n** Second footnote\nSources: ABS; e61"
  )
})

test_that("A y-axis title on its own round trips through the draw-time re-wrap", {
  lab <- labs_e61(y = "A y-axis title")

  expect_equal(
    rescale_text(lab$subtitle, "subtitle", font_size = 9, plot_width = 100),
    "<span style='font-size:9pt'>A y-axis title</span>",
    ignore_attr = TRUE
  )
})

test_that("Sources alone, and footnotes alone, round trip through the draw-time re-wrap", {
  lab <- labs_e61(sources = c("ABS", "e61"))

  expect_equal(
    rescale_text(lab$caption, "caption", font_size = 8, plot_width = 100),
    "Sources: ABS; e61"
  )

  lab <- labs_e61(footnotes = "A footnote")

  expect_equal(
    rescale_text(lab$caption, "caption", font_size = 8, plot_width = 100),
    "* A footnote"
  )
})

test_that("Labels with tricky text survive the full label update", {
  p <- minimal_plot +
    labs_e61(
      title = "Wages: a puzzle",
      subtitle = "Growth: 2001 to 2020",
      y = "Index: 2001 = 100",
      footnotes = "Source data are unavailable before 2001",
      sources = "ABS"
    )

  updated <- update_labs(p, plot_width = 15)

  expect_equal(updated@labels$title, "Wages: a puzzle")

  expect_true(grepl("Index: 2001 = 100", updated@labels$subtitle, fixed = TRUE))

  expect_equal(
    updated@labels$caption,
    "* Source data are unavailable before 2001\nSource: ABS"
  )
})
