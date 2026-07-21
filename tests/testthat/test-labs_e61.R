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
  expect_equal(lab$caption, "* Footnote 1\n** Footnote 2")
})

test_that("Single and multiple sources are formatted differently", {
  lab <- labs_e61(title = "Test", sources = "Source A")
  expect_equal(lab$caption, "Source: Source A")

  lab <- labs_e61(title = "Test", sources = c("Source A", "Source B"))
  expect_equal(lab$caption, "Sources: Source A; Source B")

  lab <- labs_e61(title = "Test", sources = c("Test", "Alphabeticalisation"))
  expect_equal(lab$caption, "Sources: Alphabeticalisation; Test")
})

test_that("Footnotes and sources can be provided together", {
  lab <- labs_e61(title = "Test", footnotes = "Footnote 1", sources = "Source A")
  expect_equal(lab$caption, "* Footnote 1\nSource: Source A")
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
