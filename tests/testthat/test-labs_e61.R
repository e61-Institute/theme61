test_that("Functionality in labs_e61 works", {

  # Users should not be able to supply a caption if footnotes or sources are supplied
  expect_error(labs_e61(title = "Something", footnotes = "Test", caption = "Fail"))

  # Test multiple footnotes work
  lab <- labs_e61(title = "Test", footnotes = c("Footnote 1", "Footnote 2"))
  expect_equal(lab$caption, "* Footnote 1\n** Footnote 2")

  # Test single/multiple sources show up differently
  lab <- labs_e61(title = "Test", sources = "Source A")
  expect_equal(lab$caption, "Source: Source A")

  lab <- labs_e61(title = "Test", sources = c("Source A", "Source B"))
  expect_equal(lab$caption, "Sources: Source A; Source B")

  lab <- labs_e61(title = "Test", sources = c("Test", "Alphabeticalisation"))
  expect_equal(lab$caption, "Sources: Alphabeticalisation; Test")

  # Test that both footnotes and sources can be provided together
  lab <- labs_e61(title = "Test", footnotes = "Footnote 1", sources = "Source A")
  expect_equal(lab$caption, "* Footnote 1\nSource: Source A")

  # Test that you can pass through other labels e.g. x axis titles
  lab <- labs_e61(title = "Test", x = "X", y = "Y", fill = "Fill", y_top = F)
  expect_equal(
    lab,
    list(x = "X", y = "Y", fill = "Fill", title = "Test", subtitle = "<span style='font-size:10pt'></span>", caption = NULL),
    ignore_attr = TRUE
  )

  # Test the text wrapping functionality works sensibly
  lab <-
    labs_e61(
      title = "Test",
      footnote_wrap = 90L,
      footnotes = "A really really really really really really really really really really really really really long footnote",
      sources = "A really really really really really really really really really really really long source"
    )

  expect_equal(lab$caption,
               "* A really really really really really really really really really really really really\nreally long footnote\nSource: A really really really really really really really really really really really\nlong source",
               ignore_attr = TRUE)

  # And that the position adjusts if you change the max width
  lab <-
    labs_e61(
      title = "Test",
      footnote_wrap = 10L,
      footnotes = "A really really footnote",
      sources = "A really really long source"
    )

  expect_equal(lab$caption,
               "* A really\nreally\nfootnote\nSource: A\nreally\nreally\nlong\nsource",
               ignore_attr = TRUE)

  # Test wrapping functionality for titles
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
