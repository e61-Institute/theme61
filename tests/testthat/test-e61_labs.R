test_that("Functionality in e61_labs works", {

  # Minimum requirements are to provide a title
  lab <- e61_labs(title = "Test")
  expect_equal(lab$title, "Test")
  expect_error(e61_labs())

  # Users should not be able to supply a caption if footnotes or sources are supplied
  expect_error(e61_labs(title = "Something", footnotes = "Test", caption = "Fail"))

  # Test multiple footnotes work
  lab <- e61_labs(title = "Test", footnotes = c("Footnote 1", "Footnote 2"))
  expect_equal(lab$caption, "* Footnote 1\n** Footnote 2")

  # Test single/multiple sources show up differently
  lab <- e61_labs(title = "Test", sources = "Source A")
  expect_equal(lab$caption, "Source: Source A")

  lab <- e61_labs(title = "Test", sources = c("Source A", "Source B"))
  expect_equal(lab$caption, "Sources: Source A; Source B")

  # Test that both footnotes and sources can be provided together
  lab <- e61_labs(title = "Test", footnotes = "Footnote 1", sources = "Source A")
  expect_equal(lab$caption, "* Footnote 1\nSource: Source A")

  # Test that you can pass through other labels e.g. x axis titles
  lab <- e61_labs(title = "Test", x = "X", y = "Y", fill = "Fill")
  expect_equal(
    lab,
    list(x = "X", y = "Y", fill = "Fill", title = "Test", subtitle = NULL, caption = NULL),
    ignore_attr = TRUE
  )

  # Test the text wrapping functionality works sensibly
  lab <- e61_labs(title = "Test", footnote_max_width = 90L, footnotes = "A really really really really really really really really really really really really really long footnote", sources = "A really really really really really really really really really really really long source")

  expect_equal(lab$caption, "* A really really really really really really really really really really really really\nreally long footnote\nSource: A really really really really really really really really really really really\nlong source")

  # And that the position adjusts if you change the max width
  lab <- e61_labs(title = "Test", footnote_max_width = 10L, footnotes = "A really really footnote", sources = "A really really long source")

  expect_equal(lab$caption, "* A really\nreally\nfootnote\nSource: A\nreally\nreally\nlong\nsource")

})
