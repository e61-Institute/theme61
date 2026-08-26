# Tests of label wrapping of theme61 charts ---------------------------------

# The overall title/subtitle/footnotes on multi-panel plots are re-wrapped by
# the internal rescale_text_multi() based on the rendered plot width (see
# save_multi.R). That width-dependent behaviour is what the multi-panel
# snapshot tests further down exist to exercise, but it can be checked much
# more cheaply by calling the wrapping function directly instead of rendering
# a full multi-panel plot for every combination of arrangement/width.
test_that("rescale_text_multi wraps title text to more lines as plot width shrinks", {

  long_text <- paste(rep("word", 40), collapse = " ")

  narrow_title <- rescale_text_multi(long_text, "title", font_size = 14, plot_width = 5)
  wide_title <- rescale_text_multi(long_text, "title", font_size = 14, plot_width = 200)

  expect_true(grepl("\n", narrow_title))
  expect_false(grepl("\n", wide_title))

  narrow_lines <- length(strsplit(narrow_title, "\n")[[1]])
  wide_lines <- length(strsplit(wide_title, "\n")[[1]])

  expect_gt(narrow_lines, wide_lines)

  # No words should be dropped or duplicated by the wrapping
  expect_equal(
    sort(strsplit(gsub("\n", " ", narrow_title), " ")[[1]]),
    sort(strsplit(long_text, " ")[[1]])
  )
})

test_that("rescale_text_multi wraps subtitle text to more lines as plot width shrinks", {

  long_text <- paste(rep("word", 40), collapse = " ")

  narrow_subtitle <- rescale_text_multi(long_text, "subtitle", font_size = 12, plot_width = 5)
  wide_subtitle <- rescale_text_multi(long_text, "subtitle", font_size = 12, plot_width = 200)

  expect_true(grepl("\n", narrow_subtitle))
  expect_false(grepl("\n", wide_subtitle))

  narrow_lines <- length(strsplit(narrow_subtitle, "\n")[[1]])
  wide_lines <- length(strsplit(wide_subtitle, "\n")[[1]])

  expect_gt(narrow_lines, wide_lines)
})

test_that("rescale_text_multi collapses manual line breaks before re-wrapping", {

  # Manual line breaks in the input shouldn't survive - the text should be
  # re-flowed as a single paragraph and re-wrapped based on plot_width
  text_with_breaks <- "word word word\nword word word\nword word word"

  wrapped <- rescale_text_multi(text_with_breaks, "title", font_size = 14, plot_width = 200)

  expect_false(grepl("\n", wrapped))
  expect_equal(
    strsplit(wrapped, " ")[[1]],
    rep("word", 9)
  )
})

# Single-panel graph examples ------------------------------------------------
#
# The wrapped-text content itself (title_wrap/subtitle_wrap/footnote_wrap/
# y_top) is unit tested directly and exactly in test-labs_e61.R without any
# rendering. The snapshot tests below are kept only where they exercise a
# rendering code path that isn't covered there: the y-axis title moving into
# the subtitle row, all label fields combined, extreme-length wrapping,
# manual width overrides, facets, and coord_flip.

test_that("Single-panel graph examples", {
  skip_on_os(c("mac", "windows"))

  withr::local_seed(42)

  # 1 - Just y-axis ----
  p1 <- minimal_plot + labs_e61(y = "Just a y-axis label")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p1, filename = "plot-sngle-wrp-test-4.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 2 - All titles ----
  p2 <- minimal_plot +
    labs_e61(
      title = "This is a title",
      y = "Just a y-axis label that goes on and on and on and on and on",
      subtitle = "This is a subtitle",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS')
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p2, filename = "plot-sngle-wrp-test-8.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 3 - Long titles and subtitles ----
  p3 <- minimal_plot +
    labs_e61(
      title = "This is a very very very very very long title that really should just be one line",
      subtitle = "A test of a very very very very very long subtitle, but that's probably okay because subtitles can be long",
      y = "Just a y-axis label that should probably be shoter given the title really goes on a bit too long and now that makes two titles",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS')
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p3, filename = "plot-sngle-wrp-test-9.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 4 - Long sources - these should now be wrapped ----
  p4 <- minimal_plot +
    labs_e61(
      title = "This is a title",
      y = "Just a y-axis label that goes on and on and on and on and on",
      subtitle = "This is a subtitle",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS', "And a third source that is more of a description now that takes up too much space.")
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p4, filename = "plot-sngle-wrp-test-10.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 5 - Manual width - check that the wrapping expands ----
  p5 <- minimal_plot +
    labs_e61(
      title = "This is a very very very very very long title that really should just be one line",
      subtitle = "A test of a very very very very very long subtitle, but that's probably okay because subtitles can be long",
      y = "Just a y-axis label that should probably be shoter given the title really goes on a bit too long and now that makes two titles",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS')
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p5, dim = list(width = 20), filename = "plot-sngle-wrp-test-12.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 6 - Faceted with manual width ----
  facetted_plot <-
    ggplot(data.frame(x = c(0, 1), y = c(0, 1), group = c("A", "B")), aes(x, y)) +
    facet_wrap(vars(group)) +
    geom_point()

  p6 <- facetted_plot +
    labs_e61(
      title = "This is a very very very very very long title that really should just be one line but now it's a plot with a title that is two lines",
      subtitle = "A test of a very very very very very long subtitle, but that's probably okay because subtitles can be long",
      y = "Just a y-axis label that should probably be shoter given the title already goes on a bit too long, which makes two long titles, but is that really such a bad thing?",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS')
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p6, filename = "plot-sngle-wrp-test-13.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 7 - Plot with coordinates flipped ----
  bar_chart <- ggplot(data.frame(x = factor(1:10), y = runif(10, 0, 20)), aes(x, y)) +
    geom_col()

  p7 <- bar_chart +
    coord_flip() +
    format_flip() +
    labs_e61(
      title = "This is a title",
      y = "Just a y-axis label that goes on and on and on and on and on",
      subtitle = "This is a subtitle",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS', "And a third source that is more of a description now that takes up too much space.")
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p7, filename = "plot-sngle-wrp-test-14.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })
})


# Tests of multi-plots ----
#
# Per-panel label wrapping (labs_e61() on each panel plot) uses a fixed
# width and doesn't depend on the panel arrangement, so it's only rendered
# once per label-content scenario below rather than once per arrangement -
# arrangement mechanics (ncol/nrow/pad_width) are already covered by
# test-sample-graphs.R. The overall title/subtitle/footnotes passed directly
# to save_e61() *do* depend on the arrangement (rescale_text_multi() wraps
# them to the rendered plot width, checked directly above), so one
# representative arrangement is kept per scenario as an end-to-end check that
# the wiring works, rather than every arrangement combination.

test_that("Multi-panel graph examples", {
  skip_on_os(c("mac", "windows"))

  withr::local_seed(42)

  # Graphs to use in the panels

  # Cont-y var with small values from 0-1
  data <- data.frame(x = factor(1:10), y = runif(10, 0, 1))

  bar_chart <- ggplot(data, aes(x, y)) +
    geom_col()

  # 1 - Titles ----
  p1a <- minimal_plot + labs_e61(title = "This is a title")

  p1b <- bar_chart + labs_e61(title = "This is a title")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p1a, p1b, filename = "plot-multi-wrp-test-4.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 2 - Subtitles ----
  p2a <- minimal_plot + labs_e61(subtitle = "This is a subtitle")

  p2b <- bar_chart + labs_e61(subtitle = "This is a subtitle")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p2a, p2b, filename = "plot-multi-wrp-test-7.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 3 - Y-axis titles ----
  p3a <- minimal_plot + labs_e61(y = "This is a y-axis")

  p3b <- bar_chart + labs_e61(y = "This is a y-axis")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p3a, p3b, filename = "plot-multi-wrp-test-10.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 4 - Footnotes and sources ----
  p4a <- minimal_plot +
    labs_e61(
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit."
      ),
      sources = c("e61", 'ABS')
    )

  p4b <- bar_chart +
    labs_e61(
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS')
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p4a, p4b, filename = "plot-multi-wrp-test-13.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 5 - Full labels ----
  p5a <- minimal_plot +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit."
      ),
      sources = c("e61", 'ABS')
    )

  p5b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS')
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p5a, p5b, filename = "plot-multi-wrp-test-16.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 6 - Full long labels ----
  p6a <- minimal_plot +
    labs_e61(
      title = "This is a very very very very very long title that really should just be one line",
      subtitle = "A test of a very very very very very long subtitle, but that's probably okay because subtitles can be long",
      y = "Just a y-axis label that should probably be shoter given the title really goes on a bit too long and now that makes two titles",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit."
      ),
      sources = c("e61", 'ABS')
    )

  p6b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit anim id est laborum."
      ),
      sources = c("e61", 'ABS')
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p6a, p6b, filename = "plot-multi-wrp-test-19.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 7 - Full labels just long enough ----
  p7a <- minimal_plot +
    labs_e61(
      title = "This is the title of the plot that is just right!!",
      subtitle = "A test of a very very very long subtitle, but not too long.",
      y = "Just a y-axis label that goes on and on and on and on and on",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit."
      ),
      sources = c("e61", 'ABS')
    )

  p7b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart"
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p7a, p7b, filename = "plot-multi-wrp-test-22.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 8 - Add padding ----
  # (kept at two variants - unlike the scenarios above, this one is
  # specifically about how pad_width/pad_height combine, not just about
  # label content, so both the pad_width-only and pad_width+pad_height
  # cases are retained)
  p8a <- minimal_plot +
    labs_e61(
      title = "This is the title of the plot that is just right!!",
      subtitle = "A test of a very very very long subtitle, but not too long.",
      y = "Just a y-axis label that goes on and on and on and on and on",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit."
      ),
      sources = c("e61", 'ABS')
    )

  p8b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart which is really quite a long title"
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p8a, p8b, spacing = list(pad_width = 3), filename = "plot-multi-wrp-test-25.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p8a, p8b, p8a, p8b,
        spacing = list(pad_width = 3, pad_height = 3),
        filename = "plot-multi-wrp-test-27.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 9 - Adding an overall title -----
  p9a <- minimal_plot +
    labs_e61(
      title = "This is the title of the plot that is just right!!",
      subtitle = "A test of a very very very long subtitle, but not too long.",
      y = "Just a y-axis label that goes on and on and on and on and on",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit."
      ),
      sources = c("e61", 'ABS')
    )

  p9b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart"
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        labs = list(title = "The is an overal chart title"),
        p9a, p9b,
        filename = "plot-multi-wrp-test-28.svg",
        bg_colour = "grey90", spell_check = FALSE
      )
    ))
  })

  # 10 - Adding an overall subtitle -----

  p10a <- minimal_plot +
    labs_e61(
      title = "This is the title of the plot that is just right!!",
      subtitle = "A test of a very very very long subtitle, but not too long.",
      y = "Just a y-axis label that goes on and on and on and on and on",
      footnotes = paste0(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
        "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
        "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
        "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
        "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
        "in culpa qui officia deserunt mollit."
      ),
      sources = c("e61", 'ABS')
    )

  p10b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart"
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        labs = list(subtitle = "The is a subtitle"),
        p10a, p10b,
        filename = "plot-multi-wrp-test-31.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 11 - Adding an overall footnotes and sources -----
  p11a <- minimal_plot +
    labs_e61(
      title = "This is the title of the plot that is just right!!",
      subtitle = "A test of a very very very long subtitle, but not too long.",
      y = "Just a y-axis label that goes on and on and on and on and on"
    )

  p11b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart"
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        labs = list(
          footnotes = paste0(
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
            "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
            "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
            "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
            "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
            "in culpa qui officia deserunt mollit."
          ),
          sources = c("e61", 'ABS')
        ),
        p11a, p11b,
        filename = "plot-multi-wrp-test-34.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 12 - Adding all aspects -----
  p12a <- minimal_plot +
    labs_e61(
      title = "This is the title of the plot that is just right!!",
      subtitle = "A test of a very very very long subtitle, but not too long.",
      y = "Just a y-axis label that goes on and on and on and on and on"
    )

  p12b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart"
    )

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        labs = list(
          title = "The is an overal chart title",
          subtitle = "The is a subtitle",
          footnotes = paste0(
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
            "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
            "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
            "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
            "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
            "in culpa qui officia deserunt mollit."
          ),
          sources = c("e61", 'ABS')
        ),
        p12a, p12b,
        filename = "plot-multi-wrp-test-37.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 13 - Checking longer titles ----
  # (kept the ncol = 3 variant, since it pairs the most extreme title/subtitle
  # lengths with a narrower per-panel width)

  p13a <- minimal_plot +
    labs_e61(title = "This is the title of the plot that is just too long",
             subtitle = "A test of a very very very long subtitle, but not too long.",
             y = "Just a y-axis label that goes on and on and on and on and on")

  p13b <- bar_chart +
    labs_e61(title = "Title",
             subtitle = "Sub title",
             y = "Y-axis plot of this very complicated chart")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        labs = list(
          title = "This is a very very very very very very very very very very very very very very very very very very very very very long title that really should just be one line",
          subtitle = "A test of a very very very very very very very very very very very very very very very very very very very very very very very very very very very very very  very very very very very very very very very very very very very long subtitle, but that's probably okay because subtitles can be long",
          footnotes = paste0(
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
            "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
            "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
            "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
            "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
            "in culpa qui officia deserunt mollit."
          ),
          sources = c("e61", 'ABS')
        ),
        p13a,
        p13b,
        p13a,
        layout = list(ncol = 3),
        filename = "plot-multi-wrp-test-41.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 14 - Checking all of the above with some additional padding ----
  # (kept the pad_width + pad_height combination, since pad_width alone is
  # already checked in scenario 8 above)

  p14a <- minimal_plot +
    labs_e61(title = "This is the title of the plot that is just too long",
             subtitle = "A test of a very very very long subtitle, but not too too long.",
             y = "Just a y-axis label that goes on and on and on and on and on and on and on and on")

  p14b <- bar_chart +
    labs_e61(title = "Title",
             subtitle = "Sub title",
             y = "Y-axis plot of this very complicated chart")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        labs = list(
          title = "The is an overall chart title that is fairly long",
          subtitle = "The is a subtitle",
          footnotes = paste0(
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
            "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
            "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
            "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
            "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
            "in culpa qui officia deserunt mollit."
          ),
          sources = c("e61", 'ABS')
        ),
        p14a,
        p14b,
        p14a,
        p14b,
        spacing = list(pad_width = 3, pad_height = 5),
        filename = "plot-multi-wrp-test-45.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 15 - Check some interesting plot configurations ----
  # (kept the nrow/ncol combination that isn't just the default arrangement;
  # the larger 2x3 and padded 3x3 variants were dropped as they were the most
  # expensive renders in this file and didn't exercise anything the unit
  # tests at the top of this file and the other retained scenarios don't
  # already cover)

  p15a <- minimal_plot +
    labs_e61(title = "This is the title of the plot that is just too long",
             subtitle = "A test of a very very very long subtitle, but not too too long.",
             y = "Just a y-axis label that goes on and on and on and on and on")

  p15b <- bar_chart +
    labs_e61(title = "Title",
             subtitle = "Sub title",
             y = "Y-axis plot of this very complicated chart")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        labs = list(
          title = "This is a very very very very very very very very very very very very very very very very very very very very very very very very very long title that really should just be one line",
          subtitle = "A test of a very very very very very very very very very very very very very very very very very very very very very very very very very long subtitle, but that's probably okay because subtitles can be long",
          footnotes = paste0(
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
            "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
            "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
            "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
            "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
            "in culpa qui officia deserunt mollit."
          ),
          sources = c("e61", 'ABS')
        ),
        p15a,
        p15b,
        p15a,
        p15b,
        layout = list(nrow = 1, ncol = 4),
        filename = "plot-multi-wrp-test-47.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })
})
