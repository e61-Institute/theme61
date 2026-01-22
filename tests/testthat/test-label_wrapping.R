# Tests of label wrapping of theme61 charts ---------------------------------

test_that("Single-panel graph examples", {

  # 1 - no titles ----
  p1 <- minimal_plot

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p1, filename = "plot-sngle-wrp-test-1.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 2 - Just title ----
  p2 <- minimal_plot + labs_e61(title = "This is a title")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p2, filename = "plot-sngle-wrp-test-2.svg",  bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 3 - Just subtitle ----
  p3 <- minimal_plot + labs_e61(subtitle = "A subtitle")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p3, filename = "plot-sngle-wrp-test-3.svg",  bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 4 - Just y-axis ----
  p4 <- minimal_plot + labs_e61(y = "Just a y-axis label")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p4, filename = "plot-sngle-wrp-test-4.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 5 - Just footnotes ----
  p5 <- minimal_plot +
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
      save_e61(p5, filename = "plot-sngle-wrp-test-5.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 6 - title and footnotes ----
  p6 <- minimal_plot +
    labs_e61(
      title = "This is a title",
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
      save_e61(p6, filename = "plot-sngle-wrp-test-6.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 7 - subtitle and footnotes ----
  p7 <- minimal_plot +
    labs_e61(
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
      save_e61(p7, filename = "plot-sngle-wrp-test-7.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })


  # 8 - All titles ----
  p8 <- minimal_plot +
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
      save_e61(p8, filename = "plot-sngle-wrp-test-8.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })


  # 9 - Long titles and subtitles ----
  p9 <- minimal_plot +
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
      save_e61(p9, filename = "plot-sngle-wrp-test-9.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 10 - Long sources - these should now be wrapped ----
  p10 <- minimal_plot +
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
      save_e61(p10, filename = "plot-sngle-wrp-test-10.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 11 - Titles right on the edge ----
  p11 <- minimal_plot +
    labs_e61(
      title = "This is the title of the plot that is just ........",
      subtitle = "A test of a very very very long subtitle, but not too long......",
      y = "Just a y-axis label that goes on and on and on and on and on",
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
      save_e61(p11, filename = "plot-sngle-wrp-test-11.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 12 - Manual width - check that the wrapping expands ----
  p12 <- minimal_plot +
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
      save_e61(p12, dim = list(width = 20), filename = "plot-sngle-wrp-test-12.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })


  # 13 - Faceted with manual width ----
  facetted_plot <-
    ggplot(data.frame(x = c(0, 1), y = c(0, 1), group = c("A", "B")), aes(x, y)) +
    facet_wrap(vars(group)) +
    geom_point()

  p13 <- facetted_plot +
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
      save_e61(p13, filename = "plot-sngle-wrp-test-13.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  # 14 - Plot with coordinates flipped ----
  bar_chart <- ggplot(data.frame(x = factor(1:10), y = runif(10, 0, 20)), aes(x, y)) +
    geom_col()

  p14 <- bar_chart +
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
      save_e61(p14, filename = "plot-sngle-wrp-test-14.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })
})


# Tests of multi-plots ----

test_that("Multi-panel graph examples", {

  withr::local_seed(42)

  # Graphs to use in the panels

  # Cont-y var with small values from 0-1
  data <- data.frame(x = factor(1:10), y = runif(10, 0, 1))

  bar_chart <- ggplot(data, aes(x, y)) +
    geom_col()


  # 1 - Just plots 2x1, 3x1, 2x2 ----
  p1a <- minimal_plot
  p1b <- bar_chart

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p1a, p1b, filename = "plot-multi-wrp-test-1.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p1a, p1b, p1a,
        nrow = 1,
        ncol = 3,
        filename = "plot-multi-wrp-test-2.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p1a, p1b, p1a, p1b,
        filename = "plot-multi-wrp-test-3.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 2 - Titles ----
  p2a <- minimal_plot + labs_e61(title = "This is a title")

  p2b <- bar_chart + labs_e61(title = "This is a title")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p2a, p2b, filename = "plot-multi-wrp-test-4.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p2a, p2b, p2a,
        ncol = 3,
        filename = "plot-multi-wrp-test-5.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p2a, p2b, p2a, p2b,
        filename = "plot-multi-wrp-test-6.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 3 - Subtitles ----
  p3a <- minimal_plot + labs_e61(subtitle = "This is a subtitle")

  p3b <- bar_chart + labs_e61(subtitle = "This is a subtitle")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p3a, p3b, filename = "plot-multi-wrp-test-7.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p3a, p3b, p3a,
        ncol = 3,
        filename = "plot-multi-wrp-test-8.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p3a, p3b, p3a, p3b,
        filename = "plot-multi-wrp-test-9.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 4 - Y-axis titles ----
  p4a <- minimal_plot + labs_e61(y = "This is a y-axis")

  p4b <- bar_chart + labs_e61(y = "This is a y-axis")

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(p4a, p4b, filename = "plot-multi-wrp-test-10.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p4a, p4b, p4a,
        ncol = 3,
        filename = "plot-multi-wrp-test-11.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        p4a, p4b, p4a, p4b,
        filename = "plot-multi-wrp-test-12.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 5 - Footnotes and sources ----
  p5a <- minimal_plot +
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

  p5b <- bar_chart +
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
      save_e61(p5a, p5b, filename = "plot-multi-wrp-test-13.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p5a, p5b, p5a,
        ncol = 3,
        filename = "plot-multi-wrp-test-14.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p5a, p5b, p5a, p5b,
        filename = "plot-multi-wrp-test-15.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 6 - Full labels ----
  p6a <- minimal_plot +
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

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(p6a, p6b, filename = "plot-multi-wrp-test-16.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p6a, p6b, p6a,
        ncol = 3,
        filename = "plot-multi-wrp-test-17.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p6a, p6b, p6a, p6b,
        filename = "plot-multi-wrp-test-18.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 7 - Full long labels ----
  p7a <- minimal_plot +
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

  p7b <- bar_chart +
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

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(p7a, p7b, filename = "plot-multi-wrp-test-19.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p7a, p7b, p7a,
        ncol = 3,
        filename = "plot-multi-wrp-test-20.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p7a, p7b, p7a, p7b,
        filename = "plot-multi-wrp-test-21.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 8 - Full labels just long enough ----
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
      y = "Y-axis plot of this very complicated chart"
    )

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(p8a, p8b, filename = "plot-multi-wrp-test-22.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p8a, p8b, p8a,
        ncol = 3,
        filename = "plot-multi-wrp-test-23.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p8a, p8b, p8a, p8b,
        filename = "plot-multi-wrp-test-24.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 9 - Add padding ----
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
      y = "Y-axis plot of this very complicated chart which is really quite a long title"
    )

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(p9a, p9b, pad_width = 3, filename = "plot-multi-wrp-test-25.svg", bg_colour = "grey90", spell_check = FALSE)
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p9a, p9b, p9a,
        ncol = 3,
        pad_width = 3,
        filename = "plot-multi-wrp-test-26.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        p9a, p9b, p9a, p9b,
        pad_width = 3,
        pad_height = 3,
        filename = "plot-multi-wrp-test-27.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 10 - Adding an overall title -----
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

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        title = "The is an overal chart title",
        p10a, p10b,
        filename = "plot-multi-wrp-test-28.svg",
        bg_colour = "grey90", spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        title = "The is an overal chart title",
        p10a, p10b, p10a,
        ncol = 3,
        filename = "plot-multi-wrp-test-29.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        title = "The is an overal chart title",
        p10a, p10b, p10a, p10b,
        filename = "plot-multi-wrp-test-30.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 11 - Adding an overall subtitle -----

  p11a <- minimal_plot +
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

  p11b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart"
    )

  # save the charts
  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        subtitle = "The is a subtitle",
        p11a, p11b,
        filename = "plot-multi-wrp-test-31.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        subtitle = "The is a subtitle",
        p11a, p11b, p11a,
        ncol = 3,
        filename = "plot-multi-wrp-test-32.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        subtitle = "The is a subtitle",
        p11a, p11b, p11a, p11b,
        filename = "plot-multi-wrp-test-33.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 12 - Adding an overall footnotes and sources -----
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

  # save the charts
  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        footnotes = paste0(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
          "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
          "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
          "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
          "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
          "in culpa qui officia deserunt mollit."
        ),
        sources = c("e61", 'ABS'),
        p12a, p12b,
        filename = "plot-multi-wrp-test-34.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        footnotes = paste0(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
          "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
          "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
          "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
          "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
          "in culpa qui officia deserunt mollit."
        ),
        sources = c("e61", 'ABS'),
        p12a, p12b, p12a,
        ncol = 3,
        filename = "plot-multi-wrp-test-35.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
        footnotes = paste0(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
          "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
          "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
          "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
          "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
          "in culpa qui officia deserunt mollit."
        ),
        sources = c("e61", 'ABS'),
        p12a, p12b, p12a, p12b,
        filename = "plot-multi-wrp-test-36.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 13 - Adding all aspects -----
  p13a <- minimal_plot +
    labs_e61(
      title = "This is the title of the plot that is just right!!",
      subtitle = "A test of a very very very long subtitle, but not too long.",
      y = "Just a y-axis label that goes on and on and on and on and on"
    )

  p13b <- bar_chart +
    labs_e61(
      title = "Title",
      subtitle = "Sub title",
      y = "Y-axis plot of this very complicated chart"
    )

  # save the charts
  withr::with_tempdir({expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p13a, p13b,
        filename = "plot-multi-wrp-test-37.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p13a,
        p13b,
        p13a,
        ncol = 3,
        filename = "plot-multi-wrp-test-38.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p13a,
        p13b,
        p13a,
        p13b,
        filename = "plot-multi-wrp-test-39.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 14 - Checking longer titles ----

  p14a <- minimal_plot +
    labs_e61(title = "This is the title of the plot that is just too long",
             subtitle = "A test of a very very very long subtitle, but not too long.",
             y = "Just a y-axis label that goes on and on and on and on and on")

  p14b <- bar_chart +
    labs_e61(title = "Title",
             subtitle = "Sub title",
             y = "Y-axis plot of this very complicated chart")

  # save the charts
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        title = "This is a very very very very very long title that really should just be one line",
        subtitle = "A test of a very very very very very long subtitle, but that's probably okay because subtitles can be long",
        footnotes = paste0(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
          "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
          "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
          "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
          "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
          "in culpa qui officia deserunt mollit."
        ),
        sources = c("e61", 'ABS'),
        p14a,
        p14b,
        filename = "plot-multi-wrp-test-40.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p14a,
        p14b,
        p14a,
        ncol = 3,
        filename = "plot-multi-wrp-test-41.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p14a,
        p14b,
        p14a,
        p14b,
        filename = "plot-multi-wrp-test-42.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  # 15 - Checking all of the above with some additional padding ----

  p15a <- minimal_plot +
    labs_e61(title = "This is the title of the plot that is just too long",
             subtitle = "A test of a very very very long subtitle, but not too too long.",
             y = "Just a y-axis label that goes on and on and on and on and on and on and on and on")

  p15b <- bar_chart +
    labs_e61(title = "Title",
             subtitle = "Sub title",
             y = "Y-axis plot of this very complicated chart")

  # save the charts
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
        title = "This is a very very very very very long title that really should just be one line",
        subtitle = "A test of a very very very very very long subtitle, but that's probably okay because subtitles can be long",
        footnotes = paste0(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor ",
          "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis ",
          "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ",
          "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore ",
          "eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt ",
          "in culpa qui officia deserunt mollit."
        ),
        sources = c("e61", 'ABS'),
        p15a,
        p15b,
        pad_width = 10,
        filename = "plot-multi-wrp-test-43.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p15a,
        p15b,
        p15a,
        pad_width = 10,
        ncol = 3,
        filename = "plot-multi-wrp-test-44.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p15a,
        p15b,
        p15a,
        p15b,
        pad_width = 3,
        pad_height = 5,
        filename = "plot-multi-wrp-test-45.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p15a,
        p15b,
        p15a,
        p15b,
        pad_height = 5,
        filename = "plot-multi-wrp-test-46.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })


  # 16 - Check some interesting plot configurations ----

  p16a <- minimal_plot +
    labs_e61(title = "This is the title of the plot that is just too long",
             subtitle = "A test of a very very very long subtitle, but not too too long.",
             y = "Just a y-axis label that goes on and on and on and on and on")

  p16b <- bar_chart +
    labs_e61(title = "Title",
             subtitle = "Sub title",
             y = "Y-axis plot of this very complicated chart")

  # save the charts
  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p16a,
        p16b,
        p16a,
        p16b,
        nrow = 1,
        ncol = 4,
        filename = "plot-multi-wrp-test-47.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p16a,
        p16b,
        p16a,
        p16b,
        p16a,
        p16b,
        nrow = 2,
        ncol = 3,
        filename = "plot-multi-wrp-test-48.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })

  withr::with_tempdir({
    expect_snapshot_file(suppressWarnings(
      save_e61(
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
        sources = c("e61", 'ABS'),
        p16a,
        p16b,
        p16a,
        p16b,
        p16a,
        p16b,
        p16a,
        p16b,
        p16a,
        nrow = 3,
        ncol = 3,
        pad_height = 3,
        pad_width = 3,
        filename = "plot-multi-wrp-test-49.svg",
        bg_colour = "grey90",
        spell_check = FALSE
      )
    ))
  })
})
