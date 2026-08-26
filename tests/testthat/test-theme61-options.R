test_that("theme61.auto_theme controls automatic theme_e61() application", {
  withr::local_options(list(theme61.auto_theme = TRUE))

  p_on <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_point()
  expect_true(inherits(p_on$theme, "theme"))

  withr::local_options(list(theme61.auto_theme = FALSE))

  p_off <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_point()
  expect_true(is.null(p_off$theme) || length(p_off$theme) == 0)
})

test_that("set_t61_options accepts theme61.auto_theme as a valid option", {
  withr::defer(options(theme61.auto_theme = TRUE))

  set_t61_options(list(theme61.auto_theme = FALSE))
  expect_false(getOption("theme61.auto_theme"))
})

test_that("theme61.iterate_mode skips auto_theme even when auto_theme is TRUE", {
  withr::local_options(list(theme61.auto_theme = TRUE, theme61.iterate_mode = TRUE))

  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_point()
  expect_true(is.null(p$theme) || length(p$theme) == 0)
})

test_that("theme61.iterate_mode skips automatic scale injection at build time", {
  withr::local_options(list(theme61.iterate_mode = FALSE))

  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_point()

  built_normal <- ggplot2::ggplot_build(p)
  ys_normal <- built_normal$plot@scales$get_scales("y")
  expect_true(inherits(ys_normal, "scale_e61"))

  withr::local_options(list(theme61.iterate_mode = TRUE))

  built_iter <- ggplot2::ggplot_build(p)
  ys_iter <- built_iter$plot@scales$get_scales("y")
  expect_true(is.null(ys_iter) || !inherits(ys_iter, "scale_e61"))
})

test_that("explicit theme61 functions still apply in iterate_mode", {
  withr::local_options(list(theme61.iterate_mode = TRUE))

  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_point() +
    scale_y_continuous_e61() +
    theme_e61()

  expect_true(inherits(p$theme, "theme"))

  built <- ggplot2::ggplot_build(p)
  ys <- built$plot@scales$get_scales("y")
  expect_true(inherits(ys, "scale_e61"))
})

test_that("set_t61_options accepts theme61.iterate_mode as a valid option", {
  withr::defer(options(theme61.iterate_mode = FALSE))

  set_t61_options(list(theme61.iterate_mode = TRUE))
  expect_true(getOption("theme61.iterate_mode"))
})

test_that("labs_e61() skips HTML/markdown subtitle styling in iterate_mode", {
  withr::local_options(list(theme61.iterate_mode = FALSE))

  l_normal <- labs_e61(subtitle = "hi")
  expect_match(l_normal$subtitle, "^<span", fixed = FALSE)

  withr::local_options(list(theme61.iterate_mode = TRUE))

  l_iter <- labs_e61(subtitle = "hi")
  expect_identical(l_iter$subtitle, "hi")
})

test_that("theme61.disable_spellcheck suppresses save_e61()'s spell-checker", {
  withr::local_options(list(theme61.disable_spellcheck = FALSE))

  p <- minimal_plot + labs_e61(title = "Thsi has a typo")

  withr::with_tempdir({
    expect_message(save_e61("spell-on.svg", p), "typo")
  })

  withr::local_options(list(theme61.disable_spellcheck = TRUE))

  withr::with_tempdir({
    expect_no_message(save_e61("spell-off.svg", p))
  })
})

test_that("set_t61_options accepts theme61.disable_spellcheck as a valid option", {
  withr::defer(options(theme61.disable_spellcheck = FALSE))

  set_t61_options(list(theme61.disable_spellcheck = TRUE))
  expect_true(getOption("theme61.disable_spellcheck"))
})

test_that("set_t61_options accepts theme61.max_discrete_colours and theme61.max_discrete_fills as valid options", {
  withr::defer(options(theme61.max_discrete_colours = NULL, theme61.max_discrete_fills = NULL))

  set_t61_options(list(theme61.max_discrete_colours = 20L, theme61.max_discrete_fills = 20L))
  expect_equal(getOption("theme61.max_discrete_colours"), 20L)
  expect_equal(getOption("theme61.max_discrete_fills"), 20L)
})

test_that("set_t61_options() validates against the fixed set of theme61 options, not whichever are currently set", {
  # Simulate theme61 being loaded but not attached (e.g. only
  # theme61::save_e61() used) by temporarily clearing every theme61.*
  # option - set_t61_options() must still accept a documented option name.
  live_opts <- names(options())
  t61_live <- live_opts[grepl("^theme61\\.", live_opts)]
  saved <- options()[t61_live]
  withr::defer(options(saved))

  cleared <- stats::setNames(vector("list", length(t61_live)), t61_live)
  options(cleared)

  withr::defer(options(theme61.base_size = 10))
  expect_no_error(set_t61_options(list(theme61.base_size = 10)))
})

test_that("set_t61_options() error message is well-formed for invalid options", {
  expect_error(
    set_t61_options(list(not_a_real_option = TRUE)),
    "Invalid options supplied: not_a_real_option\\. Valid options are: theme61\\."
  )
})

test_that("labs_e61() leaves the y-axis title as a normal axis title in iterate_mode", {
  withr::local_options(list(theme61.iterate_mode = TRUE))

  # Normally (y_top = TRUE default) the y title gets folded into the
  # subtitle and y is set to NULL - in iterate_mode it should stay a
  # plain y-axis title instead, with no HTML in either.
  l <- labs_e61(subtitle = "hi", y = "Y title")
  expect_identical(l$subtitle, "hi")
  expect_identical(l$y, "Y title")

  # Wrapped y titles should use a plain newline, not "<br>"
  l_wrap <- labs_e61(y = "A really quite long y axis title", ytitle_wrap = 10)
  expect_false(grepl("<br>", l_wrap$y, fixed = TRUE))
})
