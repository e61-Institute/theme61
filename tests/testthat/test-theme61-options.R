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

test_that("theme61.quiet_mask suppresses the ggsave()/labs() masking messages", {
  withr::local_options(list(theme61.quiet_mask = TRUE))

  expect_no_message(save_e61(withr::local_tempfile(fileext = ".svg"), minimal_plot + labs()))
  expect_no_message(ggsave(withr::local_tempfile(fileext = ".svg"), minimal_plot))
})

test_that("set_t61_options accepts theme61.quiet_mask as a valid option", {
  withr::defer(options(theme61.quiet_mask = FALSE))

  set_t61_options(list(theme61.quiet_mask = TRUE))
  expect_true(getOption("theme61.quiet_mask"))
})
