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

test_that("set_auto_theme/unset_auto_theme toggle the option", {
  withr::defer(options(theme61.auto_theme = TRUE))

  unset_auto_theme()
  expect_false(getOption("theme61.auto_theme"))

  set_auto_theme()
  expect_true(getOption("theme61.auto_theme"))
})

test_that("set_t61_options accepts theme61.auto_theme as a valid option", {
  withr::defer(options(theme61.auto_theme = TRUE))

  set_t61_options(list(theme61.auto_theme = FALSE))
  expect_false(getOption("theme61.auto_theme"))
})
