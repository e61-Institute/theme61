# Tests of check_for_y_var() (#357) ------------------------------------------

test_that("check_for_y_var() handles a y aesthetic that is an expression, not a plain symbol", {
  df <- data.frame(x = 1:5, value = c(10, 20, 30, 40, 50))
  p <- ggplot(df, aes(x, y = value / 1000)) + geom_point()

  expect_true(theme61:::check_for_y_var(p))
})

test_that("check_for_y_var() still returns TRUE for a plain numeric y symbol", {
  df <- data.frame(x = 1:5, y = c(10, 20, 30, 40, 50))
  p <- ggplot(df, aes(x, y)) + geom_point()

  expect_true(theme61:::check_for_y_var(p))
})

test_that("check_for_y_var() returns FALSE for a factor y aesthetic", {
  df <- data.frame(x = 1:3, grp = factor(c("a", "b", "c")))
  p <- ggplot(df, aes(x, y = grp)) + geom_point()

  expect_false(theme61:::check_for_y_var(p))
})

test_that("check_for_y_var() returns FALSE for a character y aesthetic", {
  df <- data.frame(x = 1:3, grp = c("a", "b", "c"))
  p <- ggplot(df, aes(x, y = grp)) + geom_point()

  expect_false(theme61:::check_for_y_var(p))
})

test_that("check_for_y_var() falls back to checking build data when y isn't in plot-level data (layer-level only)", {
  df <- data.frame(x = 1:5)
  layer_df <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5))

  # y is mapped at plot level but only exists in the layer's own data.
  p <- ggplot(df, aes(x, y = y)) + geom_point(data = layer_df)

  expect_no_error(result <- theme61:::check_for_y_var(p))
  expect_true(result)
})
