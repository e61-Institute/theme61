# Tests of check_for_y_var() (#357) ------------------------------------------
#
# check_for_y_var() used to extract the y variable's name via
# gsub("~", "", deparse(plot@mapping$y)) and index plot@data with that
# string. This broke for anything other than a plain column symbol (e.g. an
# expression like `y = value / 1000`), since plot@data[["value/1000"]] is
# never a real column. It's now evaluated directly via rlang::eval_tidy()
# against plot@data instead.

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

  # y is mapped at the plot level, but the column only exists in the layer's
  # own data - rlang::eval_tidy(plot@mapping$y, plot@data) errors here since
  # "y" isn't found in plot@data or its enclosing scope; this must be caught
  # rather than propagated, falling through to the build-data check below
  # (which does see the layer's y values and correctly returns TRUE).
  p <- ggplot(df, aes(x, y = y)) + geom_point(data = layer_df)

  expect_no_error(result <- theme61:::check_for_y_var(p))
  expect_true(result)
})
