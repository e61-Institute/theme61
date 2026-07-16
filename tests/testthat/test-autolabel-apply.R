# Tests for wiring the autolabel engine into plot_label()/save_e61()
# (issue #159): does t61_apply_autolabel() find eligible plot_label()
# labels, match them to their series by colour, and move them -- while
# leaving everything out of v1 scope (opted-out, rotated, unmatched,
# facetted) exactly where the user put it?

autolabel_apply_test_setup <- function(auto_position = TRUE) {
  data <- data.frame(
    x = rep(2000:2020, 2),
    y = c(seq(0, 5, length.out = 21), seq(10, 2, length.out = 21)),
    series = rep(c("A", "B"), each = 21)
  )

  p <- ggplot(data, aes(x, y, colour = series)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL) +
    plot_label(
      c("Series A", "Series B"),
      x = c(2005, 2005), y = c(1, 1), # deliberately bad, same spot
      colour = c("#e57200", "#1c3144"),
      auto_position = auto_position
    )

  p
}

test_that("t61_apply_autolabel moves matching labels away from a bad fallback", {
  skip_on_cran()

  p <- autolabel_apply_test_setup()
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  # Moved away from the shared, deliberately bad fallback position
  expect_false(isTRUE(all.equal(d$x[1], 2005)) && isTRUE(all.equal(d$y[1], 1)))
  expect_false(isTRUE(all.equal(d$x[2], 2005)) && isTRUE(all.equal(d$y[2], 1)))

  # Each label should have moved close to its own series' colour-matched line
  expect_lt(abs(d$y[1] - 2.5), 3) # series A ranges 0-5
  expect_lt(abs(d$y[2] - 6), 5)   # series B ranges 2-10
})

test_that("t61_apply_autolabel leaves auto_position = FALSE labels untouched", {
  skip_on_cran()

  p <- autolabel_apply_test_setup(auto_position = FALSE)
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, c(2005, 2005))
  expect_equal(d$y, c(1, 1))
})

test_that("t61_apply_autolabel skips rotated labels (v1 scope: angle = 0 only)", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2005, y = 1, colour = "#e57200", angle = 45)

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, 2005)
  expect_equal(d$y, 1)
})

test_that("t61_apply_autolabel keeps the fallback when the label colour matches no series", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Unrelated", x = 2005, y = 1, colour = "#123456")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, 2005)
  expect_equal(d$y, 1)
})

test_that("t61_apply_autolabel matches point-geom series too", {
  skip_on_cran()

  data <- data.frame(x = 2000:2010, y = seq(0, 5, length.out = 11))
  p <- ggplot(data, aes(x, y)) +
    geom_point(colour = "#e57200", size = 2) +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2005, y = 1, colour = "#e57200")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(isTRUE(all.equal(d$x, 2005)) && isTRUE(all.equal(d$y, 1)))
})

test_that("t61_apply_autolabel is a no-op when there are no plot_label() layers", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10)

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)
  expect_identical(result, p)
})

test_that("t61_apply_autolabel keeps fallbacks for facetted plots (not v1 scope)", {
  skip_on_cran()

  data <- data.frame(
    x = rep(2000:2010, 2),
    y = seq(0, 5, length.out = 22),
    grp = rep(c("p1", "p2"), each = 11)
  )
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    facet_wrap(~grp) +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2005, y = 1, colour = "#e57200", panel = list(grp = "p1"))

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, 2005)
  expect_equal(d$y, 1)
})

test_that("save_e61() end-to-end repositions eligible plot_label() text", {
  skip_on_cran()

  p <- autolabel_apply_test_setup()

  out <- tempfile(fileext = ".svg")
  expect_no_error(
    save_e61(out, plot = p, preview = TRUE, spell_check = FALSE)
  )
})

test_that("t61_match_label_series matches by colour and returns NULL otherwise", {
  skip_on_cran()

  p <- autolabel_apply_test_setup()
  built_data <- ggplot2::ggplot_build(p)$data

  match_a <- t61_match_label_series(p@layers, built_data, "#e57200")
  expect_equal(match_a$geom_type, "line")
  expect_true(all(match_a$x %in% 2000:2020))

  expect_null(t61_match_label_series(p@layers, built_data, "#ffffff"))
  expect_null(t61_match_label_series(p@layers, built_data, "not-a-colour"))
})
