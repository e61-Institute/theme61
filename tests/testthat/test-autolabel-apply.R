# Tests for wiring the autolabel engine into plot_label()/save_e61(): does
# t61_apply_autolabel() find eligible plot_label() labels, match them to
# their series by colour, and move them -- while leaving everything out of
# v1 scope (opted-out, rotated, unmatched, facetted) exactly where the user
# put it?

# Every test needs the resolved plot_label() layer (or just its data) --
# pull it out in one step instead of repeating the same which(vapply(...)) scan.
t61_label_layer <- function(plot) {
  idx <- which(vapply(plot@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  plot@layers[[idx]]
}
t61_label_layer_data <- function(plot) t61_label_layer(plot)$data

autolabel_apply_test_setup <- function(auto_position = TRUE) {
  # 5 points is enough to define each straight line exactly (same
  # endpoints/slope as a denser sample) while keeping the mask render this
  # setup drives in every calling test cheap -- placement only depends on
  # line geometry, not point density.
  x <- seq(2000, 2020, length.out = 5)
  data <- data.frame(
    x = rep(x, 2),
    y = c(seq(0, 5, length.out = 5), seq(10, 2, length.out = 5)),
    series = rep(c("A", "B"), each = 5)
  )

  # x/y are only given for auto_position = FALSE, which requires them --
  # an explicit position always wins outright now (the search never even
  # runs for that label, see t61_autolabel_plot()), so testing "the search
  # finds a good spot" means giving it nothing to fall back on.
  label_args <- if (auto_position) {
    list()
  } else {
    list(x = c(2005, 2005), y = c(1, 1)) # deliberately bad, same spot
  }

  p <- ggplot(data, aes(x, y, colour = series)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10) +
    theme(legend.position = "none") +
    ggplot2::labs(x = NULL, y = NULL) +
    do.call(plot_label, c(
      list(c("Series A", "Series B")),
      label_args,
      list(colour = c("#e57200", "#1c3144"), auto_position = auto_position)
    ))

  p
}

test_that("t61_apply_autolabel finds a good spot near each series when no position is given", {
  skip_on_cran()

  p <- autolabel_apply_test_setup()
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))

  # Each label should have moved close to its own series' colour-matched line
  expect_lt(abs(d$y[1] - 2.5), 3) # series A ranges 0-5
  expect_lt(abs(d$y[2] - 6), 5)   # series B ranges 2-10
})

test_that("t61_apply_autolabel is a complete no-op when theme61.auto_label = FALSE", {
  skip_on_cran()
  withr::local_options(list(theme61.auto_label = FALSE))

  # x/y given explicitly since plot_label() itself now requires them when
  # the option is off (see test-plot_label.R) -- a deliberately bad spot,
  # so any movement at all would mean the engine ran despite the option.
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
    plot_label(
      c("Series A", "Series B"),
      x = c(2005, 2005), y = c(1, 1),
      colour = c("#e57200", "#1c3144")
    )

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  # Same object, not just an equivalent copy -- proves the early return
  # happens before any collection/matching/render work.
  expect_identical(result, p)
})

test_that("t61_apply_autolabel leaves an explicit position untouched even when auto_position = TRUE", {
  skip_on_cran()

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
    plot_label(
      c("Series A", "Series B"),
      x = c(2005, 2005), y = c(1, 1), # deliberately bad, same spot as each other
      colour = c("#e57200", "#1c3144"),
      auto_position = TRUE
    )

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  # An explicit position always wins outright -- the search never runs for
  # it, so it stays exactly where given, however bad a spot that is.
  expect_equal(d$x, c(2005, 2005))
  expect_equal(d$y, c(1, 1))
})

test_that("t61_apply_autolabel doesn't mutate the caller's own plot object", {
  skip_on_cran()

  # Layer data (data.table) and the layer itself (ggproto) both mutate by
  # reference, so writing a resolved position into the returned plot could
  # silently write through to the caller's own copy too.
  p <- autolabel_apply_test_setup()
  before <- data.table::copy(t61_label_layer_data(p))

  invisible(t61_apply_autolabel(p, width_cm = 16, height_cm = 12))

  expect_equal(t61_label_layer_data(p), before)
})

test_that("t61_apply_autolabel leaves auto_position = FALSE labels untouched", {
  skip_on_cran()

  p <- autolabel_apply_test_setup(auto_position = FALSE)
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

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

  d <- t61_label_layer_data(result)

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

  d <- t61_label_layer_data(result)

  expect_equal(d$x, 2005)
  expect_equal(d$y, 1)
})

test_that("t61_apply_autolabel matches point-geom series too", {
  skip_on_cran()

  data <- data.frame(x = 2000:2010, y = seq(0, 5, length.out = 11))
  p <- ggplot(data, aes(x, y)) +
    geom_point(colour = "#e57200", size = 2) +
    theme_bw(base_size = 10) +
    plot_label("Series A", colour = "#e57200")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))
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

  d <- t61_label_layer_data(result)

  expect_equal(d$x, 2005)
  expect_equal(d$y, 1)
})

test_that("save_e61() end-to-end repositions eligible plot_label() text", {
  skip_on_cran()

  p <- autolabel_apply_test_setup()

  out <- tempfile(fileext = ".svg")
  expect_no_error(
    suppressMessages(save_e61(out, plot = p, preview = TRUE, spell_check = FALSE))
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

test_that("t61_match_label_series matches a column series by fill, position-adjusted", {
  skip_on_cran()

  data <- data.frame(
    year = rep(2015:2017, 2),
    value = c(1, 2, 3, 4, 5, 6),
    series = rep(c("A", "B"), each = 3)
  )
  p <- ggplot(data, aes(year, value, fill = series)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10)

  built_data <- ggplot2::ggplot_build(p)$data
  match_a <- t61_match_label_series(p@layers, built_data, "#e57200")

  expect_equal(match_a$geom_type, "column")
  expect_length(match_a$xmin, 3)
  expect_true(all(match_a$xmax > match_a$xmin))
})

test_that("t61_match_label_series matches an area series by fill and carries alpha", {
  skip_on_cran()

  data <- data.frame(year = 2000:2010, value = seq(1, 21, 2))
  p <- ggplot(data, aes(year, value)) +
    geom_area(fill = "#e57200", alpha = 0.5) +
    theme_bw(base_size = 10)

  built_data <- ggplot2::ggplot_build(p)$data
  match_a <- t61_match_label_series(p@layers, built_data, "#e57200")

  expect_equal(match_a$geom_type, "area")
  expect_equal(match_a$fill, "#e57200")
  expect_equal(match_a$alpha, 0.5)
  expect_length(match_a$ymax, 11)
})

test_that("t61_match_label_series matches geom_pointbar() by colour, ahead of its point layer", {
  skip_on_cran()

  data <- data.frame(
    x = 1:5, y = c(2, 3, 1, 4, 3),
    ymin = c(1.5, 2.2, 0.5, 3.1, 2.3), ymax = c(2.5, 3.8, 1.5, 4.9, 3.7)
  )
  p <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(colour = "#e57200") +
    theme_bw(base_size = 10)

  built_data <- ggplot2::ggplot_build(p)$data
  match_a <- t61_match_label_series(p@layers, built_data, "#e57200")

  expect_equal(match_a$geom_type, "pointbar")
  expect_equal(match_a$ymin, data$ymin)
  expect_equal(match_a$ymax, data$ymax)
})

test_that("t61_apply_autolabel repositions a column label clear of every bar", {
  skip_on_cran()

  data <- data.frame(
    year = rep(2015:2024, 2),
    value = c(seq(10, 19), seq(15, 24)),
    series = rep(c("A", "B"), each = 10)
  )
  p <- ggplot(data, aes(year, value, fill = series)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(A = "#e57200", B = "#1c3144")) +
    theme_bw(base_size = 10) +
    plot_label(c("A", "B"), colour = c("#e57200", "#1c3144"))

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))

  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm_a <- t61_measure_label_cm("A", size_mm = 3.5, width_cm = 16, height_cm = 12)
  cm_b <- t61_measure_label_cm("B", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box_a <- t61_text_box_px(d$x[1], d$y[1], cm_a, mask, hjust = 0)
  box_b <- t61_text_box_px(d$x[2], d$y[2], cm_b, mask, hjust = 0)

  expect_false(t61_test_collision(mask$occupancy, box_a$row_range, box_a$col_range))
  expect_false(t61_test_collision(mask$occupancy, box_b$row_range, box_b$col_range))
})

test_that("t61_apply_autolabel places an area label inside its band with a contrasting colour", {
  skip_on_cran()

  data <- data.frame(year = 2000:2020, value = seq(20, 40))
  p <- ggplot(data, aes(year, value)) +
    geom_area(fill = "#1c3144", alpha = 1) + # a dark fill -> expect white text
    theme_bw(base_size = 10) +
    plot_label("Series A", colour = "#1c3144")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))
  expect_gt(d$y, 0) # placed inside the visible (growing) band

  expect_equal(t61_label_layer(result)$aes_params$colour, "white")
})

test_that("t61_apply_autolabel falls back to edge-hugging line-style placement when an area's band is too narrow everywhere", {
  skip_on_cran()

  # Same narrow-band shape as t61_place_label_area()'s own unit test
  # (test-autolabel-area.R), but drives the orchestrator instead, to
  # confirm it falls through to the edge-hugging line-style search rather
  # than skipping straight to the "any empty space" tier.
  data <- data.frame(x = 0:20, y = rep(0.05, 21))
  p <- ggplot(data, aes(x, y)) +
    geom_area(fill = "#e57200") +
    theme_bw(base_size = 10) +
    coord_cartesian(ylim = c(0, 10)) +
    plot_label("Series A", colour = "#e57200")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))

  # Only the inside-the-band placement overrides to a contrast colour --
  # staying at the original colour confirms the fallback ran instead.
  expect_equal(t61_label_layer(result)$aes_params$colour, "#e57200")

  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Series A", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box <- t61_text_box_px(d$x, d$y, cm, mask, hjust = 0)
  expect_false(t61_test_collision(mask$occupancy, box$row_range, box$col_range))
})

test_that("t61_apply_autolabel repositions a geom_pointbar() label clear of the error bars", {
  skip_on_cran()

  data <- data.frame(x = 2000:2010, y = seq(0, 5, length.out = 11))
  data$ymin <- data$y - 1
  data$ymax <- data$y + 1

  p <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(colour = "#e57200") +
    theme_bw(base_size = 10) +
    plot_label("Series A", colour = "#e57200")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))

  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Series A", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box <- t61_text_box_px(d$x, d$y, cm, mask, hjust = 0)

  expect_false(t61_test_collision(mask$occupancy, box$row_range, box$col_range))
})

# x/y are optional when auto_position = TRUE: fallback order is (1) a good
# spot from the placement algorithm, (2) the caller's own x/y, (3) any
# empty space. Tests below cover (1)/(3) end-to-end and (2)'s priority.

test_that("t61_apply_autolabel resolves a real position with no x/y supplied at all", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Series A", colour = "#e57200")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  expect_false(is.na(d$x))
  expect_false(is.na(d$y))
})

test_that("t61_apply_autolabel still resolves a position when no series matches and no x/y was supplied", {
  skip_on_cran()
  withr::local_options(list(theme61.autolabel_fallback_msg = FALSE))

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Unrelated", colour = "#123456") # matches no series, and no x/y given

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  # Not NA (there's no fallback to keep any more) and shouldn't collide
  # with the line.
  expect_false(is.na(d$x))
  expect_false(is.na(d$y))

  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Unrelated", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box <- t61_text_box_px(d$x, d$y, cm, mask, hjust = 0)
  expect_false(t61_test_collision(mask$occupancy, box$row_range, box$col_range))
})

test_that("t61_apply_autolabel prefers the caller's own x/y over random empty space", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2019, y = 4.5, colour = "#e57200")

  # An explicit position always wins outright -- the scored search never
  # runs for it. Mocked (rather than e.g. stop()) since t61_apply_autolabel()
  # swallows errors, which would otherwise mask a wrongly-run search as a
  # silent "keep the original plot" -- identical to what's asserted below.
  search_ran <- FALSE
  testthat::local_mocked_bindings(t61_place_label = function(...) { search_ran <<- TRUE; NULL })

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  expect_false(search_ran)

  d <- t61_label_layer_data(result)

  expect_equal(d$x, 2019)
  expect_equal(d$y, 4.5)
})

test_that("t61_apply_autolabel(fast = TRUE) resolves a position without rendering a mask", {
  skip_on_cran()
  withr::local_options(list(theme61.autolabel_fast_msg = FALSE))

  p <- autolabel_apply_test_setup() # no x/y given (auto_position = TRUE default)

  render_calls <- 0
  testthat::local_mocked_bindings(t61_render_mask = function(...) { render_calls <<- render_calls + 1; NULL })

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12, fast = TRUE)

  expect_equal(render_calls, 0)

  d <- t61_label_layer_data(result)

  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))
})

test_that("save_e61(fast_labels = TRUE) skips the search but still resolves a position", {
  skip_on_cran()
  withr::local_options(list(theme61.autolabel_fast_msg = FALSE))

  p <- autolabel_apply_test_setup() # no x/y given (auto_position = TRUE default)

  sv_fast <- theme61:::save_single(
    filename = NULL, plot = p, chart_type = "normal", auto_scale = TRUE,
    width = NULL, height = NULL, max_height = NULL, format = "svg", base_size = 10,
    pad_width = 0, pad_height = 0, bg_colour = "white", fast_labels = TRUE
  )
  d_fast <- t61_label_layer_data(sv_fast$graph)

  expect_false(anyNA(d_fast$x)); expect_false(anyNA(d_fast$y))

  # fast_labels = FALSE runs the real scored search, so should generally
  # land somewhere different from the fixed cheap offset above.
  sv_slow <- theme61:::save_single(
    filename = NULL, plot = p, chart_type = "normal", auto_scale = TRUE,
    width = NULL, height = NULL, max_height = NULL, format = "svg", base_size = 10,
    pad_width = 0, pad_height = 0, bg_colour = "white", fast_labels = FALSE
  )
  d_slow <- t61_label_layer_data(sv_slow$graph)

  expect_false(isTRUE(all.equal(d_fast$y, d_slow$y)))
})

test_that("save_e61(preview = TRUE, fast_labels = TRUE) doesn't crash on a steeply diverging line chart", {
  skip_on_cran()
  withr::local_options(list(theme61.autolabel_fast_msg = FALSE))

  # Two lines that pull apart -- an unclamped fast-mode offset could push
  # a label's y beyond the already-fixed y-axis limits, erroring at
  # render time instead of just rendering imprecisely (see
  # t61_place_label_fast()'s clamping).
  set.seed(1)
  data1 <- data.frame(
    year = rep(2005:2024, 2),
    value = c(100 + cumsum(rnorm(20, 1.5, 1)), 100 + cumsum(rnorm(20, 3.5, 1))),
    series = rep(c("Wages", "Productivity"), each = 20)
  )
  cols1 <- c(Wages = "#e57200", Productivity = "#1c3144")

  p <- ggplot(data1, aes(year, value, colour = series)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = cols1) +
    theme_e61() +
    labs_e61(title = "Diverging series") +
    plot_label(c("Wages", "Productivity"), colour = unname(cols1))

  expect_no_error(
    suppressWarnings(suppressMessages(
      save_e61(plot = p, preview = TRUE, format = "svg", auto_scale = TRUE, fast_labels = TRUE)
    ))
  )
})

test_that("t61_apply_autolabel(fast = TRUE) shows the fast-preview reminder, only when a label needed it", {
  skip_on_cran()

  p <- autolabel_apply_test_setup() # no x/y given (auto_position = TRUE default)

  # theme61.autolabel_fast_msg = TRUE shows it every time.
  withr::local_options(list(theme61.autolabel_fast_msg = TRUE))
  expect_message(
    t61_apply_autolabel(p, width_cm = 16, height_cm = 12, fast = TRUE),
    "save_e61"
  )
  expect_message(
    t61_apply_autolabel(p, width_cm = 16, height_cm = 12, fast = TRUE),
    "save_e61"
  )

  # fast = FALSE runs the real search, so there's nothing to warn about.
  expect_no_message(t61_apply_autolabel(p, width_cm = 16, height_cm = 12, fast = FALSE))

  # Every label already has an explicit position: fast placement is never
  # actually used, so the reminder would have nothing to explain.
  p_explicit <- autolabel_apply_test_setup(auto_position = FALSE)
  expect_no_message(t61_apply_autolabel(p_explicit, width_cm = 16, height_cm = 12, fast = TRUE))

  # theme61.autolabel_fast_msg = FALSE opts out entirely.
  withr::local_options(list(theme61.autolabel_fast_msg = FALSE))
  expect_no_message(t61_apply_autolabel(p, width_cm = 16, height_cm = 12, fast = TRUE))
})

test_that("t61_apply_autolabel(fast = TRUE) reminder defaults to a 30-minute cooldown", {
  skip_on_cran()

  p <- autolabel_apply_test_setup() # no x/y given (auto_position = TRUE default)

  withr::local_options(list(theme61.autolabel_fast_msg = NULL))
  t61_env <- theme61:::t61_env
  clear_last_shown <- function() {
    if (exists("theme61.autolabel_fast_msg_last_shown", envir = t61_env, inherits = FALSE)) {
      rm(list = "theme61.autolabel_fast_msg_last_shown", envir = t61_env, inherits = FALSE)
    }
  }
  clear_last_shown()
  withr::defer(clear_last_shown())

  expect_message(
    t61_apply_autolabel(p, width_cm = 16, height_cm = 12, fast = TRUE),
    "save_e61"
  )
  # Same session, well within the cooldown window: stays quiet.
  expect_no_message(t61_apply_autolabel(p, width_cm = 16, height_cm = 12, fast = TRUE))

  # Backdate the last-shown time past the cooldown window: fires again.
  t61_env$theme61.autolabel_fast_msg_last_shown <- Sys.time() - 31 * 60
  expect_message(
    t61_apply_autolabel(p, width_cm = 16, height_cm = 12, fast = TRUE),
    "save_e61"
  )
})

test_that("t61_apply_autolabel informs every time a label settles for a fallback position instead of a real placement", {
  skip_on_cran()

  # coord_flip() + pointbar isn't flip-aware, so it bypasses the series
  # match and falls through to a fallback position.
  data <- data.frame(x = 2000:2010, y = seq(0, 5, length.out = 11))
  data$ymin <- data$y - 1
  data$ymax <- data$y + 1
  p_bypass <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(colour = "#e57200") +
    coord_flip() +
    theme_bw(base_size = 10) +
    plot_label("Series A", colour = "#e57200")

  # Collapse whitespace since cli may soft-wrap the message across lines.
  withr::local_options(list(theme61.autolabel_fallback_msg = TRUE))
  w <- testthat::capture_warnings(t61_apply_autolabel(p_bypass, width_cm = 16, height_cm = 12))
  expect_length(w, 1)
  msg <- gsub("\\s+", " ", w)
  expect_match(msg, "Series A", fixed = TRUE)
  expect_match(msg, "not yet supported for coord_flip() + area/pointbar", fixed = TRUE)

  # Fires every time, not just once per session.
  w_again <- testthat::capture_warnings(t61_apply_autolabel(p_bypass, width_cm = 16, height_cm = 12))
  expect_length(w_again, 1)

  # An unmatched label with no x/y falls back for a different reason.
  data2 <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p_unmatched <- ggplot(data2, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Unrelated", colour = "#123456")

  w2 <- testthat::capture_warnings(t61_apply_autolabel(p_unmatched, width_cm = 16, height_cm = 12))
  expect_length(w2, 1)
  msg2 <- gsub("\\s+", " ", w2)
  expect_match(msg2, "Unrelated", fixed = TRUE)
  expect_match(msg2, "no good spot found, used a fallback position", fixed = TRUE)

  # theme61.autolabel_fallback_msg = FALSE opts out entirely.
  withr::local_options(list(theme61.autolabel_fallback_msg = FALSE))
  expect_no_warning(t61_apply_autolabel(p_bypass, width_cm = 16, height_cm = 12))
  expect_no_warning(t61_apply_autolabel(p_unmatched, width_cm = 16, height_cm = 12))
})

test_that("t61_apply_autolabel informs on a fallback position under fast = TRUE too", {
  skip_on_cran()

  # No series matches this label's colour, and fast mode has no mask to
  # fall back to a collision-free spot with -- it settles for the panel
  # centre, which should still be reported as a degraded placement.
  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p_unmatched <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Unrelated", colour = "#123456")

  withr::local_options(list(theme61.autolabel_fallback_msg = TRUE, theme61.autolabel_fast_msg = FALSE))
  w <- testthat::capture_warnings(t61_apply_autolabel(p_unmatched, width_cm = 16, height_cm = 12, fast = TRUE))
  expect_length(w, 1)
  msg <- gsub("\\s+", " ", w)
  expect_match(msg, "Unrelated", fixed = TRUE)
  expect_match(msg, "no good spot found, used a fallback position", fixed = TRUE)
})

coord_flip_apply_test <- function(p) {
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)

  expect_false(anyNA(d$x))
  expect_false(anyNA(d$y))

  # d$x/d$y are stored in pre-flip data space -- convert to the mask's
  # screen space before box math, same as t61_autolabel_plot() itself does.
  labelled_mask <- t61_render_mask(result, width_cm = 16, height_cm = 12)
  for (i in seq_len(nrow(d))) {
    lab_cm <- t61_measure_label_cm(d$label[i], size_mm = 3.5, width_cm = 16, height_cm = 12)
    screen_xy <- t61_flip_xy(d$x[i], d$y[i])
    box <- t61_text_box_px(screen_xy$x, screen_xy$y, lab_cm, labelled_mask, hjust = 0)
    expect_true(t61_box_in_bounds(box$row_range, box$col_range, labelled_mask))
  }
}

test_that("t61_apply_autolabel auto-positions labels on a coord_flip() line chart", {
  skip_on_cran()

  data <- data.frame(
    category = rep(c("A", "B", "C"), 2),
    value = c(5, 8, 3, 6, 2, 9),
    series = rep(c("X", "Y"), each = 3)
  )
  cols <- c(X = "#e57200", Y = "#1c3144")

  p <- ggplot(data, aes(category, value, colour = series, group = series)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = cols) +
    coord_flip() +
    theme_bw(base_size = 10) +
    plot_label(c("X", "Y"), colour = unname(cols))

  coord_flip_apply_test(p)
})

test_that("t61_apply_autolabel auto-positions labels on a coord_flip() column chart, clear of every bar", {
  skip_on_cran()

  # Checks placement lands in-bounds and clear of bar ink, same as the
  # unflipped "repositions a column label clear of every bar" test.
  data <- data.frame(
    category = rep(c("A", "B", "C", "D", "E"), 2),
    value = c(5, 8, 3, 9, 4, 6, 2, 9, 5, 7),
    series = rep(c("X", "Y"), each = 5)
  )
  cols <- c(X = "#e57200", Y = "#1c3144")

  p <- ggplot(data, aes(category, value, fill = series)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = cols) +
    coord_flip() +
    theme_bw(base_size = 10) +
    plot_label(c("X", "Y"), colour = unname(cols))

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  d <- t61_label_layer_data(result)
  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))

  # d$x/d$y are pre-flip data space; convert to the mask's screen space
  # (see t61_flip_xy()) before the collision check.
  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm_x <- t61_measure_label_cm("X", size_mm = 3.5, width_cm = 16, height_cm = 12)
  cm_y <- t61_measure_label_cm("Y", size_mm = 3.5, width_cm = 16, height_cm = 12)
  screen_x <- t61_flip_xy(d$x[1], d$y[1])
  screen_y <- t61_flip_xy(d$x[2], d$y[2])
  box_x <- t61_text_box_px(screen_x$x, screen_x$y, cm_x, mask, hjust = 0)
  box_y <- t61_text_box_px(screen_y$x, screen_y$y, cm_y, mask, hjust = 0)

  expect_true(t61_box_in_bounds(box_x$row_range, box_x$col_range, mask))
  expect_true(t61_box_in_bounds(box_y$row_range, box_y$col_range, mask))
  expect_false(t61_test_collision(mask$occupancy, box_x$row_range, box_x$col_range))
  expect_false(t61_test_collision(mask$occupancy, box_y$row_range, box_y$col_range))
})

test_that("t61_apply_autolabel falls back (doesn't crash or misplace) for a geom_pointbar() label under coord_flip()", {
  skip_on_cran()
  withr::local_options(list(theme61.autolabel_fallback_msg = TRUE))

  # geom_pointbar()'s error-bar orientation isn't flip-aware -- area/pointbar
  # are treated as unmatched under a flip (see t61_autolabel_plot()'s docs),
  # skipping straight to the fallback tiers instead of erroring or scoring
  # against the error bars as if still vertical.
  data <- data.frame(x = 2000:2010, y = seq(0, 5, length.out = 11))
  data$ymin <- data$y - 1
  data$ymax <- data$y + 1

  p <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(colour = "#e57200") +
    coord_flip() +
    theme_bw(base_size = 10) +
    plot_label("Series A", colour = "#e57200")

  # If the bail-out didn't happen, t61_place_label() would be called with
  # geom_type = "pointbar" -- asserting it's never called proves the
  # series really was invalidated, not that some other tier just won.
  search_ran <- FALSE
  testthat::local_mocked_bindings(t61_place_label = function(...) { search_ran <<- TRUE; NULL })

  result <- NULL
  expect_warning(
    result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12),
    "coord_flip"
  )

  expect_false(search_ran)

  d <- t61_label_layer_data(result)
  expect_false(anyNA(d$x)); expect_false(anyNA(d$y))

  # Still expected to land somewhere valid via the fallback tiers (any
  # collision-free spot, or the panel centre as a last resort), not just
  # "not NA".
  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Series A", size_mm = 3.5, width_cm = 16, height_cm = 12)
  screen_xy <- t61_flip_xy(d$x, d$y)
  box <- t61_text_box_px(screen_xy$x, screen_xy$y, cm, mask, hjust = 0)
  expect_true(t61_box_in_bounds(box$row_range, box$col_range, mask))
})

# save_multi() combines independent single-panel plots via patchwork
# rather than save_single(), so it needs its own t61_apply_autolabel()
# call per panel -- these tests exercise that wiring directly.

#' Pull a plot_label() layer's resolved data out of a save_multi() result.
#' Which panel ends up as `graph`'s own layers vs. `graph$patches$plots`
#' isn't part of patchwork's contract, so match panels by comparing each
#' candidate's first layer's rendered y-values against the source plot's.
#' @noRd
multi_panel_label_data <- function(graph, source_plot) {
  candidates <- c(list(graph), graph$patches$plots)
  source_y <- sort(ggplot2::ggplot_build(source_plot)$data[[1]]$y)
  for (panel in candidates) {
    panel_y <- sort(ggplot2::ggplot_build(panel)$data[[1]]$y)
    if (isTRUE(all.equal(panel_y, source_y, tolerance = 1e-6))) {
      return(t61_label_layer_data(panel))
    }
  }
  stop("Could not match panel to source plot")
}

save_multi_test <- function(plots) {
  theme61:::save_multi(
    filename = NULL, format = "svg", plots = plots, chart_type = "normal",
    title = NULL, subtitle = NULL, footnotes = NULL, sources = NULL,
    width = NULL, height = NULL, auto_scale = TRUE,
    title_spacing_adj = 1, subtitle_spacing_adj = 1, base_size = 10,
    pad_width = 0, pad_height = 0, outer_width = NULL, outer_height = NULL,
    height_adj = NULL,
    ncol = 2, nrow = NULL, align = "v", axis = "none", rel_heights = NULL,
    bg_colour = "white"
  )
}

test_that("save_multi() auto-positions labels independently on each panel", {
  skip_on_cran()

  # Two panels sharing series names but with different data -- identical
  # resolved positions would mean the second panel's mask/series were
  # never actually rebuilt for its own data.
  data1 <- data.frame(
    x = rep(2000:2020, 2),
    y = c(seq(0, 5, length.out = 21), seq(10, 2, length.out = 21)),
    series = rep(c("A", "B"), each = 21)
  )
  data2 <- data.frame(
    x = rep(2000:2020, 2),
    y = c(seq(2, 9, length.out = 21), rep(1, 21)),
    series = rep(c("A", "B"), each = 21)
  )
  cols <- c(A = "#e57200", B = "#1c3144")

  make_panel <- function(data) {
    ggplot(data, aes(x, y, colour = series)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = cols) +
      theme_bw(base_size = 10) +
      theme(legend.position = "none") +
      plot_label(c("A", "B"), colour = unname(cols))
  }

  p1 <- make_panel(data1); p2 <- make_panel(data2)
  sv <- save_multi_test(list(p1, p2))

  d1 <- multi_panel_label_data(sv$graph, p1)
  d2 <- multi_panel_label_data(sv$graph, p2)

  expect_false(anyNA(d1$x)); expect_false(anyNA(d1$y))
  expect_false(anyNA(d2$x)); expect_false(anyNA(d2$y))

  # Each panel's "A" label should sit closer to its own series' y-range
  # than to the other panel's -- confirms it was matched/placed against
  # this panel's own data, not e.g. reusing panel 1's layout for panel 2.
  a1_y <- d1$y[d1$label == "A"]; a2_y <- d2$y[d2$label == "A"]
  expect_lt(abs(a1_y - 2.5), 3.5) # data1's A ranges 0-5
  expect_lt(abs(a2_y - 5.5), 4.5) # data2's A ranges 2-9
  expect_false(isTRUE(all.equal(a1_y, a2_y)))
})

test_that("save_multi() leaves explicit plot_label(x=, y=) positions untouched", {
  skip_on_cran()

  data <- data.frame(x = 1:10, y = 1:10)
  make_panel <- function(title) {
    ggplot(data, aes(x, y)) +
      geom_line(colour = "#e57200") +
      theme_bw(base_size = 10) +
      labs_e61(title = title) +
      plot_label("Fixed", x = 5, y = 5, colour = "#e57200", auto_position = FALSE)
  }

  sv <- save_multi_test(list(make_panel("Panel 1"), make_panel("Panel 2")))

  # Both panels use identical data, so there's nothing to match against --
  # unlike the test above, that's fine here since the expected position is
  # the same (5, 5) for either panel, regardless of patchwork's internal
  # base/patches ordering.
  panels <- c(list(sv$graph), sv$graph$patches$plots)
  expect_length(panels, 2)
  for (panel in panels) {
    d <- t61_label_layer_data(panel)
    expect_equal(d$x, 5)
    expect_equal(d$y, 5)
  }
})
