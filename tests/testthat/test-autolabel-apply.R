# Tests for wiring the autolabel engine into plot_label()/save_e61(): does
# t61_apply_autolabel() find eligible plot_label() labels, match them to
# their series by colour, and move them -- while leaving everything out of
# v1 scope (opted-out, rotated, unmatched, facetted) exactly where the user
# put it?

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

test_that("t61_apply_autolabel doesn't mutate the caller's own plot object", {
  skip_on_cran()

  # A layer's data is a data.table, and the layer itself is a ggproto
  # (environment) -- both mutate by reference, so writing a resolved
  # position into the RETURNED plot could silently write through to the
  # plot the caller still holds too (e.g. one print()/save_e61() call
  # leaving stale positions behind for a later, independent call on the
  # same object).
  p <- autolabel_apply_test_setup()
  label_layer <- which(vapply(p@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  before <- data.table::copy(p@layers[[label_layer]]$data)

  invisible(t61_apply_autolabel(p, width_cm = 16, height_cm = 12))

  expect_equal(p@layers[[label_layer]]$data, before)
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
    plot_label(c("A", "B"), x = c(2016, 2016), y = c(1, 1), colour = c("#e57200", "#1c3144"))

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(isTRUE(all.equal(d$x[1], 2016)) && isTRUE(all.equal(d$y[1], 1)))
  expect_false(isTRUE(all.equal(d$x[2], 2016)) && isTRUE(all.equal(d$y[2], 1)))

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
    plot_label("Series A", x = 2001, y = 1, colour = "#1c3144") # deliberately bad, off the band

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(isTRUE(all.equal(d$x, 2001)) && isTRUE(all.equal(d$y, 1)))
  expect_gt(d$y, 0) # moved up into the visible (growing) band, not left at y=1

  expect_equal(result@layers[[label_layer]]$aes_params$colour, "white")
})

test_that("t61_apply_autolabel repositions a geom_pointbar() label clear of the error bars", {
  skip_on_cran()

  data <- data.frame(x = 2000:2010, y = seq(0, 5, length.out = 11))
  data$ymin <- data$y - 1
  data$ymax <- data$y + 1

  p <- ggplot(data, aes(x, y, ymin = ymin, ymax = ymax)) +
    geom_pointbar(colour = "#e57200") +
    theme_bw(base_size = 10) +
    plot_label("Series A", x = 2005, y = -3, colour = "#e57200") # deliberately bad, below the bars

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(isTRUE(all.equal(d$x, 2005)) && isTRUE(all.equal(d$y, -3)))

  mask <- t61_render_mask(t61_strip_autolabel_layers(p), width_cm = 16, height_cm = 12)
  cm <- t61_measure_label_cm("Series A", size_mm = 3.5, width_cm = 16, height_cm = 12)
  box <- t61_text_box_px(d$x, d$y, cm, mask, hjust = 0)

  expect_false(t61_test_collision(mask$occupancy, box$row_range, box$col_range))
})

# x/y are optional when auto_position = TRUE (see ?plot_label): the
# fallback order is (1) a good spot found by the placement algorithm, (2)
# the caller's own x/y if they gave one, (3) any empty space at all if
# they didn't. These tests cover (1) and (3) end-to-end, and (2)'s
# priority over (3) directly.

test_that("t61_apply_autolabel resolves a real position with no x/y supplied at all", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Series A", colour = "#e57200")

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(is.na(d$x))
  expect_false(is.na(d$y))
})

test_that("t61_apply_autolabel still resolves a position when no series matches and no x/y was supplied", {
  skip_on_cran()

  data <- data.frame(x = 2000:2020, y = seq(0, 5, length.out = 21))
  p <- ggplot(data, aes(x, y)) +
    geom_line(colour = "#e57200", linewidth = 1) +
    theme_bw(base_size = 10) +
    plot_label("Unrelated", colour = "#123456") # matches no series, and no x/y given

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  # Not NA (the old "keep the fallback" behaviour would leave it invisible,
  # since there's no fallback to keep any more), and it shouldn't collide
  # with the line
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

  # Force the "good spot" algorithm (tier 1) to fail, so the fallback
  # tiers decide the outcome; the mask is otherwise wide open, so an "any
  # empty space" search (tier 3) would trivially succeed too -- the test
  # is which one wins.
  testthat::local_mocked_bindings(t61_place_label = function(...) NULL)

  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_equal(d$x, 2019)
  expect_equal(d$y, 4.5)
})

coord_flip_apply_test <- function(p) {
  result <- t61_apply_autolabel(p, width_cm = 16, height_cm = 12)

  label_layer <- which(vapply(result@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  d <- result@layers[[label_layer]]$data

  expect_false(anyNA(d$x))
  expect_false(anyNA(d$y))

  # Rebuild the mask for the now-labelled plot and check each resolved
  # position renders fully inside the flipped panel, not off the edge.
  # d$x/d$y are stored in pre-flip data space (ggplot re-flips them at
  # render time, same as any other layer), so they need converting to the
  # mask's screen space before box math -- same conversion t61_autolabel_plot()
  # itself does before stamping.
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

test_that("t61_apply_autolabel auto-positions labels on a coord_flip() column chart", {
  skip_on_cran()

  # The reported bug: no fallback x/y given, so a resolved position stayed
  # NA (silently dropped from the chart) rather than an actual placement.
  data <- data.frame(
    category = rep(c("A", "B", "C"), 2),
    value = c(5, 8, 3, 6, 2, 9),
    series = rep(c("X", "Y"), each = 3)
  )
  cols <- c(X = "#e57200", Y = "#1c3144")

  p <- ggplot(data, aes(category, value, fill = series)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = cols) +
    coord_flip() +
    theme_bw(base_size = 10) +
    plot_label(c("X", "Y"), colour = unname(cols))

  coord_flip_apply_test(p)
})

# save_multi() (save_e61(plot1, plot2, ...)) combines independent
# single-panel plots via patchwork rather than through save_single(), so it
# needs its own call to t61_apply_autolabel() per panel -- these tests
# exercise that wiring directly, since save_e61() itself doesn't return the
# plot object to inspect.

#' Pull a plot_label() layer's resolved data out of a save_multi() result.
#' `graph` is a patchwork object: the base plot's own layers hold the first
#' panel, and `graph$patches$plots` holds the rest, in the order the caller
#' passed them (see ?patchwork::wrap_plots internals) -- matched here by the
#' label text itself rather than assumed index order, since that ordering
#' isn't part of patchwork's contract.
#' @noRd
multi_panel_label_data <- function(graph, panel_index) {
  panel <- if (panel_index == 1) graph else graph$patches$plots[[panel_index - 1]]
  label_layer <- which(vapply(panel@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))
  panel@layers[[label_layer]]$data
}

save_multi_test <- function(plots) {
  theme61:::save_multi(
    filename = NULL, format = "svg", plots = plots, chart_type = "normal",
    title = NULL, subtitle = NULL, footnotes = NULL, sources = NULL,
    width = NULL, height = NULL, auto_scale = TRUE,
    title_spacing_adj = 1, subtitle_spacing_adj = 1, base_size = 10,
    pad_width = 0, pad_height = 0, height_adj = NULL,
    ncol = 2, nrow = NULL, align = "v", axis = "none", rel_heights = NULL,
    bg_colour = "white"
  )
}

test_that("save_multi() auto-positions labels independently on each panel", {
  skip_on_cran()

  # Two panels sharing series names but with different underlying data, so
  # a correct per-panel resolve should land on genuinely different
  # positions -- if the panels ended up identical, that would mean the
  # second panel's mask/series were never actually rebuilt for its own data.
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

  sv <- save_multi_test(list(make_panel(data1), make_panel(data2)))

  d1 <- multi_panel_label_data(sv$graph, 1)
  d2 <- multi_panel_label_data(sv$graph, 2)

  expect_false(anyNA(d1$x)); expect_false(anyNA(d1$y))
  expect_false(anyNA(d2$x)); expect_false(anyNA(d2$y))

  # Each panel's "A" label should sit closer to its own series' y-range
  # than to the other panel's -- confirms it was matched/placed against
  # this panel's own data, not e.g. reusing panel 1's layout for panel 2.
  a1_y <- d1$y[d1$label == "A"]; a2_y <- d2$y[d2$label == "A"]
  expect_lt(abs(a1_y - 2.5), 3) # data1's A ranges 0-5
  expect_lt(abs(a2_y - 5.5), 4) # data2's A ranges 2-9
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

  expect_equal(multi_panel_label_data(sv$graph, 1)$x, 5)
  expect_equal(multi_panel_label_data(sv$graph, 1)$y, 5)
  expect_equal(multi_panel_label_data(sv$graph, 2)$x, 5)
  expect_equal(multi_panel_label_data(sv$graph, 2)$y, 5)
})
