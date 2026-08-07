# Tests for save_e61(build_up = TRUE) (#253) -------------------------------

# Internal helpers -----------------------------------------------------------

test_that("resolve_build_up steps through x categories for a bar chart, zeroing hidden ones", {
  df <- data.frame(cat = factor(c("A", "B", "C"), levels = c("A", "B", "C")), val = c(1, 2, 3))
  p <- ggplot(df, aes(cat, val)) + geom_col()

  res <- resolve_build_up(p)

  expect_length(res$steps, 3)
  expect_identical(res$targets, 1L)

  expect_equal(res$steps[[1]][[1]]$val, c(1, 0, 0))
  expect_equal(res$steps[[2]][[1]]$val, c(1, 2, 0))
  expect_equal(res$steps[[3]][[1]]$val, c(1, 2, 3))

  # Category column itself is untouched (never filtered/removed)
  expect_identical(as.character(res$steps[[1]][[1]]$cat), c("A", "B", "C"))
})

test_that("resolve_build_up respects factor level order, not appearance order", {
  df <- data.frame(cat = factor(c("C", "A", "B"), levels = c("B", "C", "A")), val = c(1, 2, 3))
  p <- ggplot(df, aes(cat, val)) + geom_col()

  res <- resolve_build_up(p)

  # Level order is B, C, A - so step 1 should only reveal B (val = 3)
  step1 <- res$steps[[1]][[1]]
  expect_equal(step1$val[step1$cat == "B"], 3)
  expect_equal(step1$val[step1$cat != "B"], c(0, 0))
})

test_that("resolve_build_up reveals stacked area groups bottom-to-top by zeroing", {
  df <- data.frame(
    x = rep(1:5, 2),
    y = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
    grp = factor(rep(c("bottom", "top"), each = 5), levels = c("bottom", "top"))
  )
  p <- ggplot(df, aes(x, y, fill = grp)) + geom_area()

  res <- resolve_build_up(p)
  expect_length(res$steps, 2)

  # Work out which group is empirically on the bottom of the stack, at a real
  # data row (x = 3) rather than x = built$x[1] - geom_area() pads the built
  # data with an extra all-zero row just outside the data range for smooth
  # rendering of the area edge, which would make every group look tied for
  # the bottom and silently mask a real ordering bug (see #331 follow-up).
  built <- ggplot_build(p)$data[[1]]
  ref <- built[built$x == 3, ]
  ref <- ref[order(ref$group), ]
  expect_true(all(ref$ymax > 0)) # sanity check this is a real, non-padding row
  bottom_is_first_level <- ref$ymin[1] <= ref$ymin[nrow(ref)]
  bottom_level <- if (bottom_is_first_level) levels(df$grp)[1] else levels(df$grp)[2]

  step1 <- res$steps[[1]][[1]]
  revealed <- unique(step1$grp[step1$y != 0])
  hidden <- unique(step1$grp[step1$y == 0])

  expect_setequal(as.character(revealed), bottom_level)
  expect_setequal(as.character(hidden), setdiff(levels(df$grp), bottom_level))

  # Final step is the untouched, fully-revealed data
  expect_equal(res$steps[[2]][[1]]$y, df$y)
})

test_that("resolve_build_up steps through groups for a multi-line chart, blanking to NA", {
  df <- data.frame(
    x = rep(1:5, 2),
    y = c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1),
    grp = rep(c("A", "B"), each = 5)
  )
  p <- ggplot(df, aes(x, y, colour = grp)) + geom_line()

  res <- resolve_build_up(p)
  expect_length(res$steps, 2)

  step1 <- res$steps[[1]][[1]]
  expect_equal(step1$y[step1$grp == "A"], df$y[df$grp == "A"])
  expect_true(all(is.na(step1$y[step1$grp == "B"])))

  step2 <- res$steps[[2]][[1]]
  expect_equal(step2$y, df$y)
})

test_that("resolve_build_up falls back to a cumulative x-axis build for a single ungrouped series", {
  df <- data.frame(x = 1:10, y = (1:10)^2)
  p <- ggplot(df, aes(x, y)) + geom_line()

  res <- resolve_build_up(p)
  expect_length(res$steps, 10)

  # Each step reveals strictly more of the series than the last, and the last
  # step is the fully-revealed series
  n_revealed <- vapply(res$steps, function(s) sum(!is.na(s[[1]]$y)), numeric(1))
  expect_true(all(diff(n_revealed) >= 0))
  expect_equal(n_revealed[10], 10)
  expect_equal(res$steps[[10]][[1]]$y, df$y)
})

test_that("build_up_n controls the number of steps for a continuous build", {
  df <- data.frame(x = 1:20, y = 1:20)
  p <- ggplot(df, aes(x, y)) + geom_line()

  res <- resolve_build_up(p, build_up_n = 4)
  expect_length(res$steps, 4)
  expect_equal(res$steps[[4]][[1]]$y, df$y)
})

test_that("resolve_build_up blanks every layer sharing the reference layer's data (geom_pointbar)", {
  df <- data.frame(
    grp = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    est = c(1, 2, 3),
    lo = c(0.5, 1.5, 2.5),
    hi = c(1.5, 2.5, 3.5)
  )
  p <- ggplot(df, aes(x = grp, y = est, ymin = lo, ymax = hi)) + geom_pointbar()

  res <- resolve_build_up(p)

  expect_length(res$targets, 2) # errorbar layer + point layer
  expect_length(res$steps, 3)

  step1 <- res$steps[[1]]
  for (layer_data in step1) {
    hidden <- layer_data$grp != "A"
    if ("y" %in% names(layer_data)) expect_true(all(is.na(layer_data$y[hidden])))
    if ("ymin" %in% names(layer_data)) expect_true(all(is.na(layer_data$ymin[hidden])))
    if ("ymax" %in% names(layer_data)) expect_true(all(is.na(layer_data$ymax[hidden])))
  }
})

test_that("resolve_build_up errors for unsupported geoms", {
  df <- data.frame(x = 1:5, y = 1:5, lab = letters[1:5])
  p <- ggplot(df, aes(x, y, label = lab)) + geom_text()

  expect_error(resolve_build_up(p), "not supported")
})

# save_e61(build_up = TRUE) integration ---------------------------------------

test_that("save_e61 build_up saves one suffixed file per category with identical dimensions", {
  df <- data.frame(cat = factor(c("A", "B", "C")), val = c(1, 2, 3))
  p <- ggplot(df, aes(cat, val)) + geom_col()

  withr::with_tempdir({
    suppressWarnings(save_e61("chart.svg", p, build_up = TRUE))

    expect_true(file.exists("chart_1.svg"))
    expect_true(file.exists("chart_2.svg"))
    expect_true(file.exists("chart_3.svg"))
    expect_false(file.exists("chart.svg"))

    get_dims <- function(f) {
      txt <- paste(readLines(f, n = 5), collapse = " ")
      c(width = regmatches(txt, regexpr('(?<=width=")[^"]+', txt, perl = TRUE)),
        height = regmatches(txt, regexpr('(?<=height=")[^"]+', txt, perl = TRUE)))
    }

    d1 <- get_dims("chart_1.svg")
    d2 <- get_dims("chart_2.svg")
    d3 <- get_dims("chart_3.svg")

    expect_equal(d1, d2)
    expect_equal(d1, d3)
  })
})

test_that("save_e61 build_up writes one csv per step when save_data = TRUE", {
  df <- data.frame(cat = factor(c("A", "B")), val = c(1, 2))
  p <- ggplot(df, aes(cat, val)) + geom_col()

  withr::with_tempdir({
    suppressWarnings(save_e61("chart.svg", p, build_up = TRUE, save_data = TRUE))

    expect_true(file.exists("chart_1.csv"))
    expect_true(file.exists("chart_2.csv"))

    step1 <- data.table::fread("chart_1.csv")
    expect_equal(step1$val, c(1, 0))
  })
})

test_that("save_e61 build_up is rejected for multi-panel, faceted, preview and return_plot_obj", {
  df <- data.frame(cat = factor(c("A", "B")), val = c(1, 2))
  p <- ggplot(df, aes(cat, val)) + geom_col()
  p_facet <- p + facet_wrap(~cat)

  withr::with_tempdir({
    expect_error(save_e61("chart.svg", p, p, build_up = TRUE), "single-panel")
    expect_error(save_e61("chart.svg", p, build_up = TRUE, return_plot_obj = TRUE), "return_plot_obj")
    expect_error(save_e61("chart.svg", p, build_up = TRUE, preview = TRUE), "preview")
    expect_error(suppressWarnings(save_e61("chart.svg", p_facet, build_up = TRUE)), "facet")
  })
})
