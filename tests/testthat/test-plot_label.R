test_that("Single plot label works", {
  p1 <- minimal_plot_label +
    plot_label(label = "Plot 1", x = 2, y = 2, auto_position = FALSE)

  # plot_label should add exactly one new layer (a text/label geom)
  expect_gte(length(p1@layers), 1)

  last_layer <- p1@layers[[length(p1@layers)]]

  # Should not inherit plot aesthetics (plot_label should stand alone)
  expect_false(isTRUE(last_layer$inherit.aes))

  # Should be a text geom
  expect_true(inherits(last_layer$geom, "GeomText"))

  # Layer should carry a one-row data.frame with the label and coordinates
  expect_s3_class(last_layer$data, "data.frame")
  expect_equal(nrow(last_layer$data), 1)

  expect_true(all(c("label", "x", "y") %in% names(last_layer$data)))
  expect_equal(last_layer$data$label[[1]], "Plot 1")
  expect_equal(last_layer$data$x[[1]], 2)
  expect_equal(last_layer$data$y[[1]], 2)

  # Sanity: should build without warnings/errors
  expect_no_warning(ggplot_build(p1))
})

test_that("Multi-plot labels work with colour and fill", {

  # ---- aes - colour
  data1 <- data.frame(
    x = c(0, 1, 0, 1, 0, 1),
    y = c(1.1, 1.1, 2.1, 2.1, 3.1, 3.1),
    group = factor(c(1, 1, 2, 2, 3, 3))
  )

  p1 <- ggplot(data1, aes(x, y, colour = group)) +
    geom_line() +
    plot_label(c("1", "2", "3"),
               rep(0.5, 3),
               c(1.3, 2.3, 3.3),
               auto_position = FALSE)

  # Force theme61 ggplot_build hook to add scale_colour_e61()
  b1 <- expect_no_warning(ggplot_build(p1))

  l1 <- b1@plot@layers[[length(b1@plot@layers)]]
  expect_equal(l1$data$label, c("1", "2", "3"))

  # Get the colour scale palette that the plot is using after build
  sc_col <- b1@plot@scales$get_scales("colour")
  expect_false(is.null(sc_col))

  n1 <- length(levels(data1$group))
  expected_colours <- sc_col$palette(n1)

  # plot_label sets explicit colours via the geom parameter (stored on the layer)
  expect_equal(l1$aes_params$colour, expected_colours)


  # ---- aes - fill
  data2 <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(1, 1, 2, 2),
    group = factor(c(1, 1, 2, 2))
  )

  p2 <- ggplot(data2, aes(x, y, fill = group)) +
    geom_col(position = "dodge") +
    plot_label(c("1", "2"),
               c(-0.25, 0.25),
               c(1.2, 2.2),
               auto_position = FALSE)

  b2 <- expect_no_warning(ggplot_build(p2))

  l2 <- b2@plot@layers[[length(b2@plot@layers)]]
  expect_equal(l2$data$label, c("1", "2"))

  sc_fill <- b2@plot@scales$get_scales("fill")
  expect_false(is.null(sc_fill))

  n2 <- length(levels(data2$group))
  expected_fills <- sc_fill$palette(n2)

  # Labels should match the fill palette too (even though label uses colour)
  expect_equal(l2$aes_params$colour, expected_fills)
})

test_that("plot_label() derives label/colour from a discrete colour/fill scale", {

  data <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(0, 0, 1, 1),
    grp = factor(c("Alpha", "Alpha", "Beta", "Beta"))
  )
  cols <- c(Alpha = "#7b3294", Beta = "#008837")

  base <- ggplot(data, aes(x, y, colour = grp)) +
    geom_line() +
    scale_colour_manual(values = cols)

  # label omitted entirely: derived from the scale's own levels, in order
  p1 <- base + plot_label()
  d1 <- p1@layers[[length(p1@layers)]]$data
  expect_equal(d1$label, c("Alpha", "Beta"))
  expect_equal(d1$colour, unname(cols[c("Alpha", "Beta")]))

  # label supplied, colour omitted: matched positionally against the
  # scale's levels (assumes label is given in the same order)
  p2 <- base + plot_label(c("Group Alpha", "Group Beta"))
  d2 <- p2@layers[[length(p2@layers)]]$data
  expect_equal(d2$label, c("Group Alpha", "Group Beta"))
  expect_equal(d2$colour, unname(cols[c("Alpha", "Beta")]))

  # colour explicitly supplied: always wins, scale ignored
  p3 <- base + plot_label(c("Alpha", "Beta"), colour = c("red", "blue"))
  d3 <- p3@layers[[length(p3@layers)]]$data
  expect_equal(d3$colour, c("red", "blue"))

  # values= order in the scale_colour_manual() call is irrelevant -- the
  # scale's own (trained) level order wins, same as a legend would show
  base_shuffled <- ggplot(data, aes(x, y, colour = grp)) +
    geom_line() +
    scale_colour_manual(values = c(Beta = "#008837", Alpha = "#7b3294"))
  p4 <- base_shuffled + plot_label()
  d4 <- p4@layers[[length(p4@layers)]]$data
  expect_equal(d4$label, c("Alpha", "Beta"))
  expect_equal(d4$colour, unname(cols[c("Alpha", "Beta")]))

  # fill works the same way as colour
  data_fill <- data.frame(x = c("a", "b"), y = c(1, 2))
  fill_cols <- c(a = "#111111", b = "#222222")
  p5 <- ggplot(data_fill, aes(x, y, fill = x)) +
    geom_col() +
    scale_fill_manual(values = fill_cols) +
    plot_label()
  d5 <- p5@layers[[length(p5@layers)]]$data
  expect_equal(d5$label, c("a", "b"))
  expect_equal(d5$colour, unname(fill_cols))

  # More labels than the scale has levels for: falls back to the e61
  # palette entirely, rather than partially matching
  p6 <- base + plot_label(c("Alpha", "Beta", "Gamma"))
  d6 <- p6@layers[[length(p6@layers)]]$data
  expect_equal(d6$colour, palette_e61(3))

  # No scale supplied at all: theme61 auto-injects a default discrete scale
  # at build time (see maybe_add_default_scales()), and that now counts too
  # -- label/colour derive from it, the same as an explicit manual scale.
  no_scale <- ggplot(data, aes(x, y, colour = grp))
  p8 <- no_scale + geom_line() + plot_label()
  b8 <- ggplot_build(no_scale + geom_line())
  sc8 <- b8@plot@scales$get_scales("colour")
  expected8 <- unname(sc8$map(levels(data$grp)))
  d8 <- p8@layers[[length(p8@layers)]]$data
  expect_equal(d8$label, levels(data$grp))
  expect_equal(d8$colour, expected8)

  # An algorithmic discrete scale (not "manual") counts too -- label can be
  # omitted, and colour is matched to the scale's actual resolved colours,
  # not an independent call to the e61 palette.
  algo_scale <- ggplot(data, aes(x, y, colour = grp)) + geom_line() + scale_colour_e61()
  p7a <- algo_scale + plot_label()
  ba <- ggplot_build(algo_scale)
  sca <- ba@plot@scales$get_scales("colour")
  expected7 <- unname(sca$map(levels(data$grp)))
  d7a <- p7a@layers[[length(p7a@layers)]]$data
  expect_equal(d7a$label, levels(data$grp))
  expect_equal(d7a$colour, expected7)

  p7 <- algo_scale + plot_label(c("Alpha", "Beta"))
  d7 <- p7@layers[[length(p7@layers)]]$data
  expect_equal(d7$colour, expected7)

  # Continuous colour/fill still doesn't count -- there's no fixed set of
  # levels to derive a default label from, so this still errors
  data_cont <- data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1), z = c(1, 2, 3, 4))
  cont_scale <- ggplot(data_cont, aes(x, y, colour = z)) + geom_point()
  expect_error(
    cont_scale + plot_label(),
    "no discrete"
  )
})

test_that("Plots with additional aes still work", {

  data <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(0.5, 0.5, 1.5, 1.5),
    linetype = factor(c(1, 1, 2, 2))
  )

  p <- ggplot(data, aes(x, y, colour = linetype, linetype = linetype)) +
    geom_line() +
    scale_y_continuous_e61(c(0, 2, 1)) +
    plot_label(c("Solid", "Dotted"),
               c(0.25, 0.25),
               c(0.75, 1.75),
               auto_position = FALSE)

  l <- p@layers[[length(p@layers)]]

  # Make sure labels don't inherit aes so linetype doesn't propagate
  expect_false(isTRUE(l$inherit.aes))

  # And it should not have linetype mapping/params attached
  expect_true(is.null(l$mapping$linetype))
  expect_true(is.null(l$aes_params$linetype))
  expect_true(is.null(l$params$linetype))

  expect_no_warning(ggplot_build(p))

})

test_that("String dates get converted to date dates properly", {

  retval <- plot_label("Label", x = "2020-01-01", y = 1)

  expect_equal(class(retval$x), "Date")

})

test_that("Specifying custom colours works in plot_label()", {

  # Default colours -- unresolved on the raw plot_label() object (it has
  # no access to the plot yet, see .build_plot_label_layer()), resolved
  # to the e61 palette once actually added to a plot with no manual scale
  # to derive colours from instead.
  retval <- plot_label("Test", 1, 1)
  expect_true(is.na(retval$colour))

  built <- minimal_plot_label + retval
  expect_equal(built@layers[[length(built@layers)]]$data$colour, palette_e61(1))

  # Custom colours
  retval <- plot_label("Test", 1, 1, colour = "#000000")
  expect_equal(retval$colour, "#000000")

  retval <- plot_label(
    c("Test 1", "Test 2"),
    c(1, 1),
    c(1, 2),
    colour = c("#000000", "#cccccc"))
  expect_equal(retval$colour, c("#000000", "#cccccc"))
})

test_that("Text and label plot labels work", {
  p1 <- minimal_plot_label +
    plot_label("label", 2, 2, geom = "label", auto_position = FALSE) +
    scale_y_continuous_e61()

  p2 <- minimal_plot_label +
    plot_label("text", 2, 2, geom = "text", auto_position = FALSE)

  expect_true(inherits(p1@layers[[length(p1@layers)]]$geom, "GeomLabel"))
  expect_true(inherits(p2@layers[[length(p1@layers)]]$geom, "GeomText"))

})

test_that("Specifying incorrect length of label characteristics fails", {
  expect_error({
    ggplot() + plot_label(rep("x", 3),
                           x = rep(0, 3),
                           y = rep(0, 3),
                           hjust = rep(0, 2))
  })


  expect_error({
    ggplot() + plot_label(rep("x", 3),
                           x = rep(0, 3),
                           y = rep(0, 3),
                           angle = rep(0, 2))
  })

})

test_that("x/y are optional when auto_position = TRUE, required otherwise", {
  # auto_position = FALSE has no algorithm to fall back on, so x/y stay required
  expect_error(
    plot_label("a", colour = "red", auto_position = FALSE),
    "required when `auto_position = FALSE`"
  )

  # x and y must be supplied together, or not at all
  expect_error(plot_label("a", x = 1, colour = "red"), "supplied together")
  expect_error(plot_label("a", y = 1, colour = "red"), "supplied together")

  # Both omitted, auto_position = TRUE (the default): no error
  expect_no_error(plot_label("a", colour = "red"))

  # A facetted plot has no automatic positioning to fall back on either
  data <- data.frame(x = 1:10, y = 1:10, grp = rep(c("A", "B"), 5))
  p <- ggplot(data, aes(x, y)) + geom_line(colour = "red") + facet_wrap(~grp)
  expect_error(
    p + plot_label("a", colour = "red"),
    "facetted"
  )
})

test_that("theme61.autolabel = FALSE restores the old always-required x/y behaviour", {
  withr::local_options(list(theme61.autolabel = FALSE))

  # auto_position = TRUE (the default) normally allows x/y to be omitted --
  # with the option off, it no longer does.
  expect_error(
    plot_label("a", colour = "red"),
    "automatic positioning is disabled"
  )

  # auto_position = FALSE already requires x/y on its own -- turning the
  # option off doesn't change that error message.
  expect_error(
    plot_label("a", colour = "red", auto_position = FALSE),
    "auto_position = FALSE"
  )

  # An explicit position is unaffected either way.
  expect_no_error(plot_label("a", x = 1, y = 1, colour = "red"))
})

test_that("Rotated text requires x/y (angle != 0 has no automatic positioning to fall back on)", {
  expect_error(
    plot_label("a", colour = "red", angle = 45),
    "angle != 0"
  )

  # A mix of rotated and unrotated labels still requires x/y for all of
  # them, since they're one auto_position call, not resolved individually.
  expect_error(
    plot_label(c("a", "b"), colour = c("red", "blue"), angle = c(0, 45)),
    "angle != 0"
  )

  # An explicit position is fine regardless of rotation.
  expect_no_error(plot_label("a", x = 1, y = 1, colour = "red", angle = 45))

  # auto_position = FALSE already requires x/y on its own -- rotation
  # doesn't change that error.
  expect_error(
    plot_label("a", colour = "red", angle = 45, auto_position = FALSE),
    "auto_position = FALSE"
  )
})

test_that("Changing horizontal alignment of text works", {

  p <- minimal_plot_label +
    plot_label(
      c("Left-aligned text", "Centre-aligned text", "Right-aligned text"),
      x = rep(2, 3),
      y = c(1.5, 2, 2.5),
      hjust = c(0, 0.5, 1)
    )

  # Check that hjust is stored correctly in the ggplot object
  expect_equal(p@layers[[length(p@layers)]]$data$hjust, c(0, 0.5, 1))
})

test_that("Label rotation works", {

  ## Separate plot_labels work
  p1 <- minimal_plot_label +
    plot_label("Normal text", 0.5, 1.5, angle = 90, auto_position = FALSE) +
    plot_label("Vertical text", 1.5, 1.5, angle = 0, auto_position = FALSE) +
    plot_label("Diagonal text", 2.5, 1.5, angle = 45, auto_position = FALSE) +
    scale_y_continuous_e61(limits = c(0, 3))

  # 3 geoms for labels get added + 1 geom_point
  expect_equal(length(p1@layers), 4)

  # Angles are correctly stored in the ggplot object
  expect_equal(p1@layers[[2]]$aes_params$angle, 90)
  expect_equal(p1@layers[[3]]$aes_params$angle, 0)
  expect_equal(p1@layers[[4]]$aes_params$angle, 45)

  ## plot_label with multiple angles works
  p2 <- minimal_plot_label +
    plot_label(c("Normal text", "Vertical text", "Diagonal text"),
                x = rep(2, 3),
                y = c(2.1, 2.3, 2.15),
                angle = c(0, 90, 45),
                auto_position = FALSE) +
    scale_y_continuous_e61(c(0, 3))

  # Only one extra layer gets created with all the label info
  expect_equal(length(p2@layers), 2)

  # Angles are stored as a vector in the ggplot object
  expect_equal(p2@layers[[2]]$aes_params$angle, c(0, 90, 45))

})

test_that("Labels work on facet_wrap", {

  data <- data.frame(
    x = rep(c(1, 2), 2),
    y = rep(c(1, 2), 2),
    f_var = factor(c(1, 1, 2, 2)),
    group = factor(c(1, 2, 1, 2))
  )

  p <- ggplot(data, aes(x, y, colour = group)) +
    facet_wrap(~f_var) +
    geom_point() +
    scale_y_continuous_e61(c(0, 3, 1))

  ## Place labels on 1 facet only
  lab_name <- c("Lab 1", "Lab 2")
  x_pos <- c(1.25, 1.75)
  y_pos <- c(1, 2)

  p1 <- p + plot_label(
    label = lab_name,
    x = x_pos,
    y = y_pos,
    panel = list(f_var = "1")
  )

  last_layer <- p1@layers[[length(p1@layers)]]

  # Should produce a data.frame with correct values and locations
  expect_equal(last_layer$data$label, lab_name)
  expect_equal(last_layer$data$x, x_pos)
  expect_equal(last_layer$data$y, y_pos)
  expect_equal(as.character(last_layer$data$f_var), rep("1", 2))

  # Should build without issues
  expect_no_warning(ggplot_build(p1))

  ## Place 1 labels on 1 facet and the other label on the other
  p2 <- p + plot_label(
    label = lab_name,
    x = x_pos,
    y = y_pos,
    panel = list(f_var = c("1", "2"))
  )

  last_layer <- p2@layers[[length(p2@layers)]]

  # Should be positioned on different panels
  expect_equal(as.character(last_layer$data$f_var), c("1", "2"))

  ## Defaults to putting the labels on all facets if no facet specified
  p3 <- p + plot_label(
    label = c("Lab 1", "Lab 2"),
    x = c(1.25, 1.75),
    y = c(1, 2)
  )

  last_layer <- p3@layers[[length(p3@layers)]]

  # Should be positioned on all panels
  expect_equal(nrow(last_layer$data), 4L)
  expect_equal(last_layer$data$label,
               rep(c("Lab 1", "Lab 2"), 2))
  expect_equal(as.character(last_layer$data$f_var),
               rep(c("1", "2"), each = 2))
})

test_that("Facet panel names must match facet variables", {
  data <- data.frame(
    x = rep(c(1, 2), 2),
    y = rep(c(1, 2), 2),
    f_var = factor(c(1, 1, 2, 2))
  )

  p <- ggplot(data, aes(x, y)) +
    facet_wrap(~f_var) +
    geom_point()

  expect_error(
    p + plot_label("oops", 1, 1, panel = list(bad = "1")),
    regexp = "do not match the plot's facet variables"
  )
})

test_that("Labels work on facet_grid", {
  df <- data.frame(
    x = 1:8,
    y = 1:8,
    r = ordered(rep(c("B", "A"), each = 4), levels = c("B", "A")),
    c = factor(rep(c("2", "1"), times = 4), levels = c("2", "1"))
  )

  p0 <- ggplot(df, aes(x, y)) +
    geom_point() +
    facet_grid(r ~ c)

  # Baseline facet layout order
  lay0 <- ggplot_build(p0)@layout$layout[, c("r", "c"), drop = FALSE]

  # Add labels without specifying facet vars (labels should appear on all panels; layout unchanged)
  p_all <- p0 + plot_label(
    label = c("L1", "L2"),
    x = c(2, 3),
    y = c(2, 3)
  )

  lay_all <- ggplot_build(p_all)@layout$layout[, c("r", "c"), drop = FALSE]
  expect_identical(lay_all, lay0)

  # Target different labels to different panels (must supply BOTH facet vars)
  p_target <- p0 + plot_label(
    label = c("RowB-Col2", "RowA-Col1"),
    x = c(2, 3),
    y = c(2, 3),
    panel = list(r = c("B", "A"),
                 c = c("2", "1"))
  )

  lab_data <- ggplot_build(p_target)@data[[2]]  # assumes: layer1 = points, layer2 = labels

  expect_equal(nrow(lab_data), 2L)
  expect_identical(as.character(lab_data$PANEL), c("1", "4"))

  # Helpful error if user supplies only one facet var for a facetted plot
  expect_error(
    p0 + plot_label("oops", 2, 2, panel = list(r = "A")),
    regexp = "Missing:\\s*c"
  )
})

test_that("plot_label works when facet vars use reserved names (colour, size)", {

  df <- data.frame(
    x = 1:8,
    y = 1:8,
    # facet vars intentionally collide with plot_label's internal columns
    colour = ordered(rep(c("B", "A"), each = 4), levels = c("B", "A")),
    size   = factor(rep(c("2", "1"), times = 4), levels = c("2", "1"))
  )

  p0 <- ggplot(df, aes(x, y)) +
    geom_point() +
    facet_grid(colour ~ size)

  # 1) Untargeted labels should not error (and should not mis-detect targeting)
  expect_no_error(
    p0 + plot_label(
      label = c("L1", "L2"),
      x = c(2, 3),
      y = c(2, 3)
    )
  )

  # 2) Targeted labels: provide BOTH facet vars via panel=list(...)
  p_target <- p0 + plot_label(
    label = c("RowB-Col2", "RowA-Col1"),
    x = c(2, 3),
    y = c(2, 3),
    panel = list(
      colour = c("B", "A"),
      size   = c("2", "1")
    )
  )

  built <- ggplot_build(p_target)

  # label layer should be second layer: geom_point then geom_text/geom_label
  lab_data <- built$data[[2]]

  # Should not have been expanded (because panel specified)
  expect_equal(nrow(lab_data), 2L)

  # Facet vars should be present and match what we supplied
  expect_true(all(c("colour", "size") %in% names(lab_data)))
  expect_identical(as.character(lab_data$colour), c("B", "A"))
  expect_identical(as.character(lab_data$size), c("2", "1"))

  # 3) Partial panel specification should error with "Missing: size"
  expect_error(
    p0 + plot_label("oops", 2, 2, panel = list(colour = "A")),
    regexp = "Missing:\\s*size"
  )
})


test_that("Facet order preserved", {
  df <- data.frame(grp = 1:3, val = rep(1, 3))
  df$grp <- ordered(df$grp, levels = 3:1)

  p0 <- ggplot(df, aes(val, val)) +
    geom_point() +
    facet_wrap(~grp)

  p1 <- p0 +
    plot_label("a point", 1.25, 1, panel = list(grp = "1")) +
    scale_x_continuous_e61(c(1, 2))

  lay0 <- ggplot_build(p0)@layout$layout[["grp"]]
  lay1 <- ggplot_build(p1)@layout$layout[["grp"]]

  expect_identical(lay1, lay0)
})
