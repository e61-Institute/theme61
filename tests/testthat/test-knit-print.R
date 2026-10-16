svg_size_in <- function(path) {
  header <- paste(readLines(path, n = 5, warn = FALSE), collapse = " ")
  dims <- as.numeric(regmatches(header, regexec("width='([0-9.]+)pt' height='([0-9.]+)pt'", header))[[1]][2:3])
  stats::setNames(dims / 72, c("width", "height"))
}

test_that("t61_saved_size approximates the size save_e61() saves at", {
  labelled <- minimal_plot + labs_e61(title = "A title", subtitle = "Subtitle", sources = "ABS")
  bars <- ggplot(data.frame(k = c("Housing", "Food"), v = c(30, 18)), aes(k, v)) + geom_col()

  for (p in list(labelled, bars + coord_flip(), labelled + facet_wrap(~x),
                 labelled + facet_wrap(~x, ncol = 1))) {
    saved <- withr::local_tempfile(fileext = ".svg")
    suppressWarnings(save_e61(saved, p, spell_check = FALSE))

    # save_graph() pads the width by 0.1cm.
    expect_equal(t61_saved_size(p), svg_size_in(saved) - c(cm_to_in(0.1), 0), tolerance = 0.05)
  }
})

test_that("scale fits the saved chart inside the figure", {
  ref <- t61_saved_size(minimal_plot)

  expect_equal(t61_knit_scale(minimal_plot, ref[["width"]], ref[["height"]]), 1)
  expect_equal(t61_knit_scale(minimal_plot, 2 * ref[["width"]], 2 * ref[["height"]]), 2)

  # The relatively tighter dimension wins, whether that's width or height.
  expect_equal(t61_knit_scale(minimal_plot, 4 * ref[["width"]], ref[["height"]]), 1)
  expect_equal(t61_knit_scale(minimal_plot, ref[["width"]], 4 * ref[["height"]]), 1)


})

test_that("inline font sizes from labs_e61() are scaled, keeping attributes", {
  x <- structure("<span style='font-size:10pt'>A</span><br><span style='font-size:9pt'>B</span>", subtitle_wrap = TRUE)
  out <- t61_scale_inline_pt(x, 2)

  expect_equal(as.vector(out), "<span style='font-size:20pt'>A</span><br><span style='font-size:18pt'>B</span>")
  expect_true(attr(out, "subtitle_wrap"))
  expect_identical(t61_scale_inline_pt("plain", 2), "plain")
  expect_null(t61_scale_inline_pt(NULL, 2))
})

test_that("t61_rescale_plot scales every absolute size without mutating the original", {
  p <- finalise_e61_plot(
    minimal_plot_label + geom_line(linewidth = 1) + plot_label("Label", x = 1, y = 1)
  )
  line_layer <- length(p@layers) - 1
  label_layer <- length(p@layers)
  orig <- ggplot2::complete_theme(p@theme)
  label_size <- p@layers[[label_layer]]$aes_params$size

  scaled <- t61_rescale_plot(p, 2)
  th <- scaled@theme

  expect_equal(th$text$size, 2 * orig$text$size)
  expect_equal(th$line$linewidth, 2 * orig$line$linewidth)
  expect_equal(th$axis.line.x$linewidth, 2 * orig$axis.line.x$linewidth)
  expect_equal(as.numeric(th$plot.margin), 2 * as.numeric(orig$plot.margin))
  expect_equal(th$geom$linewidth, 2 * orig$geom$linewidth)
  expect_equal(th$geom$pointsize, 2 * orig$geom$pointsize)

  # Relative sizes already follow the base size.
  expect_s3_class(th$axis.text$size, "rel")
  expect_equal(unclass(th$axis.text$size), unclass(orig$axis.text$size))

  expect_equal(scaled@layers[[line_layer]]$aes_params$linewidth, 2)
  expect_equal(scaled@layers[[label_layer]]$aes_params$size, 2 * label_size)

  expect_equal(p@layers[[line_layer]]$aes_params$linewidth, 1)
  expect_equal(p@layers[[label_layer]]$aes_params$size, label_size)

  expect_identical(t61_rescale_plot(p, 1), p)
})

knit_chart <- function(dir, chunk_opts = "") {
  rmd <- file.path(dir, "doc.Rmd")
  writeLines(c(paste0("```{r chart", chunk_opts, "}"), "p", "```",
               "```{r after}", "after <- ggplot2::last_plot()", "```"), rmd)
  env <- list2env(list(p = minimal_plot), parent = globalenv())
  withr::with_dir(dir, suppressWarnings(knitr::knit("doc.Rmd", "doc.md", quiet = TRUE, envir = env)))

  list(fig = list.files(file.path(dir, "figure"), full.names = TRUE), env = env)
}

test_that("knitting draws on knitr's own device and leaves last_plot() unscaled", {
  withr::local_dir(withr::local_tempdir())
  out <- knit_chart(".")

  expect_length(out$fig, 1)
  expect_equal(out$env$after@theme$text$size, minimal_plot@theme$text$size)
})

test_that("knitted charts are rescaled unless theme61.knit_rescale = FALSE", {
  withr::local_dir(withr::local_tempdir())
  dir.create("on")
  dir.create("off")

  on <- knit_chart("on")
  withr::with_options(list(theme61.knit_rescale = FALSE), off <- knit_chart("off"))

  expect_false(unname(tools::md5sum(on$fig)) == unname(tools::md5sum(off$fig)))
})
