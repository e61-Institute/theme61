svg_size_in <- function(path) {
  header <- paste(readLines(path, n = 5, warn = FALSE), collapse = " ")
  dims <- as.numeric(regmatches(header, regexec("width='([0-9.]+)pt' height='([0-9.]+)pt'", header))[[1]][2:3])
  stats::setNames(dims / 72, c("width", "height"))
}

test_that("t61_saved_size approximates the size save_e61() saves at", {
  p <- minimal_plot + labs_e61(title = "A title", subtitle = "Subtitle", sources = "ABS")

  saved <- withr::local_tempfile(fileext = ".svg")
  suppressWarnings(save_e61(saved, p, spell_check = FALSE))
  expected <- svg_size_in(saved)

  # save_graph() pads the width by 0.1cm.
  expect_equal(t61_saved_size(p), expected - c(cm_to_in(0.1), 0), tolerance = 0.05)
})

test_that("text scale fits the saved chart inside the figure", {
  ref <- t61_saved_size(minimal_plot)

  expect_equal(t61_knit_text_scale(minimal_plot, ref[["width"]], ref[["height"]]), 1)
  expect_equal(t61_knit_text_scale(minimal_plot, 2 * ref[["width"]], 2 * ref[["height"]]), 2)

  # The relatively tighter dimension wins, whether that's width or height.
  expect_equal(t61_knit_text_scale(minimal_plot, 4 * ref[["width"]], ref[["height"]]), 1)
  expect_equal(t61_knit_text_scale(minimal_plot, ref[["width"]], 4 * ref[["height"]]), 1)

  # Facets are laid out across the full column.
  faceted <- minimal_plot + facet_wrap(~x)
  expect_equal(t61_saved_size(faceted)[["width"]], 2 * ref[["width"]])

  expect_equal(t61_knit_text_scale(minimal_plot, NULL, NULL), 1)
})

test_that("inline font sizes from labs_e61() are scaled, keeping attributes", {
  x <- structure("<span style='font-size:10pt'>A</span><br><span style='font-size:9pt'>B</span>", subtitle_wrap = TRUE)
  out <- t61_scale_inline_pt(x, 2)

  expect_equal(as.vector(out), "<span style='font-size:20pt'>A</span><br><span style='font-size:18pt'>B</span>")
  expect_true(attr(out, "subtitle_wrap"))
  expect_identical(t61_scale_inline_pt("plain", 2), "plain")
  expect_null(t61_scale_inline_pt(NULL, 2))
})

test_that("t61_scale_text scales theme text and text layers without mutating the original", {
  p <- minimal_plot_label + plot_label("Label", x = 1, y = 1)
  label_layer <- length(p@layers)
  base <- p@theme$text$size
  label_size <- p@layers[[label_layer]]$aes_params$size

  scaled <- t61_scale_text(p, 2)

  expect_equal(scaled@theme$text$size, 2 * base)
  expect_equal(scaled@layers[[label_layer]]$aes_params$size, 2 * label_size)

  expect_equal(p@theme$text$size, base)
  expect_equal(p@layers[[label_layer]]$aes_params$size, label_size)

  expect_identical(t61_scale_text(p, 1), p)
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

test_that("knitted text is scaled unless theme61.knit_scale_text = FALSE", {
  withr::local_dir(withr::local_tempdir())
  dir.create("on")
  dir.create("off")

  on <- knit_chart("on")
  withr::with_options(list(theme61.knit_scale_text = FALSE), off <- knit_chart("off"))

  expect_false(unname(tools::md5sum(on$fig)) == unname(tools::md5sum(off$fig)))
})
