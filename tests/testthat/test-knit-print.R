render_chunk <- function(plot, output_format, dir) {
  rmd <- file.path(dir, "doc.Rmd")
  writeLines(c("```{r chart, echo = FALSE}", "p", "```"), rmd)

  suppressWarnings(
    rmarkdown::render(rmd, output_format = output_format, quiet = TRUE,
                      envir = list2env(list(p = plot), parent = globalenv()))
  )
}

svg_width <- function(path) {
  header <- paste(readLines(path, n = 5, warn = FALSE), collapse = " ")
  as.numeric(sub(".*?width='([0-9.]+)pt'.*", "\\1", header, perl = TRUE))
}

test_that("knitted HTML embeds the chart exactly as save_e61() saves it", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  dir <- withr::local_tempdir()
  html <- render_chunk(minimal_plot, rmarkdown::html_document(self_contained = FALSE), dir)

  fig <- list.files(dir, pattern = "^chart-e61-[0-9a-f]{8}\\.svg$", recursive = TRUE, full.names = TRUE)
  expect_length(fig, 1)

  saved <- withr::local_tempfile(fileext = ".svg")
  suppressWarnings(save_e61(saved, minimal_plot, spell_check = FALSE))
  expect_equal(svg_width(fig), svg_width(saved))

  # Displayed at the saved physical width, not stretched to fig.width.
  expect_true(any(grepl("chart-e61-[0-9a-f]{8}\\.svg.*width:\\s*[0-9.]+in", readLines(html))))
})

test_that("non-HTML, non-LaTeX output gets a PNG, and out.width is respected", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  dir <- withr::local_tempdir()
  rmd <- file.path(dir, "doc.Rmd")
  writeLines(c("```{r chart, echo = FALSE, out.width = '50%'}", "p", "```"), rmd)
  md <- suppressWarnings(rmarkdown::render(rmd, output_format = rmarkdown::md_document(), quiet = TRUE,
                                           envir = list2env(list(p = minimal_plot), parent = globalenv())))

  expect_length(list.files(dir, pattern = "^chart-e61-[0-9a-f]{8}\\.png$", recursive = TRUE), 1)
  expect_true(any(grepl("50%", readLines(md))))
})

test_that("theme61.knit_as_saved = FALSE falls back to knitr's own device", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  withr::local_options(list(theme61.knit_as_saved = FALSE))

  dir <- withr::local_tempdir()
  render_chunk(minimal_plot, rmarkdown::html_document(self_contained = FALSE), dir)

  expect_length(list.files(dir, pattern = "-e61-", recursive = TRUE), 0)
  expect_length(list.files(dir, pattern = "^chart-1\\.png$", recursive = TRUE), 1)
})

test_that("different plots in one chunk get distinct files", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  dir <- withr::local_tempdir()
  rmd <- file.path(dir, "doc.Rmd")
  writeLines(c("```{r chart, echo = FALSE}", "p", "p + ggplot2::geom_line()", "```"), rmd)
  suppressWarnings(rmarkdown::render(rmd, output_format = rmarkdown::html_document(self_contained = FALSE),
                                     quiet = TRUE, envir = list2env(list(p = minimal_plot), parent = globalenv())))

  expect_length(list.files(dir, pattern = "^chart-e61-[0-9a-f]{8}\\.svg$", recursive = TRUE), 2)
})
