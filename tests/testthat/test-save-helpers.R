test_that("check_spelling() treats words in the custom dictionary as correctly spelled", {
  expect_null(check_spelling("e61"))
  expect_match(check_spelling("Thsi has a typo"), "Thsi")
})

test_that("make_preview_svg copies the saved svg when svg was one of the saved formats", {
  g <- minimal_plot

  withr::with_tempdir({
    save_graph(graph = g, format = "svg", filename = "plot",
                width = 10, height = 10, bg_colour = "white", res = 1)

    preview <- make_preview_svg(graph = g, format = c("svg", "pdf"), filename = "plot",
                                 width = 10, height = 10, bg_colour = "white", res = 1)

    expect_true(file.exists(preview))
    expect_match(preview, "\\.svg$")
    expect_identical(unname(tools::md5sum("plot.svg")), unname(tools::md5sum(preview)))
  })
})

test_that("make_preview_svg renders a fresh svg when svg was not a saved format", {
  g <- minimal_plot

  withr::with_tempdir({
    save_graph(graph = g, format = "pdf", filename = "plot",
                width = 10, height = 10, bg_colour = "white", res = 1)

    expect_false(file.exists("plot.svg"))

    preview <- make_preview_svg(graph = g, format = "pdf", filename = "plot",
                                 width = 10, height = 10, bg_colour = "white", res = 1)

    expect_true(file.exists(preview))
    expect_match(preview, "\\.svg$")
  })
})

test_that("make_preview_svg renders a fresh svg when only png was a saved format", {
  g <- minimal_plot

  withr::with_tempdir({
    save_graph(graph = g, format = "png", filename = "plot",
                width = 10, height = 10, bg_colour = "white", res = 1)

    expect_false(file.exists("plot.svg"))

    preview <- make_preview_svg(graph = g, format = "png", filename = "plot",
                                 width = 10, height = 10, bg_colour = "white", res = 1)

    expect_true(file.exists(preview))
    expect_match(preview, "\\.svg$")
  })
})
