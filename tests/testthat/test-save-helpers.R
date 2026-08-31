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

test_that("svg_to_bitmap() does not write intermed.svg/intermed.<fmt> into the working directory (#354)", {
  g <- minimal_plot

  withr::with_tempdir({
    save_graph(graph = g, format = "svg", filename = "plot",
                width = 10, height = 10, bg_colour = "white", res = 1)

    out <- svg_to_bitmap("plot.svg", "output.png", res = 2)

    expect_true(file.exists("output.png"))
    expect_true(file.exists(out))

    # The buggy version wrote these hardcoded relative paths into whatever
    # the current working directory happened to be.
    expect_false(file.exists("intermed.svg"))
    expect_false(file.exists("intermed.png"))

    # Only the input svg and the requested output should be present.
    expect_setequal(list.files(), c("plot.svg", "output.png"))
  })
})

test_that("svg_to_bitmap() rescales output regardless of the res argument passed in", {
  g <- minimal_plot

  withr::with_tempdir({
    save_graph(graph = g, format = "svg", filename = "plot",
                width = 10, height = 10, bg_colour = "white", res = 1)

    out_default <- svg_to_bitmap("plot.svg", "default.png", res = 1)
    out_double <- svg_to_bitmap("plot.svg", "double.png", res = 2)

    info_default <- magick::image_info(magick::image_read("default.png"))
    info_double <- magick::image_info(magick::image_read("double.png"))

    expect_equal(info_double$width, info_default$width * 2)
    expect_equal(info_double$height, info_default$height * 2)
  })
})
