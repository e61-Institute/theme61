test_that("palette_e61 returns palettes of the expected size", {
  expect_equal(palette_e61(1), e61_tealdark1)
  expect_length(palette_e61(4), 4)
  expect_length(palette_e61(12), 12)
})

test_that("palette_e61 reverse option returns reversed palette", {
  pal <- palette_e61(5)
  expect_equal(palette_e61(5, reverse = TRUE), rev(pal))
})

test_that("palette_e61 validates palette size", {
  expect_error(palette_e61(0))
  expect_error(palette_e61(13))
})

test_that("get_palette() validates n directly (no upstream validation from scale functions)", {
  expect_error(theme61:::get_palette("a"), "does not support")
  expect_error(theme61:::get_palette(-1), "does not support")
  expect_error(theme61:::get_palette(0), "does not support")
  expect_error(theme61:::get_palette(2.5), "does not support")
  expect_error(theme61:::get_palette(13), "does not support")
  expect_error(theme61:::get_palette(c(1, 2)), "does not support")
  expect_error(theme61:::get_palette(NA), "does not support")
})

test_that("get_palette() returns the right number of hex colours for valid n", {
  for (n in 1:12) {
    pal <- theme61:::get_palette(n)
    expect_length(pal, n)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
  }
})

test_that("scale_colour_e61()/scale_fill_e61() surface get_palette()'s validation error", {
  df <- data.frame(x = 1:13, y = 1:13, g = factor(1:13))
  p <- ggplot(df, aes(x, y, colour = g)) + geom_point() + scale_colour_e61()

  expect_error(ggplot_build(p), "does not support")
})

test_that("e61_pal() returns a palette-generating function", {
  pal_fun <- theme61:::e61_pal(palette = "light")
  expect_type(pal_fun, "closure")
})

test_that("e61_pal() generates n valid hex colours", {
  pal_fun <- theme61:::e61_pal(palette = "dark")
  cols <- pal_fun(5)
  expect_length(cols, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("e61_pal() reverse = TRUE reverses the underlying colour ramp", {
  fwd <- theme61:::e61_pal(palette = "light", reverse = FALSE)(3)
  rev_cols <- theme61:::e61_pal(palette = "light", reverse = TRUE)(3)

  # the ramp is reversed, so anchor colours should come out swapped
  expect_equal(fwd[1], rev_cols[3])
  expect_equal(fwd[3], rev_cols[1])
})

test_that("e61_pal() errors for an unknown palette name", {
  expect_error(theme61:::e61_pal(palette = "not_a_palette"))
})

test_that("gen_palette() generates an 8-colour gradient ending near the supplied colour", {
  grad <- theme61:::gen_palette("#0d8982")
  expect_length(grad, 1)
  expect_length(grad[[1]], 8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", grad[[1]])))
  # first colour in the ramp should be exactly the supplied colour
  expect_equal(toupper(grad[[1]][1]), "#0D8982")
})

test_that("gen_palette() supports multiple input colours", {
  grad <- theme61:::gen_palette(c("#0d8982", "#10485E"))
  expect_length(grad, 2)
  expect_length(grad[[1]], 8)
  expect_length(grad[[2]], 8)
})

test_that("named e61 colour objects hold the expected hex codes", {
  expect_equal(toupper(e61_tealdark1), "#0D8982")
  expect_equal(toupper(e61_bluedark1), "#10485E")
  expect_equal(e61_boxback, "#ecf9fa")
})

test_that("e61_aus_colours has expected named entries and hex values", {
  expect_true(all(c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT", "AUS") %in%
                    names(e61_aus_colours)))
  expect_equal(unname(e61_aus_colours["NSW"]), "#4A90E2")
  expect_equal(unname(e61_aus_colours["Sydney"]), unname(e61_aus_colours["NSW"]))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", e61_aus_colours)))
})
