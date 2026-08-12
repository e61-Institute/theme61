test_that("scale_colour_e61_highlight colours the highlighted level and greys out the rest", {
  data <- data.frame(
    year = rep(2020:2023, 3),
    region = rep(c("NSW", "VIC", "QLD"), each = 4),
    value = c(4, 5, 6, 7, 3, 3, 4, 4, 2, 2, 3, 3)
  )

  p <- ggplot(data, aes(year, value, colour = region)) +
    geom_line() +
    scale_colour_e61_highlight(highlight = "NSW")

  built <- ggplot_build(p)
  colours <- built$data[[1]]$colour
  names(colours) <- data$region

  expect_equal(unique(colours[data$region == "NSW"]), palette_e61(1))
  expect_equal(unique(colours[data$region == "VIC"]), unique(colours[data$region == "QLD"]))
  expect_false(unique(colours[data$region == "VIC"]) %in% palette_e61(1))
})

test_that("A vector of groups can be highlighted at once", {
  data <- data.frame(
    region = c("NSW", "VIC", "QLD"),
    value = c(4, 3, 2)
  )

  p <- ggplot(data, aes(region, value, fill = region)) +
    geom_col() +
    scale_fill_e61_highlight(highlight = c("NSW", "QLD"))

  built <- ggplot_build(p)
  fills <- built$data[[1]]$fill
  names(fills) <- data$region

  expect_length(unique(fills), 3)
  expect_false(fills[["VIC"]] %in% fills[c("NSW", "QLD")])
})

test_that("unhighlighted colour can be customised", {
  data <- data.frame(region = c("NSW", "VIC"), value = c(1, 2))

  p <- ggplot(data, aes(region, value, fill = region)) +
    geom_col() +
    scale_fill_e61_highlight(highlight = "NSW", unhighlighted = "#123456")

  built <- ggplot_build(p)
  fills <- built$data[[1]]$fill
  names(fills) <- data$region

  expect_equal(unname(fills[["VIC"]]), "#123456")
})

test_that("A mapped colour/fill aesthetic is required", {
  data <- data.frame(x = 1:3, y = 1:3)

  expect_error(
    ggplot(data, aes(x, y)) +
      geom_line() +
      scale_colour_e61_highlight(highlight = "a")
  )
})

test_that("Highlighting a value not present in the data warns and greys everything out", {
  data <- data.frame(region = c("NSW", "VIC"), value = c(1, 2))

  expect_warning(
    p <- ggplot(data, aes(region, value, fill = region)) +
      geom_col() +
      scale_fill_e61_highlight(highlight = "WA")
  )

  fills <- unique(ggplot_build(p)$data[[1]]$fill)
  expect_length(fills, 1)
})
