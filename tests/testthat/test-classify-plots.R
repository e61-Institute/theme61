test_that("as_e61_plot() coerces ggplot -> e61_plot (and is idempotent)", {

  p0 <- ggplot2::ggplot() + geom_point(aes(1, 1))

  p1 <- as_e61_plot(p0)
  expect_true(inherits(p1, "e61_plot"))
  expect_true(inherits(p1, "ggplot"))

  # e61_plot should be first for method priority
  expect_identical(class(p1)[1], "e61_plot")

  # idempotent
  p2 <- as_e61_plot(p1)
  expect_identical(class(p2), class(p1))
})

test_that("as_e61_plot() errors on unsupported inputs", {

  expect_error(as_e61_plot(1), "e61", ignore.case = TRUE)
  expect_error(as_e61_plot("x"), "e61", ignore.case = TRUE)
  expect_error(as_e61_plot(list(a = 1)), "e61", ignore.case = TRUE)
})

test_that("as_e61_plot() works elementwise for lists of ggplots", {

  plots <- list(
    ggplot() + geom_point(aes(1, 1)),
    ggplot() + geom_point(aes(1, 1))
  )

  out <- lapply(plots, as_e61_plot)
  expect_true(all(vapply(out, inherits, logical(1), what = "e61_plot")))
  expect_true(all(vapply(out, inherits, logical(1), what = "ggplot")))
})

test_that("classify_e61_map() adds e61_map iff plot is spatial (GeomSf heuristic)", {

  # Non-spatial plot should not gain e61_map
  p_plain <- ggplot() + geom_point(aes(1, 1))
  p_plain2 <- classify_e61_map(p_plain)

  expect_false(inherits(p_plain2, "e61_map"))

  # Spatial plot: emulate a geom whose class contains "GeomSf" without requiring sf
  p_sf <- ggplot() + geom_point(aes(1, 1))
  p_sf@layers <- c(
    p_sf@layers,
    list(list(geom = structure(list(), class = "GeomSf")))
  )

  p_sf2 <- classify_e61_map(p_sf)

  expect_true(inherits(p_sf2, "e61_map"))
  expect_true(inherits(p_sf2, "e61_plot"))

  # Map class should take priority over e61_plot
  expect_identical(class(p_sf2)[1:2], c("e61_map", "e61_plot"))

  # Calling classify again should be idempotent
  p_sf3 <- classify_e61_map(p_sf2)
  expect_identical(class(p_sf3), class(p_sf2))

  # It should still be a ggplot
  expect_true(inherits(p_sf2, "ggplot"))
})

test_that("classify_e61_map() preserves/ensures e61_plot when tagging e61_map", {

  # Start as an e61_plot (i.e. already coerced)
  p <- as_e61_plot(ggplot() + geom_point(aes(1, 1)))

  # Make it "spatial" via GeomSf heuristic
  p@layers <- c(
    p@layers,
    list(list(geom = structure(list(), class = "GeomSf")))
  )

  out <- classify_e61_map(p)

  expect_identical(class(out)[1:2], c("e61_map", "e61_plot"))
})

test_that("classify_e61_map() works for lists (including mixed ggplot/e61_plot/e61_map)", {

  # plain ggplot
  p1 <- ggplot() + geom_point(aes(1, 1))

  # e61_plot
  p2 <- as_e61_plot(ggplot() + geom_point(aes(1, 1)))

  # "spatial" ggplot (should become e61_map)
  p3 <- ggplot() + geom_point(aes(1, 1))
  p3@layers <- c(p3@layers, list(list(geom = structure(list(), class = "GeomSf"))))

  # already e61_map (build from p3 then classify once)
  p4 <- classify_e61_map(p3)

  out <- classify_e61_map(list(p1, p2, p3, p4))

  expect_length(out, 4)
  expect_false(inherits(out[[1]], "e61_map"))
  expect_false(inherits(out[[2]], "e61_map"))
  expect_true(inherits(out[[3]], "e61_map"))
  expect_true(inherits(out[[4]], "e61_map"))

  # For spatial ones, ensure map priority ordering
  expect_identical(class(out[[3]])[1:2], c("e61_map", "e61_plot"))
  expect_identical(class(out[[4]])[1:2], c("e61_map", "e61_plot"))
})

test_that("classify_e61_map() errors on unsupported inputs", {

  expect_error(classify_e61_map(1), "e61", ignore.case = TRUE)
  expect_error(classify_e61_map("x"), "e61", ignore.case = TRUE)
})
