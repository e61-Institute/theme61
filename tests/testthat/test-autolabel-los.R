# Tests for line-of-sight (issue #159): is a candidate label position
# visibly connected to its series, i.e. does the straight line between
# them cross any other ink in the occupancy mask?

test_that("t61_bresenham_points traces horizontal, vertical and diagonal paths correctly", {
  h <- t61_bresenham_points(5, 0, 5, 10)
  expect_true(all(h[, "row"] == 5))
  expect_equal(range(h[, "col"]), c(0, 10))

  v <- t61_bresenham_points(0, 5, 10, 5)
  expect_true(all(v[, "col"] == 5))
  expect_equal(range(v[, "row"]), c(0, 10))

  d <- t61_bresenham_points(0, 0, 10, 10)
  expect_equal(nrow(d), 11)
  expect_true(all(d[, "row"] == d[, "col"]))

  steep <- t61_bresenham_points(0, 0, 10, 2)
  expect_equal(unname(steep[1, ]), c(row = 0, col = 0))
  expect_equal(unname(steep[nrow(steep), ]), c(row = 10, col = 2))
})

test_that("t61_bresenham_points is symmetric under reversal", {
  fwd <- t61_bresenham_points(0, 0, 10, 4)
  rev <- t61_bresenham_points(10, 4, 0, 0)
  expect_identical(fwd, rev[nrow(rev):1, ])
})

test_that("t61_line_of_sight detects a clear path vs one blocked by ink", {
  occ <- matrix(FALSE, nrow = 20, ncol = 20)
  expect_true(t61_line_of_sight(occ, 2, 2, 18, 18))

  occ_blocked <- occ
  occ_blocked[10, ] <- TRUE
  expect_false(t61_line_of_sight(occ_blocked, 2, 2, 18, 18))
})

test_that("t61_line_of_sight ignores ink right at the path endpoints", {
  # The label's own box and the series' own line sit exactly at the two
  # endpoints of every line-of-sight test; skip_ends must trim those or
  # every test would trivially fail.
  occ <- matrix(FALSE, nrow = 20, ncol = 20)
  occ[2, 2] <- TRUE
  occ[18, 18] <- TRUE
  expect_true(t61_line_of_sight(occ, 2, 2, 18, 18))
})
