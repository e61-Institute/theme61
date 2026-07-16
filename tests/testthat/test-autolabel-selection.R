# Tests for candidate generation and selection scoring (issue #159).

test_that("t61_candidate_grid excludes the margin and covers the range", {
  grid <- t61_candidate_grid(c(0, 100), c(0, 10), n_x = 5, n_y = 5, margin = 0.1)

  expect_equal(nrow(grid), 25)
  expect_gte(min(grid$x), 10)
  expect_lte(max(grid$x), 90)
  expect_gte(min(grid$y), 1)
  expect_lte(max(grid$y), 9)
})

test_that("t61_distance_tier buckets relative to label height", {
  expect_equal(t61_distance_tier(0.1, label_height_cm = 1), 1L)
  expect_equal(t61_distance_tier(0.6, label_height_cm = 1), 2L)
  expect_equal(t61_distance_tier(1.2, label_height_cm = 1), 3L)
  expect_equal(t61_distance_tier(3,   label_height_cm = 1), 4L)

  # Thresholds scale with label height, not an absolute cm value
  expect_equal(t61_distance_tier(0.6, label_height_cm = 2), 1L)
})

test_that("t61_select_best_candidate prefers close + clear line of sight", {
  candidates <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 2, 3),
    distance_cm = c(0.2, 0.1, 5),
    next_closest_cm = c(10, 10, 10),
    los = c(TRUE, FALSE, TRUE)
  )

  best <- t61_select_best_candidate(candidates, label_height_cm = 1)
  expect_equal(best$x, 1) # tier 1 + clear LOS beats an even-closer blocked candidate
})

test_that("t61_select_best_candidate penalises ambiguous placement near another series", {
  candidates <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    distance_cm = c(0.3, 0.35),
    next_closest_cm = c(0.1, 5), # candidate 1 is actually closer to a different series
    los = c(TRUE, TRUE)
  )

  best <- t61_select_best_candidate(candidates, label_height_cm = 1)
  expect_equal(best$x, 2)
})

test_that("t61_select_best_candidate returns NULL for an empty candidate set", {
  empty <- data.frame(x = numeric(), y = numeric(), distance_cm = numeric(),
                       next_closest_cm = numeric(), los = logical())
  expect_null(t61_select_best_candidate(empty, label_height_cm = 1))
})
