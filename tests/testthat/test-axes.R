test_that("Functionality in auto y-axis scaling works", {

  # Test aesthetic limits (combines all four functions) ----

  # Bug specific tests - things that testers have broken
  expect_equal(get_aes_limits(2.2, 4.01), list(2, 4.5, 0.5))

  # Test same side
  expect_equal(get_aes_limits(4, 44), list(0, 45, 15))
  expect_equal(get_aes_limits(11, 44), list(10, 45, 5))
  expect_equal(get_aes_limits(16, 74), list(15, 75, 10))
  expect_equal(get_aes_limits(-74, -16), list(-75, -15, 10))
  expect_equal(get_aes_limits(-14, -1), list(-15, 0, 5))
  expect_equal(get_aes_limits(-14, -10), list(-15, -9, 1))
  expect_equal(get_aes_limits(-20, -14), list(-25, -10, 5))
  expect_equal(get_aes_limits(0, 11.5), list(0, 15, 5))

  # Testing opposite sides
  expect_equal(get_aes_limits(-4, 44), list(-10, 50, 10))
  expect_equal(get_aes_limits(-0.4, 44), list(-10, 50, 10))
  expect_equal(get_aes_limits(-44, 44), list(-45, 45, 15))

  # Test the from zero functionality
  expect_equal(get_aes_limits(4, 44, from_zero = T), list(0, 45, 15))
  expect_equal(get_aes_limits(11, 44, from_zero = T), list(0, 45, 15))
  expect_equal(get_aes_limits(16, 74, from_zero = T), list(0, 75, 15))
  expect_equal(get_aes_limits(-16, -74, from_zero = T), list(-75, 0, 15))
  expect_equal(get_aes_limits(-14, -1, from_zero = T), list(-15, 0, 5))
  expect_equal(get_aes_limits(-14, -10, from_zero = T), list(-15, 0, 5))
  expect_equal(get_aes_limits(-20, -14, from_zero = T), list(-25, 0, 5))

  # Test orders of magnitude
  expect_equal(get_aes_limits(-0.44, 0.44), list(-0.45, 0.45, 0.15))
  expect_equal(get_aes_limits(-4400, 4400), list(-4500, 4500, 1500))
  expect_equal(get_aes_limits(160, 740), list(150, 750, 100))


  # Test aesthetic tick marks ----

  # Test correct function
  expect_equal(get_aes_ticks(0, 50), 10)
  expect_equal(get_aes_ticks(0, 60), 10)
  expect_equal(get_aes_ticks(20, 60), 10)
  expect_equal(get_aes_ticks(20, 30), 2)
  expect_equal(get_aes_ticks(-20, 30), 10)
  expect_equal(get_aes_ticks(-10, 30), 10)
  expect_equal(get_aes_ticks(-15, 30), 15)
  expect_equal(get_aes_ticks(-30, 30), 10)
  expect_equal(get_aes_ticks(-5, 30), 5)
  expect_equal(get_aes_ticks(-5, 5), 2.5)
  expect_equal(get_aes_ticks(-50, -20), 5)
  expect_equal(get_aes_ticks(-100, -20), 20)
  expect_equal(get_aes_ticks(20, 100), 20)
  expect_equal(get_aes_ticks(25, 100), 15)
  expect_equal(get_aes_ticks(40, 100), 10)
  expect_equal(get_aes_ticks(100, 40), 10)
  expect_equal(get_aes_ticks(4, 44), 10)
  expect_equal(get_aes_ticks(10, 50), 10)

  # Alternate orders of magnitude
  expect_equal(get_aes_ticks(25, 27), 0.5)
  expect_equal(get_aes_ticks(0.25, 0.27), 0.005)
  expect_equal(get_aes_ticks(2500, 2700), 50)
  expect_equal(get_aes_ticks(-0.5, 0.5), 0.25)

  # Test errors are produced correctly
  expect_equal(get_aes_ticks(-15, 40), NULL)
  expect_equal(get_aes_ticks(-40, 15), NULL)
  expect_equal(get_aes_ticks(5, 5), NULL)

  # Test error - too many breaks
  expect_equal(get_aes_ticks(-40, 4), NULL)
  expect_equal(get_aes_ticks(-4, 40), NULL)


  # Test aesthetic pairing ----

  # Test same side
  expect_equal(get_aes_pair(4, 44), list(0, 45))
  expect_equal(get_aes_pair(11, 44), list(10, 45))
  expect_equal(get_aes_pair(16, 74), list(15, 75))
  expect_equal(get_aes_pair(-16, -74), list(-75, -15))
  expect_equal(get_aes_pair(-1, -14), list(-15, 0))
  expect_equal(get_aes_pair(-10, -14), list(-15, -9))
  expect_equal(get_aes_pair(-20, -14), list(-25, -10))

  # Testing opposite sides
  expect_equal(get_aes_pair(-4, 44), list(-10, 50))
  expect_equal(get_aes_pair(-0.4, 44), list(-10, 50))
  expect_equal(get_aes_pair(-44, 44), list(-45, 45))

  # Test orders of magnitude
  expect_equal(get_aes_pair(-0.44, 0.44), list(-0.45, 0.45))
  expect_equal(get_aes_pair(-4400, 4400), list(-4500, 4500))
  expect_equal(get_aes_pair(160, 740), list(150, 750))


  # Test aesthetic numbers ----

  # Testing basic function
  expect_equal(get_aes_num(87, diff = 100, type = "next_largest"), 90)
  expect_equal(get_aes_num(101, diff = 100, type = "next_largest"), 150)
  expect_equal(get_aes_num(987, diff = 100, type = "next_largest"), 1000)

  expect_equal(get_aes_num(32, diff = 100, type = "next_largest"), 35)
  expect_equal(get_aes_num(0.029, diff = 100, type = "next_largest"), 0.03)
  expect_equal(get_aes_num(0.29, diff = 100, type = "next_largest"), 0.3)

  expect_equal(get_aes_num(-87, diff = 100, type = "next_largest"), -90)
  expect_equal(get_aes_num(-101, diff = 100, type = "next_largest"), -150)
  expect_equal(get_aes_num(-987, diff = 100, type = "next_largest"), -1000)
  expect_equal(get_aes_num(-0.029, diff = 100, type = "next_largest"), -0.03)
  expect_equal(get_aes_num(-0.29, diff = 100, type = "next_largest"), -0.3)

  # Test the difference argument
  expect_equal(get_aes_num(8.641, diff = 0.009, type = "next_largest"), 8.6415)
  expect_equal(get_aes_num(864.1, diff = 0.9, type = "next_largest"), 864.15)
  expect_equal(get_aes_num(864.09, diff = 0.9, type = "next_largest"), 864.10)
  expect_equal(get_aes_num(1010, diff = 20, type = "next_largest"), 1015)
  expect_equal(get_aes_num(1009, diff = 20, type = "next_largest"), 1010)
  expect_equal(get_aes_num(109, diff = 20, type = "next_largest"), 110)
  expect_equal(get_aes_num(109, diff = 200, type = "next_largest"), 150)
  expect_equal(get_aes_num(101, diff = 100, type = "next_largest"), 150)
  expect_equal(get_aes_num(101, diff = 90, type = "next_largest"), 110)

  # Test the type argument
  expect_equal(get_aes_num(5, diff = 90, type = "next_smallest"), 4.5)
  expect_equal(get_aes_num(9, diff = 90, type = "next_smallest"), 8)
  expect_equal(get_aes_num(21, diff = 90, type = "next_smallest"), 20)
  expect_equal(get_aes_num(17, diff = 90, type = "next_smallest"), 15)

  expect_equal(get_aes_num(1700, diff = 9000, type = "next_smallest"), 1500)
  expect_equal(get_aes_num(0.17, diff = 90, type = "next_smallest"), 0.15)
})

test_that("Unsuitable custom limits throw error message", {

  data <- data.frame(x = seq(1, 10, 1), y = runif(10, 3, 6))

  ggplot(data, aes(x, y)) +
    geom_point() +
    scale_y_continuous_e61(limits = c(0, 10, 2))

  ggplot(data, aes(x, y)) +
    geom_point() +
    scale_y_continuous_e61(limits = c(0, 2, 1))


})
