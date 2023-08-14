test_that("Functionality in labs_e61 works", {

  # Test aesthetic tic marks

  # Test correct function
  expect_equal(get_aes_ticks(0, 50), 10)
  expect_equal(get_aes_ticks(0, 60), 15)
  expect_equal(get_aes_ticks(20, 60), 10)
  expect_equal(get_aes_ticks(20, 30), 2)
  expect_equal(get_aes_ticks(-20, 30), 10)
  expect_equal(get_aes_ticks(-10, 30), 10)
  expect_equal(get_aes_ticks(-15, 30), 15)
  expect_equal(get_aes_ticks(-30, 30), 15)
  expect_equal(get_aes_ticks(-5, 30), 5)
  expect_equal(get_aes_ticks(-5, 5), 5)
  expect_equal(get_aes_ticks(-50, -20), 10)
  expect_equal(get_aes_ticks(-100, -20), 20)
  expect_equal(get_aes_ticks(20, 100), 20)
  expect_equal(get_aes_ticks(25, 100), 15)
  expect_equal(get_aes_ticks(40, 100), 15)
  expect_equal(get_aes_ticks(100, 40), 15)
  expect_equal(get_aes_ticks(4, 44), 10)

  # Alternate orders of magnitude
  expect_equal(get_aes_ticks(25, 27), 0.5)
  expect_equal(get_aes_ticks(0.25, 0.27), 0.005)
  expect_equal(get_aes_ticks(2500, 2700), 50)

  # Test errors are produced correctly
  expect_equal(get_aes_ticks(-15, 40), NULL)
  expect_equal(get_aes_ticks(-40, 15), NULL)
  expect_equal(get_aes_ticks(5, 5), NULL)

  # Test error - too many breaks
  expect_equal(get_aes_ticks(-40, 4), NULL)
  expect_equal(get_aes_ticks(-4, 40), NULL)


  # Test aesthetic pairing

  # Test same side
  expect_equal(get_aes_pair(4, 44), list(0, 50))
  expect_equal(get_aes_pair(11, 44), list(10, 50))
  expect_equal(get_aes_pair(16, 74), list(15, 75))
  expect_equal(get_aes_pair(-16, -74), list(-15, -75))
  expect_equal(get_aes_pair(-1, -14), list(0, -15))
  expect_equal(get_aes_pair(-10, -14), list(-10, -15))
  expect_equal(get_aes_pair(-20, -14), list(0, -25))

  # Testing opposite sides
  expect_equal(get_aes_pair(-4, 44), list(-10, 50))
  expect_equal(get_aes_pair(-0.4, 44), list(-10, 50))
  expect_equal(get_aes_pair(-44, 44), list(-50, 50))

  # Test orders of magnitude
  expect_equal(get_aes_pair(-0.44, 0.44), list(-0.50, 0.50))
  expect_equal(get_aes_pair(-4400, 4400), list(-5000, 5000))
  expect_equal(get_aes_pair(160, 740), list(150, 750))


  # Test aesthetic numbers

  # Testing basic function
  expect_equal(get_aes_num(87), 100)
  expect_equal(get_aes_num(101), 120)
  expect_equal(get_aes_num(987), 1000)
  expect_equal(get_aes_num(32), 40)
  expect_equal(get_aes_num(0.029), 0.03)
  expect_equal(get_aes_num(0.29), 0.3)

  expect_equal(get_aes_num(-87), -100)
  expect_equal(get_aes_num(-101), -120)
  expect_equal(get_aes_num(-987), -1000)
  expect_equal(get_aes_num(-0.029), -0.03)
  expect_equal(get_aes_num(-0.29), -0.3)

  # Test the type argument
  expect_equal(get_aes_num(5, type = "next_smallest"), 4)
  expect_equal(get_aes_num(9, type = "next_smallest"), 8)
  expect_equal(get_aes_num(21, type = "next_smallest"), 20)
  expect_equal(get_aes_num(17, type = "next_smallest"), 16)

  expect_equal(get_aes_num(1700, type = "next_smallest"), 1600)
  expect_equal(get_aes_num(0.17, type = "next_smallest"), 0.16)

})
