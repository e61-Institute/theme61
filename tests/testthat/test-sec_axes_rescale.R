test_that("Test the function works in isolation", {
  withr::local_options(list(sec_axis_msg = FALSE))

  expect_equal(sec_rescale_inv(c(10, 20, 30), scale = 0.1),
               c(100, 200, 300))

  expect_equal(sec_rescale_inv(c(10, 20, 30), shift = 10),
               c(20, 30, 40))

  expect_equal(sec_rescale(c(100, 200, 300), scale = 0.1, shift = 0),
               c(10, 20, 30))

  expect_equal(sec_rescale(c(20, 30, 40), scale = 1, shift = 10),
               c(10, 20, 30))
})


test_that("Graphs produced with manipulated secondary axes work", {

  skip("These need to be checked interactively.")

  # Note: Rescaled graphs need to be run twice to save the scale/shift

  # Test rescaling
  ggplot(data.frame(x = 1, y1 = 10, y2 = 200), aes(x)) +
    geom_point(aes(y = y1), colour = "red") +
    geom_point(aes(y = sec_rescale_inv(y2, scale = 10))) +
    scale_y_continuous_e61(limits = c(0, 25, 5),
                           sec_axis = sec_axis(~sec_rescale(.), name = "%"),
                           rescale_sec = TRUE) +
    labs_e61(y = "%")

  save_e61(tempfile(fileext = ".svg"))

  # Test shift up and down
  ggplot(data.frame(x = 1, y1 = 10, y2 = 30), aes(x)) +
    geom_point(aes(y = y1), colour = "red") +
    geom_point(aes(y = sec_rescale_inv(y2, shift = -10))) +
    scale_y_continuous_e61(limits = c(0, 25, 5),
                           sec_axis = sec_axis(~sec_rescale(.), name = "%"),
                           rescale_sec = TRUE) +
    labs_e61(y = "%")

  save_e61(tempfile(fileext = ".svg"))

})
