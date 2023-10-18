test_that("ggplot2 functions are masked by theme61", {
  withr::local_options(list(quiet_wrap = FALSE))

  # Check if labs() throws a msg
  expect_message(suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), minimal_plot + labs()), ".*Please use."))

  # Check if ggsave() throws a msg
  expect_message(suppressWarnings(ggsave(withr::local_tempfile(fileext = ".svg"), ggplot() + labs()), ".*Please use.*"))
})
