test_that("ggplot2 functions are masked by theme61", {
  withr::local_options(list(quiet_wrap = FALSE))

  suppressMessages(expect_message(ggsave(withr::local_tempfile(fileext = ".svg"), ggplot()), ".*Please use.*"))

  suppressMessages(expect_message(ggsave(withr::local_tempfile(fileext = ".svg"), ggplot() + labs()), ".*Please use.*"))

  suppressMessages(expect_message(ggsave(withr::local_tempfile(fileext = ".svg"), ggplot() + scale_y_continuous()), ".*Please use.*"))
})
