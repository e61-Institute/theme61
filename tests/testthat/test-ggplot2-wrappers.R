test_that("ggplot2 functions are masked by theme61", {
  expect_message(ggsave(withr::local_tempfile(fileext = ".svg"), ggplot()), ".*Please use.*")

  expect_message(ggsave(withr::local_tempfile(fileext = ".svg"), ggplot() + labs()), ".*Please use.*")

  expect_message(ggsave(withr::local_tempfile(fileext = ".svg"), ggplot() + scale_y_continuous()), ".*Please use.*")
})
