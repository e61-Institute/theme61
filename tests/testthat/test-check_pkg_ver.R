test_that("Interactive prompt works", {
  testthat::local_mocked_bindings(
    gh = function(...) list(list(tag_name = "v0.9.0")),
    .env = asNamespace("gh")
  )
  testthat::local_mocked_bindings(
    readline = function(...) "N",
    .env = baseenv()
  )

  expect_no_error(check_pkg_ver(test = TRUE))
})
