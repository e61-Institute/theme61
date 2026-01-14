test_that("Interactive prompt works", {
  skip_if_not_installed("gh")

  ns <- asNamespace("gh")
  orig_gh <- get("gh", envir = ns)
  unlockBinding("gh", ns)
  assign("gh", function(...) list(list(tag_name = "v0.9.0")), ns)
  lockBinding("gh", ns)
  on.exit({
    unlockBinding("gh", ns)
    assign("gh", orig_gh, ns)
    lockBinding("gh", ns)
  }, add = TRUE)

  testthat::local_mocked_bindings(
    readline = function(...) "N",
    .env = baseenv()
  )

  expect_no_error(check_pkg_ver(test = TRUE))
})
