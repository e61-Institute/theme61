test_that("check_pkg_ver() is silent when package is up-to-date", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    gh = function(...) list(list(tag_name = "v1.2.3")),
    .env = asNamespace("gh")
  )
  local_mocked_bindings(
    packageVersion = function(...) package_version("1.2.3"),
    .env = asNamespace("theme61")
  )

  expect_silent(theme61:::check_pkg_ver())
})

test_that("check_pkg_ver() emits a startup message and proceeds when offline", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    gh = function(...) stop("network down"),
    .env = asNamespace("gh")
  )

  expect_message(
    theme61:::check_pkg_ver(),
    "could not check if your version of theme61 is up-to-date",
    fixed = FALSE
  )
})

test_that("check_pkg_ver() prompts when out-of-date (test mode) and exits on N", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    gh = function(...) list(list(tag_name = "v999.999.999")),
    .env = asNamespace("gh")
  )
  local_mocked_bindings(
    readline = function(...) "N",
    .env = asNamespace("theme61")
  )

  expect_message(theme61:::check_pkg_ver(test = TRUE), "out-of-date")
})
