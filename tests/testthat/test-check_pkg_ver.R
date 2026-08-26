test_that("check_pkg_ver() is silent when package is up-to-date", {
  skip_if_not_installed("gh")

  v <- as.character(utils::packageVersion("theme61"))

  local_mocked_bindings(
    gh = function(...) list(list(tag_name = paste0("v", v))),
    .env = asNamespace("gh")
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
    ignore.case = TRUE
  )
})

test_that("check_pkg_ver() prompts when out-of-date and exits on N (test=TRUE)", {
  skip_if_not_installed("gh")

  # Avoid any real network call
  local_mocked_bindings(
    gh = function(...) list(list(tag_name = "v0.0.0")),
    .env = asNamespace("gh")
  )

  # Force the interactive prompt path, regardless of how the test suite
  # itself is being run (interactively or not).
  local_mocked_bindings(
    interactive = function() TRUE,
    .env = asNamespace("theme61")
  )

  n_readline <- 0L
  fake_readline <- function(...) {
    n_readline <<- n_readline + 1L
    "N"
  }

  local_mocked_bindings(
    .t61_readline = fake_readline,
    .env = asNamespace("theme61")
  )

  # Don’t rely on cli output streams; just ensure it runs and prompted.
  expect_null(suppressMessages(theme61:::check_pkg_ver(test = TRUE)))
  expect_gte(n_readline, 1L)
})

test_that("check_pkg_ver() does not hang and does not prompt in non-interactive sessions", {
  skip_if_not_installed("gh")

  # Avoid any real network call
  local_mocked_bindings(
    gh = function(...) list(list(tag_name = "v0.0.0")),
    .env = asNamespace("gh")
  )

  # Simulate a non-interactive session (Rscript, R CMD check, knitr/Quarto, CI)
  local_mocked_bindings(
    interactive = function() FALSE,
    .env = asNamespace("theme61")
  )

  # readline() would return "" immediately and spin forever in a real
  # non-interactive session - assert it is never even called.
  fake_readline <- function(...) stop("readline should not be called when non-interactive")

  local_mocked_bindings(
    .t61_readline = fake_readline,
    .env = asNamespace("theme61")
  )

  expect_null(suppressMessages(theme61:::check_pkg_ver(test = TRUE)))
})
