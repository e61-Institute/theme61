test_that("setup_stadia_maps() errors instead of hanging on readline() in a non-interactive session", {
  skip_if_not_installed("ggmap")

  # testthat runs non-interactively, so interactive() is FALSE here and no
  # bypass argument has been supplied - this must fail fast with a clear
  # cli::cli_abort() rather than calling readline() (which would error/hang).
  expect_error(
    setup_stadia_maps(),
    class = "rlang_error"
  )

  expect_error(
    setup_stadia_maps(),
    regexp = "non-interactive"
  )
})

test_that("setup_stadia_maps(api_key = ...) skips the prompts and reaches registration directly", {
  skip_if_not_installed("ggmap")

  n_readline <- 0L
  fake_readline <- function(...) {
    n_readline <<- n_readline + 1L
    "N"
  }

  local_mocked_bindings(
    .t61_readline = fake_readline,
    .env = asNamespace("theme61")
  )

  local_mocked_bindings(
    register_stadiamaps = function(key, write = FALSE) invisible(NULL),
    has_stadiamaps_key = function() TRUE,
    .package = "ggmap"
  )

  expect_no_error(setup_stadia_maps(api_key = "fake-key-for-test"))
  expect_equal(n_readline, 0L)
})

test_that("setup_stadia_maps(update_ggmap = FALSE) does not prompt for the ggmap update", {
  skip_if_not_installed("ggmap")

  n_readline <- 0L
  fake_readline <- function(...) {
    n_readline <<- n_readline + 1L
    "N"
  }

  local_mocked_bindings(
    .t61_readline = fake_readline,
    .env = asNamespace("theme61")
  )

  # update_ggmap is answered, but no api_key is given, so we still hit the
  # non-interactive api_key guard - confirms the update prompt itself was
  # bypassed (n_readline stays 0) rather than the whole function short-circuiting.
  expect_error(
    setup_stadia_maps(update_ggmap = FALSE),
    regexp = "non-interactive"
  )
  expect_equal(n_readline, 0L)
})
