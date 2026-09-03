# Writes `json` to wherever the code under test asked for its download, so the
# real parsing and version-validation runs against it.
fake_download <- function(json) {
  function(url, destfile, ...) {
    writeLines(json, destfile)
    0L
  }
}

release_json <- function(tag) {
  paste0('[{"url":"https://api.github.com/repos/e61-institute/theme61/releases/1",',
         '"tag_name":"', tag, '","name":"', tag, '","draft":false}]')
}

test_that("check_pkg_ver() is silent when package is up-to-date", {
  v <- as.character(utils::packageVersion("theme61"))

  local_mocked_bindings(
    .t61_download_file = fake_download(release_json(paste0("v", v))),
    .env = asNamespace("theme61")
  )

  expect_silent(theme61:::check_pkg_ver())
})

test_that("check_pkg_ver() emits a startup message and proceeds when offline", {
  local_mocked_bindings(
    .t61_download_file = function(...) stop("network down"),
    .env = asNamespace("theme61")
  )

  expect_message(
    theme61:::check_pkg_ver(),
    "could not check if your version of theme61 is up-to-date",
    ignore.case = TRUE
  )
})

test_that("check_pkg_ver() prompts when out-of-date and exits on N (test=TRUE)", {
  # Avoid any real network call
  local_mocked_bindings(
    .t61_download_file = fake_download(release_json("v0.0.0")),
    .env = asNamespace("theme61")
  )

  # Force the interactive prompt path, regardless of how the test suite
  # itself is being run (interactively or not).
  local_mocked_bindings(
    .t61_interactive = function() TRUE,
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
  # Avoid any real network call
  local_mocked_bindings(
    .t61_download_file = fake_download(release_json("v0.0.0")),
    .env = asNamespace("theme61")
  )

  # Simulate a non-interactive session (Rscript, R CMD check, knitr/Quarto, CI)
  local_mocked_bindings(
    .t61_interactive = function() FALSE,
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

test_that("t61_latest_release() reads the first tag_name, with or without a v prefix", {
  local_mocked_bindings(
    .t61_download_file = fake_download(release_json("v1.2.3")),
    .env = asNamespace("theme61")
  )
  expect_equal(theme61:::t61_latest_release(), package_version("1.2.3"))

  local_mocked_bindings(
    .t61_download_file = fake_download(release_json("4.5.6")),
    .env = asNamespace("theme61")
  )
  expect_equal(theme61:::t61_latest_release(), package_version("4.5.6"))
})

test_that("t61_latest_release() takes the most recent release when several are returned", {
  json <- paste0('[{"tag_name":"v2.0.0"},{"tag_name":"v1.0.0"}]')

  local_mocked_bindings(
    .t61_download_file = fake_download(json),
    .env = asNamespace("theme61")
  )

  expect_equal(theme61:::t61_latest_release(), package_version("2.0.0"))
})

test_that("t61_latest_release() returns NULL rather than erroring on a non-release response", {
  # What a rate-limited or errored API call actually returns
  local_mocked_bindings(
    .t61_download_file = fake_download('{"message":"API rate limit exceeded","status":"403"}'),
    .env = asNamespace("theme61")
  )
  expect_null(theme61:::t61_latest_release())

  # A tag that isn't a version number at all
  local_mocked_bindings(
    .t61_download_file = fake_download(release_json("nightly-build")),
    .env = asNamespace("theme61")
  )
  expect_null(theme61:::t61_latest_release())

  # Empty release list
  local_mocked_bindings(
    .t61_download_file = fake_download("[]"),
    .env = asNamespace("theme61")
  )
  expect_null(theme61:::t61_latest_release())
})

test_that("check_pkg_ver() stays silent when the release feed is unusable", {
  local_mocked_bindings(
    .t61_download_file = fake_download('{"message":"Not Found"}'),
    .env = asNamespace("theme61")
  )

  expect_silent(theme61:::check_pkg_ver())
})
