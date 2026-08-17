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
# add_map_e61() and setup_stadia_maps() both need network access and a real
# Stadia Maps API key (via ggmap::get_stadiamap()/ggmap::has_stadiamaps_key()),
# so full integration tests aren't feasible here. These tests instead cover
# the input-validation guards in add_map_e61() that run *before* the network
# call, by mocking ggmap::has_stadiamaps_key() so the validation code is
# actually reached. `has_stadiamaps_key()` is called via `::` from e61_map.R,
# so (per ?testthat::local_mocked_bindings, "Namespaced calls") mocking it
# requires `.package = "ggmap"` rather than mocking inside theme61's own
# namespace, unlike the local_mocked_bindings() usage in test-check_pkg_ver.R
# for functions theme61 calls unqualified.
#
# local_mocked_bindings() scopes its unmock-on-exit to `.env`'s calling
# frame, so this small helper must be passed `parent.frame()` explicitly -
# without it, the mock would be undone the instant the helper returns,
# before the test body ever runs.
local_mock_stadia_key <- function(has_key = TRUE, env = parent.frame()) {
  testthat::local_mocked_bindings(
    has_stadiamaps_key = function(...) has_key,
    .package = "ggmap",
    .env = env
  )
}

# add_map_e61()'s bbox validation only *warns* for the top/bottom > 0 case
# (it does not stop execution), so code would otherwise fall through to a
# real ggmap::get_stadiamap() network call. Mock get_stadiamap() itself to
# fail immediately with a sentinel error so no network request is ever made,
# while still letting every validation guard above it run for real.
local_mock_no_network <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    get_stadiamap = function(...) stop("network_blocked_in_tests"),
    .package = "ggmap",
    .env = env
  )
}

test_that("add_map_e61() warns when latitude coordinates are positive", {
  skip_if_not_installed("ggmap")
  local_mock_stadia_key(TRUE)
  local_mock_no_network()

  bad_bbox <- c(top = 33.757742, right = 151.492882, bottom = -34.024779, left = 150.839539)

  expect_warning(
    tryCatch(add_map_e61(bbox = bad_bbox), error = function(e) NULL),
    "south of the equator"
  )
})

test_that("add_map_e61() warns when bottom coordinate is positive", {
  skip_if_not_installed("ggmap")
  local_mock_stadia_key(TRUE)
  local_mock_no_network()

  bad_bbox <- c(top = -33.757742, right = 151.492882, bottom = 34.024779, left = 150.839539)

  expect_warning(
    tryCatch(add_map_e61(bbox = bad_bbox), error = function(e) NULL),
    "south of the equator"
  )
})

test_that("add_map_e61() errors when top < bottom", {
  skip_if_not_installed("ggmap")
  local_mock_stadia_key(TRUE)

  bad_bbox <- c(top = -35, right = 151.492882, bottom = -34, left = 150.839539)

  expect_error(add_map_e61(bbox = bad_bbox), "top coordinate must be greater than your bottom")
})

test_that("add_map_e61() errors when right < left", {
  skip_if_not_installed("ggmap")
  local_mock_stadia_key(TRUE)

  bad_bbox <- c(top = -33.757742, right = 150, bottom = -34.024779, left = 151)

  expect_error(add_map_e61(bbox = bad_bbox), "right coordinate must be greater than your left")
})

test_that("add_map_e61() errors when top == bottom", {
  skip_if_not_installed("ggmap")
  local_mock_stadia_key(TRUE)

  bad_bbox <- c(top = -34, right = 151.492882, bottom = -34, left = 150.839539)

  expect_error(add_map_e61(bbox = bad_bbox), "can't be equal")
})

test_that("add_map_e61() errors when right == left", {
  skip_if_not_installed("ggmap")
  local_mock_stadia_key(TRUE)

  bad_bbox <- c(top = -33.757742, right = 151, bottom = -34.024779, left = 151)

  expect_error(add_map_e61(bbox = bad_bbox), "can't be equal")
})

test_that("add_map_e61() errors without a Stadia Maps API key, before hitting the network", {
  skip_if_not_installed("ggmap")
  local_mock_stadia_key(FALSE)

  expect_error(add_map_e61(), "Stadia Maps API key")
})

test_that("add_map_e61() validation guards run in the documented order (top/bottom checked before left/right)", {
  skip_if_not_installed("ggmap")
  local_mock_stadia_key(TRUE)

  # top < bottom AND right < left: the top/bottom check should fire first
  bad_bbox <- c(top = -35, right = 150, bottom = -34, left = 151)

  expect_error(add_map_e61(bbox = bad_bbox), "top coordinate must be greater than your bottom")
})
