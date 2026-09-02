# SVG snapshot tests compare exact rendered glyph output. Even with the same
# embedded pt-sans font file, macOS/Windows font rendering engines produce
# different glyph paths than Linux's, so a snapshot regenerated on Linux (CI's
# source of truth) never matches there for reasons unrelated to the code being
# tested. Skip only in CI on those OSes -- a local Mac/Windows developer still
# gets to run (and see real regressions in) these tests, they just can't rely
# on this specific check the way a Linux CI run can.
skip_snapshot_off_ci_linux <- function() {
  on_ci <- identical(Sys.getenv("CI"), "true")
  on_linux <- identical(Sys.info()[["sysname"]], "Linux")

  if (on_ci && !on_linux) {
    testthat::skip("svg snapshot tests only run on Linux CI (font rendering differs by OS)")
  }
}
