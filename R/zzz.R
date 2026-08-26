# Required for data.table to work
.datatable.aware = TRUE

# helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# Required to setup an empty environment with this name
t61_env <- NULL

# Code inside here runs when the package is loaded with library(theme61)
.onLoad <- function(libname, pkgname) {

  # Check if package is up-to-date (hard opt-out for CI / airgapped machines,
  # see THEME61_DISABLE_VERSION_CHECK)
  if (!identical(Sys.getenv("THEME61_DISABLE_VERSION_CHECK", unset = ""), "1")) {
    tryCatch(check_pkg_ver(), error = function(e) invisible(NULL))
  }

  # Add PT Sans font and set up for use
  tryCatch(.t61_init_fonts(), error = function(e) invisible(NULL))

  # Set up package environment for things like dual y-axis
  t61_env <<- new.env()

  ## Update default colours from black (hard opt-out for anyone who doesn't
  ## want theme61 changing ggplot2's session-wide geom defaults, see
  ## THEME61_DISABLE_GEOM_DEFAULTS)
  if (!identical(Sys.getenv("THEME61_DISABLE_GEOM_DEFAULTS", unset = ""), "1")) {

    # Update default colour
    geoms_colour <- c("point", "line", "pointrange", "errorbar", "boxplot")
    lapply(geoms_colour, \(x) {update_geom_defaults(x, aes(colour = e61_tealdark))})

    # Update default fill
    geoms_fill <- c("col")
    lapply(geoms_fill, \(x) {update_geom_defaults(x, aes(fill = e61_tealdark))})

    # Update defaults for other types
    # Keep geom_ribbon() semi-transparent by default.
    update_geom_defaults("ribbon", aes(fill = e61_tealdark, alpha = 0.1))

    # Restore ggplot2 defaults for geoms that inherit from GeomRibbon so they
    # do not unintentionally inherit ribbon transparency.
    update_geom_defaults("area", aes(alpha = NA))
  }

}

.onAttach <- function(libname, pkgname) {
  op <- options()
  op.theme61 <- list(
    theme61.auto_label = TRUE,
    theme61.auto_theme = TRUE,
    theme61.autolabel_fallback_msg = TRUE,
    theme61.autolabel_fast_msg = TRUE,
    theme61.base_size = 10,
    theme61.default_save_format = "svg",
    theme61.disable_spellcheck = FALSE,
    theme61.iterate_mode = FALSE,
    theme61.open_in_browser = FALSE,
    theme61.preview_on_print = TRUE,
    theme61.sec_axis_msg = TRUE
  )

  to_set <- !(names(op.theme61) %in% names(op))
  if (any(to_set)) {
    options(op.theme61[to_set])
  }

  invisible()
}
