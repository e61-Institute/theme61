# Required for data.table to work
.datatable.aware = TRUE

# helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# Required to setup an empty environment with this name
t61_env <- NULL

# The full set of theme61.* options and their defaults. This is the single
# source of truth used both to set defaults at load time (.onLoad(), below)
# and to validate option names in set_t61_options() (see
# R/theme61-options.R) - so it must be kept in sync with every documented
# option in ?set_t61_options.
.t61_default_options <- list(
  theme61.auto_label = TRUE,
  theme61.auto_theme = TRUE,
  theme61.autolabel_fast_msg = TRUE,
  theme61.base_size = 10,
  theme61.disable_spellcheck = FALSE,
  theme61.iterate_mode = FALSE,
  theme61.max_discrete_colours = 12L,
  theme61.max_discrete_fills = 12L,
  theme61.open_in_browser = FALSE,
  theme61.preview_on_print = TRUE,
  theme61.sec_axis_msg = TRUE
)

# Code inside here runs when the package is loaded, including via
# namespace-qualified use (e.g. theme61::save_e61()) that never attaches the
# package with library()/require()
.onLoad <- function(libname, pkgname) {

  # Set default options, without clobbering anything the user already set
  # (e.g. via options() before theme61 was loaded)
  op <- options()
  to_set <- !(names(.t61_default_options) %in% names(op))
  if (any(to_set)) {
    options(.t61_default_options[to_set])
  }

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
