# Required for data.table to work
.datatable.aware = TRUE

# Required to setup an empty environment with this name
t61_env <- NULL

# Code inside here runs when the package is loaded with library(theme61)
.onLoad <- function(libname, pkgname) {

  # Check if package is up-to-date
  tryCatch(check_pkg_ver(), error = function(e) invisible(NULL))

  # Add PT Sans font and set up for use
  tryCatch(.t61_init_fonts(), error = function(e) invisible(NULL))

  # Set up package environment for things like dual y-axis
  t61_env <<- new.env()

  ## Update default colours from black

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

.onAttach <- function(libname, pkgname) {
  op <- options()
  op.theme61 <- list(
    theme61.auto_theme = TRUE,
    theme61.base_size = 10,
    theme61.default_save_format = "svg",
    theme61.open_e61_graph = FALSE,
    theme61.preview_on_print = TRUE
  )

  to_set <- !(names(op.theme61) %in% names(op))
  if (any(to_set)) {
    options(op.theme61[to_set])
  }

  invisible()
}
