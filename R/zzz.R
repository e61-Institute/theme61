# Required for data.table to work
.datatable.aware = TRUE

# helper
`%||%` <- function(x, y) if (is.null(x)) y else x

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

  # Ensure namespaces are available
  if (!requireNamespace("S7", quietly = TRUE)) {
    stop("S7 must be installed.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 must be installed.", call. = FALSE)
  }

  # Get the ggplot2 S7 generic safely
  update_ggplot <- getExportedValue("ggplot2", "update_ggplot")

  # Register method: (e61_theme_spec, any) -> attach spec to plot
  S7::method(update_ggplot, list(e61_theme_spec_class, S7::class_any)) <- .update_ggplot_e61_theme_spec

  # Required for S7 methods defined in packages
  S7::methods_register()

}

.onAttach <- function(libname, pkgname) {
  op <- options()
  op.theme61 <- list(
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
