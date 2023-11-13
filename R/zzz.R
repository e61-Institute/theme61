# Required for data.table to work
.datatable.aware = TRUE

# Required to setup an empty environment with this name
t61_env <- NULL

# Code inside here runs when the package is loaded with library(theme61)
.onLoad <- function(libname, pkgname) {

  # Check if package is up-to-date
  check_pkg_ver()

  # Add PT Sans font and set up for use
  sysfonts::font_add_google("PT Sans", "pt-sans")
  showtext::showtext_auto()

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
  update_geom_defaults("ribbon", aes(fill= e61_tealdark, alpha = 0.1))

}
