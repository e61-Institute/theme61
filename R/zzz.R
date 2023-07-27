t61_env <- NULL

.onLoad <- function(libname, pkgname) {

  # Add PT Sans font and set up for use
  sysfonts::font_add_google("PT Sans", "pt-sans")
  showtext::showtext_auto()

  # Set up package environment for things like dual y-axis
  t61_env <<- new.env()

}