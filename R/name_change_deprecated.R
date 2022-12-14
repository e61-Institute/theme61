# This file contains the names of functions that were renamed when we appended e61 to most functions.

e61_labs <- function(...) {
  lifecycle::deprecate_stop("0.2.0", "e61_labs()", "labs_e61()")
}

e61_save <- function(...) {
  lifecycle::deprecate_stop("0.2.0", "e61_save()", "save_e61()")
}

e61_y_title_top <- function(...) {
  lifecycle::deprecate_stop("0.2.0", "e61_y_title_top()", "y_title_top_e61()")
}

e61_colour_manual <- function(...) {
  lifecycle::deprecate_stop("0.2.0", "e61_colour_manual()", "scale_colour_e61()")
}


e61_fill_manual <- function(...) {
  lifecycle::deprecate_stop("0.2.0", "e61_fill_manual()", "scale_fill_e61()")
}


e61_colour_aus <- function(...) {
  lifecycle::deprecate_stop("0.2.0", "e61_colour_aus()", "scale_colour_e61_aus()")
}


e61_fill_aus <- function(...) {
  lifecycle::deprecate_stop("0.2.0", "e61_fill_aus()", "scale_fill_e61_aus()")
}
