# This file contains the names of defunct functions following past breaking
# changes. Remove these entirely after 2 minor release versions have passed.
# i.e. if a function was made defunct in v0.2, then remove the function error
# message code entirely from v0.4 onwards, as users have had ample warning to
# fix their code.

# Defunct from v0.7 -------------------------------------------------------

# Also remove a bunch of defunct function arguments in labs_e61, plot_label, save_e61 and scales

#' Deprecated
#' @export
#' @noRd
add_zeroline <- function(...) {
  lifecycle::deprecate_soft(when = "0.6.0",
                            what = "add_zeroline()",
                            with = "add_baseline()")
}

#' Deprecated
#' @export
#' @noRd
e61_palette <- function(...) {
  lifecycle::deprecate_soft(when = "0.6.0",
                            what = "e61_palette()",
                            with = "palette_e61()")
}

#' Deprecated
#' @export
#' @noRd
set_open_graph <- function(...) {
  lifecycle::deprecate_soft(when = "0.6.1",
                            what = "set_open_graph()",
                            with = "set_open_graph_browser()",
                            details = "Graphs now automatically appear in the Viewer pane when saved with save_e61(), so setting the option to open in the browser is now unnecessary."
                            )
}

# Defunct from v0.8 -----------------------------------------------------

#' Defunct
#' @export
#' @noRd
mpanel_e61 <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.6.0", what = "mpanel_e61()",
    details = "You no longer need to use mpanel_e61() when making multi-panel graphs. Instead, individual panels are directly supplied to save_e61(). See the documentation for save_e61() for more information."
    )
}

# Deprecate indefinitely ----------------------------------------------

#' Deprecated
#' @export
#' @noRd
mplot_label <- function(...) {
  lifecycle::deprecate_soft(when = "0.6.0",
                            what = "mplot_label()",
                            with = "plot_label()")
}

mplab <- function(...) {
  lifecycle::deprecate_soft(when = "0.6.0",
                            what = "mplab()",
                            with = "plab()")
}

# # Keep this as an example of a defunct function
#
# #' Defunct
# #' @export
# #' @noRd
# y_title_top_e61 <- function(...) {
#   lifecycle::deprecate_stop(when = "0.3.0", what = "y_title_top_e61()",
#                             details = "The functionality of `y_title_top_e61()`
#                               has been incorporated directly into `theme_e61()`.
#                               As a result the function has been removed from the
#                               package, and you should remove it from your code.")
#
# }
