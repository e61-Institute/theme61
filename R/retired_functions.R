# This file contains the names of defunct functions following past breaking
# changes. Remove these entirely after 2 minor release versions have passed.
# i.e. if a function was made defunct in v0.2, then remove the function error
# message code entirely from v0.4 onwards, as users have had ample warning to
# fix their code.


# Defunct from v0.7 -------------------------------------------------------

#' Deprecated
#' @export
#' @noRd
add_zeroline <- function(...) {
  lifecycle::deprecate_soft(when = "0.6.0",
                            what = "add_zeroline()",
                            with = "add_baseline()")
}

# Defunct from vX.X -----------------------------------------------------

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
