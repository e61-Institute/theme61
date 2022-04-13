#' Format axes in the e61 Institute style
#'
#' @param expand_bottom How much to expand bottom axis line by
#' @param expand_top How much to expand top axis line by
#' @param sec_axis Adds a secondary axis (defaults to `dup_axis()`, which
#'   duplicates the axis), see \link[ggplot2]{sec_axis} for more details. Set to
#'   FALSE to hide a secondary axis.
#' @param ...
#'
#' @rdname e61_axes
#' @return
#' @export

scale_y_continuous_e61 <- function(expand_bottom = 0,
                                   expand_top = 0.015,
                                   sec_axis = dup_axis(),
                                   ...) {

  if (is.logical(sec_axis) && !sec_axis) {
    sec_axis <- waiver()
  }

  e61_y_continuous(expand_bottom = expand_bottom,
                   expand_top = expand_top,
                   sec_axis = sec_axis,
                   ...)
}

#' @rdname e61_axes
#' @export

scale_x_continuous_e61 <- function(expand_left = 0,
                                   expand_right = 0.015,
                                   ...) {

  e61_x_continuous(expand_left = expand_left,
                       expand_right = expand_right,
                       ...)
}

# These functions go in the above functions
e61_y_continuous <- function(expand_bottom = 0,
                             expand_top = 0.015,
                             sec_axis = sec_axis,
                             ...) {

  scale_y_continuous(expand = ggplot2::expansion(mult = c(expand_bottom,
                                                          expand_top)),
                     sec.axis = sec_axis,
                     ...)
}



e61_x_continuous <- function(expand_left = 0,
                             expand_right = 0.015,
                             ...) {

  scale_x_continuous(expand = ggplot2::expansion(mult = c(expand_left,
                                                          expand_right)),
                     ...)

}
