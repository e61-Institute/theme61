#' Format axes in the e61 Institute style
#'
#' @param expand_bottom Numeric. Add extra space between data points and the
#'   bottom of the graph. See \link[ggplot2]{expansion} for details.
#' @param expand_top Numeric. Add extra space between data points and the top of
#'   the graph.
#' @param sec_axis Adds a secondary axis (defaults to \code{dup_axis()}, which
#'   duplicates the axis), see \link[ggplot2]{sec_axis} for more details. Set to
#'   FALSE to hide a secondary axis.
#' @param expand_left Numeric. Add extra space between data points and the left
#'   of the graph.
#' @param expand_right Numeric. Add extra space between data points and the
#'   right of the graph.
#' @inheritDotParams ggplot2::scale_y_continuous
#'
#' @rdname e61_axes
#' @return
#' @export

scale_y_continuous_e61 <- function(expand_bottom = 0,
                                   expand_top = 0,
                                   sec_axis = dup_axis(),
                                   limits = NULL,
                                   ...) {

  if (is.logical(sec_axis) && !sec_axis) {
    sec_axis <- waiver()
  }

  if (!is.null(limits) && is.numeric(limits)) {
    # Very slightly reduce the upper limit so the break label does not appear
    # This is needed so that the axis title does not overlap with the break label
    limits[[2]] <- limits[[2]] - 0.0001
  }

  e61_y_continuous(expand_bottom = expand_bottom,
                   expand_top = expand_top,
                   sec_axis = sec_axis,
                   limits = limits,
                   ...)
}

#' @inheritDotParams ggplot2::scale_x_continuous
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
                             limits = limits,
                             ...) {

  scale_y_continuous(expand = ggplot2::expansion(mult = c(expand_bottom,
                                                          expand_top)),
                     sec.axis = sec_axis,
                     limits = limits,
                     ...)
}



e61_x_continuous <- function(expand_left = 0,
                             expand_right = 0.015,
                             ...) {

  scale_x_continuous(expand = ggplot2::expansion(mult = c(expand_left,
                                                          expand_right)),
                     ...)

}
