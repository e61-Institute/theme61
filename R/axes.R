#' Format axes in the e61 Institute style
#'
#' @param expand_bottom Numeric. Add extra space between data points and the
#'   bottom of the graph. See \link[ggplot2]{expansion} for details.
#' @param expand_top Numeric. Add extra space between data points and the top of
#'   the graph.
#' @param sec_axis Adds a secondary axis (defaults to \code{dup_axis()}, which
#'   duplicates the axis), see \link[ggplot2]{sec_axis} for more details. Set to
#'   FALSE to hide a secondary axis.
#' @param y_top Logical. Ensures there is space at the top of the y-axis for the
#'   axis label. Defaults to TRUE. Set to FALSE if the axis label is placed
#'   elsewhere.
#' @param expand_left Numeric. Add extra space between data points and the left
#'   of the graph.
#' @param expand_right Numeric. Add extra space between data points and the
#'   right of the graph.
#' @param limits One of:
#'   \itemize{
#'     \item{\code{NULL} to use the default scale range.}
#'     \item{A numeric vector of length two providing the minimum and maximum
#'     limits of the scale.}
#'     \item{A numeric vector of length three providing the limits of the scale
#'     and the increment between each axis tick, e.g. \code{c(0, 25, 5)} will
#'     set the axis to range from 0 to 25, with increments of 5 per tick.}
#'     }
#' @inheritDotParams ggplot2::scale_y_continuous -breaks -minor_breaks -n.breaks
#'   -expand -sec.axis
#'
#' @description These functions format the x and y axes to be consistent with
#'   e61 styling. This includes removing white space at the beginning and end of
#'   each axis.
#'
#' @rdname e61_axes
#' @export

scale_y_continuous_e61 <- function(limits,
                                   sec_axis = dup_axis(),
                                   y_top = TRUE,
                                   expand_bottom = 0,
                                   expand_top = 0,
                                   ...) {

  if (!is.null(limits) && is.numeric(limits)) {
    # Very slightly reduce the upper limit so the break label does not appear
    # This is needed so that the axis title does not overlap with the break label
    limits[[2]] <- limits[[2]] - 1e-10
  }

  # Set sec_axis to default behaviour if we don't want it
  if (isFALSE(sec_axis)) sec_axis <- waiver()

  # Put the little bit of y-axis back in
  if (isFALSE(y_top)) limits[[2]] <- limits[[2]] + 1e-10

  e61_y_continuous(
    expand_bottom = expand_bottom,
    expand_top = expand_top,
    sec_axis = sec_axis,
    limits = limits,
    ...
  )

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

  if (length(limits) == 3) {
    custom_breaks <- seq(limits[[1]], limits[[2]], limits[[3]])

  } else {
    custom_breaks <- ggplot2::waiver()
  }

  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(expand_bottom,
                                         expand_top)),
    sec.axis = sec_axis,
    limits = limits,
    breaks = custom_breaks,
    ...
  )
}



e61_x_continuous <- function(expand_left = 0,
                             expand_right = 0.015,
                             ...) {

  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(expand_left,
                                                                   expand_right)),
                              ...)

}
