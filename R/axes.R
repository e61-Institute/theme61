#' Format axes in the e61 Institute style
#'
#' These functions format the x and y axes to be consistent with e61 styling.
#' This includes removing white space at the beginning and end of each axis.
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
#' @rdname e61_axes
#' @export

scale_y_continuous_e61 <- function(limits = NULL,
                                   sec_axis = dup_axis(),
                                   y_top = TRUE,
                                   expand_bottom = 0,
                                   expand_top = 0,
                                   ...) {

  # Set sec_axis to default behaviour if we don't want it
  if (isFALSE(sec_axis)) sec_axis <- waiver()

  # Put the little bit of y-axis back in
  if (isFALSE(y_top)) limits[[2]] <- limits[[2]] + 0.0001

  # Prepares limits and breaks
  if (!is.null(limits) && is.numeric(limits)) {

    if (length(limits) == 3) {
      breaks <- seq(limits[[1]], limits[[2]], limits[[3]])

      # Hides the last break to make space for the unit label
      breaks[breaks == max(breaks, na.rm = TRUE)] <- NA

    } else {
      breaks <- function(x) {
        x <- scales::breaks_extended()(x)
        # Hides the last break to make space for the unit label
        x[x == max(x, na.rm = TRUE)] <- NA
        return(x)
      }
    }
  } else {
    breaks <- ggplot2::waiver()
  }

  # Put it all together
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(expand_bottom, expand_top)),
    sec.axis = sec_axis,
    limits = limits,
    breaks = breaks,
    ...
  )

}

#' @inheritParams scale_y_continuous_e61
#' @inheritDotParams ggplot2::scale_x_continuous
#' @rdname e61_axes
#' @export

scale_x_continuous_e61 <- function(limits = NULL,
                                   expand_left = 0,
                                   expand_right = 0,
                                   ...) {

  # Prepares limits and breaks
  if (!is.null(limits) && is.numeric(limits)) {

    if (length(limits) == 3) {
      breaks <- seq(limits[[1]], limits[[2]], limits[[3]])

      # Hides the first and last break
      breaks[breaks == min(breaks, na.rm = TRUE)] <- NA
      breaks[breaks == max(breaks, na.rm = TRUE)] <- NA

    } else {
      breaks <- function(x) {
        x <- scales::breaks_extended()(x)
        # Hides the first and last break
        x[x == min(x, na.rm = TRUE)] <- NA
        x[x == max(x, na.rm = TRUE)] <- NA
        return(x)
      }
    }
  } else {
    breaks <- ggplot2::waiver()
  }

  # Put it all together
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(expand_left, expand_right)),
    limits = limits,
    breaks = breaks,
    ...)

}
