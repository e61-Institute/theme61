#' Format axes in the e61 style
#'
#' These functions format the x and y axes to be consistent with e61 styling.
#' This includes removing white space at the beginning and end of each axis.
#'
#' @param expand_bottom,expand_top Numeric. Add extra space between data points
#'   and the top/bottom of the graph. See [expansion][ggplot2::expansion] for
#'   details.
#' @param sec_axis Logical. Defaults to duplicating the y-axis so it shows on
#'   the left and right. Set to FALSE to hide the secondary axis.
#' @param rescale_sec Logical. Set this to TRUE if you are using a rescaled
#'   secondary axis, otherwise leave it as FALSE (default). To add a rescaled
#'   secondary axis, see the documentation for [sec_rescale].
#' @param y_top Logical. Ensures there is space at the top of the y-axis for the
#'   axis label. Defaults to TRUE. Set to FALSE if the axis label is placed
#'   elsewhere. If you change this argument you also need to change the argument
#'   with the same name in [theme_e61].
#' @param expand_left,expand_right Numeric. Add extra space between data points
#'   and the left/right of the graph. See [expansion][ggplot2::expansion] for
#'   details.
#' @param limits One of:
#'   \itemize{
#'     \item{A numeric vector of length three providing the limits of the scale
#'     and the increment between each axis tick, e.g. `c(0, 25, 5)` will
#'     set the axis to range from 0 to 25, with increments of 5 per tick.}
#'     \item{A numeric vector of length two providing the minimum and maximum
#'     limits of the scale. The break increments will be automatically chosen.}
#'     \item{`NULL` to use the default scale range.}
#'     }
#' @inheritDotParams ggplot2::scale_y_continuous name oob na.value trans guide
#'   position
#'
#' @rdname e61_axes
#' @export

scale_y_continuous_e61 <- function(limits = NULL,
                                   sec_axis = ggplot2::dup_axis(),
                                   rescale_sec = FALSE,
                                   y_top = TRUE,
                                   expand_bottom = 0,
                                   expand_top = 0,
                                   ...) {

  # Set sec_axis to default behaviour if we don't want it
  if (isFALSE(sec_axis)) sec_axis <- ggplot2::waiver()

  # Prepares limits and breaks
  if (!is.null(limits) && is.numeric(limits)) {

    if (length(limits) == 3) {
      breaks <- round(seq(limits[[1]], limits[[2]], limits[[3]]), 10)

      # Hides the last break to make space for the unit label
      if (isTRUE(y_top)) breaks[breaks == max(breaks, na.rm = TRUE)] <- NA

    } else {
      breaks <- function(x) {
        x <- scales::breaks_extended()(x)
        # Hides the last break to make space for the unit label
        if (isTRUE(y_top)) x[x == max(x, na.rm = TRUE)] <- NA
        return(x)
      }
    }
  } else {
    breaks <- ggplot2::waiver()
  }

  # Prepares breaks for the rescaled secondary axis if used
  if (isTRUE(rescale_sec)) {
    sec_breaks <- sec_rescale(breaks)
    sec_labels <- sec_breaks
    sec_labels[is.na(sec_labels)] <- ""
    sec_axis$breaks <- sec_breaks
    sec_axis$labels <- sec_labels
  }

  if(!is.null(limits)){
    # Put it all together
    retval <- ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(expand_bottom, expand_top)),
      sec.axis = sec_axis,
      limits = limits,
      breaks = breaks,
      ...
    )

  } else {
    retval <- ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(expand_bottom, expand_top)),
      sec.axis = sec_axis,
      ...
    )
  }

  # Set a class if e61 scales are used
  class(retval) <- c(class(retval), "scale_e61")

  # Set an additional class if rescaled dual axis used
  if (isTRUE(rescale_sec)) class(retval) <- c(class(retval), "rescale_y")

  # Set an additional class if no y_top requested
  if (isFALSE(y_top))
    class(retval) <- c(class(retval), "no_y_top")

  # Only add our data-range check if numeric limits were supplied
  if (!is.null(limits) && is.numeric(limits)) {
    # Save the original train function
    orig_train <- retval$train

    # Override the train method
    retval$train <- function(x) {
      # Call the original train to update x based on data
      orig_train(x)
      # x now contains the data values (possibly transformed) used to train the scale
      data_range <- range(x, na.rm = TRUE)

      # Stop if actual data range fall outside the provided limits
      if (limits[1] > data_range[1] || limits[2] < data_range[2]) {
        cli::cli_abort("Supplied limits are outside the data's range. Data range: [{data_range[1]}, {data_range[2]}]; Supplied limits: [{limits[1]}, {limits[2]}]. Change your limits so they contain the full range of the data.",
                       call = expr(scale_y_continuous_e61()),
                       class = "error"
                       )
      }
    }
  }

  return(retval)
}

#' @param hide_first_last Logical. Defaults to TRUE. Hides the first and
#'   last x-axis labels to avoid overlapping with the bottom of the y-axis.
#' @inheritParams scale_y_continuous_e61
#' @rdname e61_axes
#' @export

scale_x_continuous_e61 <- function(limits = NULL,
                                   expand_left = 0.05,
                                   expand_right = 0.05,
                                   hide_first_last = TRUE,
                                   ...) {

  # Prepares limits and breaks
  if (!is.null(limits) && is.numeric(limits)) {

    if (length(limits) == 3) {
      breaks <- round(seq(limits[[1]], limits[[2]], limits[[3]]), 10)

      # Hides the first and last break
      if (hide_first_last) {
        breaks[breaks == min(breaks, na.rm = TRUE)] <- NA
        breaks[breaks == max(breaks, na.rm = TRUE)] <- NA
      }


    } else {
      breaks <- function(x) {
        x <- scales::breaks_extended()(x)
        # Hides the first and last break
        if (hide_first_last) {
          x[x == min(x, na.rm = TRUE)] <- NA
          x[x == max(x, na.rm = TRUE)] <- NA
        }
        return(x)
      }
    }
  } else {
    breaks <- ggplot2::waiver()
  }

  # Put it all together
  retval <- ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(expand_left, expand_right)),
    limits = limits,
    breaks = breaks,
    ...
    )

  class(retval) <- c(class(retval), "scale_e61")

  return(retval)

}
