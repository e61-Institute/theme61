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
#' @param add_space Logical. This argument is for internal theme61 purposes
#'   only. It is recommended that as a user you do not include it in your
#'   function call. Defaults to FALSE to ensure that we only add the extra white
#'   space above the chart when we are saving it.
#' @inheritDotParams ggplot2::scale_y_continuous name oob na.value trans guide position
#' @rdname e61_axes
#' @export

scale_y_continuous_e61 <- function(limits = NULL,
                                   sec_axis = ggplot2::dup_axis(),
                                   rescale_sec = FALSE,
                                   expand_bottom = 0,
                                   expand_top = 0,
                                   add_space = FALSE,
                                   ...) {

  # Set sec_axis to default behaviour if we don't want it
  if (isFALSE(sec_axis)) sec_axis <- ggplot2::waiver()

  # Prepares limits and breaks
  breaks <- resolve_breaks_e61(limits)

  # Prepares breaks for the rescaled secondary axis if used
  if (isTRUE(rescale_sec)) {
    sec_breaks <- sec_rescale(breaks)
    sec_labels <- sec_breaks
    sec_labels[is.na(sec_labels)] <- ""
    sec_axis$breaks <- sec_breaks
    sec_axis$labels <- sec_labels
  }

  if(!is.null(limits) && add_space){

    # Add 3% to the supplied limits to create a bit of white space at the
    # top of the chart. applied_limits (not the original limits) is what
    # actually gets passed to the scale, so it's also what the data-range
    # check below must validate against.
    applied_limits <- c(limits[1], limits[2] + (limits[2] - limits[1]) * 0.03)

    retval <- ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(expand_bottom, expand_top)),
      sec.axis = sec_axis,
      limits = applied_limits,
      breaks = breaks,
      ...
    )

  } else if(!is.null(limits)){

    # Make sure limits are only the min and max values (i.e. strictly length = 2)
    limits <- limits[1:2]
    applied_limits <- limits

    # Put it all together
    retval <- ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(expand_bottom, expand_top)),
      sec.axis = sec_axis,
      limits = applied_limits,
      breaks = breaks,
      ...
    )

  } else {
    applied_limits <- NULL
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

  # Only add our data-range check if numeric limits were supplied
  if (!is.null(limits) && is.numeric(limits)) {
    # Save the original train function
    orig_train <- retval$train

    # Override the train method
    retval$train <- function(x) {
      # Call the original train to update x based on data
      orig_train(x)

      # Fixes issues with -Inf and Inf when adding shaded areas to graphs
      x_ok <- x[is.finite(x)]

      # If nothing left after filtering (e.g. only Inf/-Inf annotations), don't enforce the check
      if (length(x_ok) == 0L) return(invisible())

      # x now contains the data values (possibly transformed) used to train the scale
      data_range <- range(x_ok, na.rm = TRUE)

      # Stop if actual data range fall outside the limits actually applied
      # to the scale (applied_limits, not the original limits argument -
      # with add_space = TRUE those differ by the 3% top padding, and
      # checking against the pre-padding value here would reject data that
      # the scale itself comfortably has room for).
      if (applied_limits[1] > data_range[1] || applied_limits[2] < data_range[2]) {
        cli::cli_abort("Supplied limits are outside the data's range. Data range: [{data_range[1]}, {data_range[2]}]; Supplied limits: [{applied_limits[1]}, {applied_limits[2]}]. Change your limits so they contain the full range of the data.",
                       call = expr(scale_y_continuous_e61()),
                       class = "error"
                       )
      }
    }
  }

  return(retval)
}

#' @param hide_first_last Logical. Defaults to FALSE. Hides the first and
#'   last x-axis labels to avoid overlapping with the bottom of the y-axis.
#' @inheritParams scale_y_continuous_e61
#' @rdname e61_axes
#' @export

scale_x_continuous_e61 <- function(limits = NULL,
                                   expand_left = 0.05,
                                   expand_right = 0.05,
                                   hide_first_last = FALSE,
                                   ...) {

  # Prepares limits and breaks
  breaks <- resolve_breaks_e61(limits, hide_first_last)

  # Make sure limits are only the min and max values (i.e. strictly length = 2)
  limits <- limits[1:2]

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

#' Resolve the `breaks` argument for scale_x/y_continuous_e61() from a
#' `limits` argument: a length-3 c(min, max, increment) becomes an explicit
#' break sequence, any other numeric limits fall back to
#' scales::breaks_extended(), and NULL/non-numeric limits use the default
#' waiver(). Shared by scale_x_continuous_e61() and scale_y_continuous_e61().
#' @noRd
resolve_breaks_e61 <- function(limits, hide_first_last = FALSE) {

  if (is.null(limits) || !is.numeric(limits)) {
    return(ggplot2::waiver())
  }

  drop_ends <- function(x) {
    if (hide_first_last) {
      x[x == min(x, na.rm = TRUE)] <- NA
      x[x == max(x, na.rm = TRUE)] <- NA
    }
    x
  }

  if (length(limits) == 3) {
    drop_ends(round(seq(limits[[1]], limits[[2]], limits[[3]]), 10))
  } else {
    function(x) drop_ends(scales::breaks_extended()(x))
  }
}
