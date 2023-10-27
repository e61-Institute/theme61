#' Dual y-axis graphing functions
#'
#' \code{ggplot2}, by design, makes it rather difficult to create graphs where
#' the primary and secondary y-axis are on different scales. Hadley Wickham
#' claims this is to prevent people from abusing secondary y-axis to show dodgy
#' correlations (investment banking-style). However, sometimes they have valid
#' uses and the following functions enable this with as little fiddliness as
#' possible.
#'
#' \code{sec_rescale_inv} is used in the geom that will be displayed on the
#' secondary axis. \code{sec_rescale} is used in the \code{sec_axis} argument of
#' \code{scale_y_continuous_e61}.
#'
#' See the examples for how to use the two functions to manipulate the secondary
#' axis. Trial and error will be needed to select appropriate scale and shift
#' values.
#'
#' **Note:** Due to the way that ggplot functions interact with the R
#' environment, you have to run the code that generates the graph **twice**
#' after you change it before the changes will show up.
#'
#' @param values Vector of data that would normally be passed as the y aesthetic
#'   in the graph.
#' @param scale Numeric. Multiplicative factor that rescales the axis. For
#'   example, if the scale was originally 0 to 50, then \code{scale = 0.1} would
#'   rescale this to 0 to 5.
#' @param shift Numeric. Moves the axis up and down. For example, if the scale
#'   was 0 to 5, \code{shift = 5} moves the secondary scale down by 5 units to
#'   range from -5 to 0.
#' @rdname dual_y_axis
#' @export
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#' data <- data.frame(x = 1:5, y1 = 1:5 * 10, y2 = 5:1 - 5)
#'
#' ggplot(data, aes(x)) +
#'   geom_col(aes(y = y1)) +
#'   geom_point(aes(y = sec_rescale_inv(y2, scale = 0.1, shift = 5))) +
#'   # Some extra arguments are required to correctly format the secondary axis:
#'   # Set rescaled_sec = TRUE in scale_y_continuous_e61() to format the breaks correctly.
#'   # The secondary y-axis label (name = "%") needs to be explicitly specified otherwise it will not appear.
#'   scale_y_continuous_e61(limits = c(0, 60, 10), sec_axis = sec_axis(~sec_rescale(.), name = "%"), rescale_sec = TRUE) +
#'   labs_e61(y = "%")
#' }
#'
sec_rescale_inv <- function(values, scale = 1, shift = 0) {

  # Store scale and shift vars to supply to sec_rescale()
  assign("sec_axis_scale", scale, envir = t61_env)
  assign("sec_axis_shift", shift, envir = t61_env)

  # Inform the user of the weirdness
  if (getOption("sec_axis_msg", default = TRUE)) {
    cli::cli_alert_info("Did your graph not show any change to the secondary axis? Due to weirdness, you need to run the graph code twice after making changes to the secondary axis rescaling. This message appears once per session. To view it again, run `options(sec_axis_msg = TRUE)`.",
                        wrap = TRUE)

    # Turn off the option after appearing once
    options(sec_axis_msg = FALSE)
  }

  return ((values + shift) / scale)
}

#' @rdname dual_y_axis
#' @export
sec_rescale <- function(values, scale = t61_env$sec_axis_scale, shift = t61_env$sec_axis_shift) {
  return (values * scale - shift)
}
