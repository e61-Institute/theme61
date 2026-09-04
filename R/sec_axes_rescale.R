#' Dual y-axis graphing functions
#'
#' `ggplot2`, by design, makes it rather difficult to create graphs where
#' the primary and secondary y-axis are on different scales. Hadley Wickham
#' claims this is to prevent people from abusing secondary y-axis to show dodgy
#' correlations (investment banking-style). However, sometimes they have valid
#' uses and the following functions enable this with as little fiddliness as
#' possible.
#'
#' Two functions are needed to make a rescaled secondary axis:
#'
#' * `sec_rescale_inv()` transforms the y aesthetic of the series that is to be
#'   plotted against the secondary axis, so it is drawn in the units of the
#'   primary axis.
#' * `sec_rescale_axis()` builds the secondary axis itself, and is passed to the
#'   `sec_axis` argument of [scale_y_continuous_e61()].
#'
#' Give both functions the same `scale` and `shift` values. Trial and error will
#' be needed to select appropriate values.
#'
#' `sec_rescale()` converts values from the primary axis back into the secondary
#' axis units. It is used internally by `sec_rescale_axis()` and is exported for
#' users who want to do the conversion themselves.
#'
#' @param values Vector of data that would normally be passed as the y aesthetic
#'   in the graph.
#' @param scale Numeric. Multiplicative factor that rescales the axis. For
#'   example, if the scale was originally 0 to 50, then `scale = 0.1` would
#'   rescale this to 0 to 5.
#' @param shift Numeric. Moves the axis up and down. For example, if the scale
#'   was 0 to 5, `shift = 5` moves the secondary scale down by 5 units to
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
#'   # Rescale the secondary series into primary axis units...
#'   geom_point(aes(y = sec_rescale_inv(y2, scale = 0.1, shift = 5))) +
#'   # ... and give the secondary axis the same scale and shift.
#'   scale_y_continuous_e61(
#'     limits = c(0, 60, 10),
#'     sec_axis = sec_rescale_axis(scale = 0.1, shift = 5, name = "%")
#'   ) +
#'   labs_e61(y = "%")
#' }
#'
sec_rescale_inv <- function(values, scale = 1, shift = 0) {

  check_rescale_arg(scale, "scale")
  check_rescale_arg(shift, "shift")

  return ((values + shift) / scale)
}

#' @rdname dual_y_axis
#' @export
sec_rescale <- function(values, scale, shift) {

  check_rescale_arg(scale, "scale")
  check_rescale_arg(shift, "shift")

  return (values * scale - shift)
}

#' @param name Character. Title for the secondary axis. Defaults to
#'   [waiver()][ggplot2::waiver], which reuses the primary axis title.
#' @returns `sec_rescale_axis()` returns a secondary axis object that can be
#'   passed to the `sec_axis` argument of [scale_y_continuous_e61()] (or to
#'   `sec.axis` in [ggplot2::scale_y_continuous()]).
#' @rdname dual_y_axis
#' @export
sec_rescale_axis <- function(scale = 1, shift = 0, name = ggplot2::waiver()) {

  check_rescale_arg(scale, "scale")
  check_rescale_arg(shift, "shift")

  new_sec_rescale_axis(scale = scale, shift = shift, name = name)
}

#' Build the secondary axis object used by sec_rescale_axis(). Carries the
#' scale/shift in an attribute so scale_y_continuous_e61() can align the
#' secondary breaks with the primary ones without any shared session state.
#' @noRd
new_sec_rescale_axis <- function(scale, shift, name = ggplot2::waiver(),
                                 breaks = ggplot2::waiver(),
                                 labels = ggplot2::waiver()) {

  axis <- ggplot2::sec_axis(
    transform = function(x) sec_rescale(x, scale = scale, shift = shift),
    name = name,
    breaks = breaks,
    labels = labels
  )

  attr(axis, "t61_rescale") <- list(scale = scale, shift = shift, name = name)

  axis
}

#' Validate a scale/shift argument.
#' @noRd
check_rescale_arg <- function(x, arg) {

  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a single finite number.")
  }

  if (identical(arg, "scale") && x == 0) {
    cli::cli_abort("{.arg scale} must not be 0, as the secondary axis would collapse to a single value.")
  }

  invisible(x)
}
