#' Colour and fill scales using colours from the e61 palette
#'
#' The colour and fill scales are designed for discrete scales. If the data are
#' continuous, we recommend binning the data as this often makes it easier to
#' distinguish between values than a continuous scale. If a continuous scale is
#' desired, the `discrete` argument can be set to `FALSE`.
#'
#' @param reverse Logical. TRUE reverses the colour order. Defaults to FALSE.
#' @param discrete Logical. Indicate whether to use a discrete scale. Defaults
#'   to TRUE.
#' @param palette Character. The specific e61 palette for continuous scales.
#'   Must be supplied if a continuous scale is used.
#' @param n `r lifecycle::badge("deprecated")` n is no longer used.
#' @inheritDotParams ggplot2::scale_colour_manual
#'
#' @return ggplot2 object
#' @rdname scale_e61
#' @export
#'
#' @examples
#'
#' ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'    geom_point() +
#'    scale_colour_e61() +
#'    theme_e61()

scale_colour_e61 <- function(reverse = FALSE,
                             discrete = TRUE,
                             aesthetics = "colour",
                             palette = c("light", "dark", "diverging", "grey"),
                             n = NULL,
                             ...) {

  if (!missing("n"))
    lifecycle::deprecate_stop(when = "0.6.0",
                              what = "scale_colour_e61(n)",
                              details = c("!" = "Please remove it from your function call."))

  palette <- match.arg(palette)

  if (discrete) {
    retval <- discrete_scale(aesthetics, palette = get_palette, ...)

  } else {

    pal <- e61_pal(palette = palette, reverse = reverse)
    retval <- ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }

  class(retval) <- c(class(retval), "scale_col_e61")

  return(retval)
}

#' @rdname scale_e61
#' @export
scale_fill_e61 <- function(reverse = FALSE,
                           discrete = TRUE,
                           aesthetics = "fill",
                           palette = c("light", "dark", "diverging", "grey"),
                           n = NULL,
                           ...) {

  if (!missing("n"))
    lifecycle::deprecate_stop(when = "0.6.0",
                              what = "scale_fill_e61(n)",
                              details = c("!" = "Please remove it from your function call."))

  palette <- match.arg(palette)

  if (discrete) {
    retval <- discrete_scale(aesthetics, palette = get_palette, ...)

  } else {

    pal <- e61_pal(palette = palette, reverse = reverse)
    retval <- ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }

  class(retval) <- c(class(retval), "scale_col_e61")

  return(retval)
}

#' A consistent set of colours for Australian states and territories for
#' graphing
#'
#' @inheritDotParams ggplot2::scale_colour_manual
#'
#' @return ggplot2 object
#' @rdname scale_e61_aus
#' @export
#'
#' @examples
#'
#' graph_data <- data.frame(
#'   state = c("AUS", "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
#'   value = runif(9)
#'   )
#'
#' ggplot(graph_data, aes(x = state, y = value, fill = state)) +
#'   geom_col() +
#'   scale_fill_e61_aus()

scale_colour_e61_aus <- function(...) {

  ggplot2::scale_colour_manual(values = e61_aus_colours, limits = force, ...)

}

#' @inheritDotParams ggplot2::scale_fill_manual
#'
#' @rdname scale_e61_aus
#' @export

scale_fill_e61_aus <- function(...) {

  ggplot2::scale_fill_manual(values = e61_aus_colours, limits = force, ...)

}
