#' Set e61 colour palettes in scale_*_manual/gradient
#'
#' @param n Numeric. The number of colours in your colour scale.
#' @param reverse Logical. TRUE reverses the colour order.
#' @param discrete Logical. Indicate whether to use a discrete scale.
#' @param palette Character. The specific e61 palette for continuous scales.
#' @inheritDotParams ggplot2::scale_colour_manual
#'
#' @return ggplot2 object
#' @rdname e61_scale
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'    geom_point() +
#'    e61_colour_manual(n = 3) +
#'    theme_e61()
#'
#'
e61_colour_manual <- function(n = 0,
                              reverse = FALSE,
                              discrete = TRUE,
                              palette = c("light", "dark", "diverging", "grey"),
                              ...) {
  if (discrete) {
    return(ggplot2::scale_colour_manual(
      ...,
      values = e61_palette(n = n, reverse = reverse))
      )
  }

  if (!discrete) {
    pal <- e61_pal(palette = palette, reverse = reverse)
    return(ggplot2::scale_color_gradientn(colours = pal(256), ...))
  }


}

#' @rdname e61_scale
#' @export
e61_fill_manual <- function(n = 0, reverse = FALSE,
                            discrete = TRUE,
                            palette = c("light", "dark", "diverging", "grey"),
                            ...) {
  if (discrete) {
    return(
      ggplot2::scale_fill_manual(...,
                                 values = e61_palette(n = n,reverse = reverse))
    )
  }

  if (!discrete) {
    pal <- e61_pal(palette = palette, reverse = reverse)
    return(ggplot2::scale_fill_gradientn(colours = pal(256), ...))
  }

}

#' A consistent set of colours for Australian states and territories for
#' graphing
#'
#' @inheritDotParams ggplot2::scale_colour_manual
#'
#' @return ggplot2 object
#' @rdname e61_scale_aus
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' graph_data <- data.frame(
#'   state = c("AUS", "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
#'   value = runif(9)
#'   )
#'
#' ggplot(graph_data, aes(x = state, y = value, fill = state)) +
#'   geom_col() +
#'   e61_fill_aus()

e61_colour_aus <- function(...) {

  ggplot2::scale_colour_manual(values = e61_aus_colours, limits = force, ...)

}

#' @inheritDotParams ggplot2::scale_fill_manual
#'
#' @rdname e61_scale_aus
#' @export

e61_fill_aus <- function(...) {

  ggplot2::scale_fill_manual(values = e61_aus_colours, limits = force, ...)

}
