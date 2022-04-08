

#' Set e61 Colour Palettes in scale_*_manual/gradient
#'
#' @param n Numeric. The number of colours in your colour scale.
#' @param reverse Logical. Whether to reverse the colour order.
#' @param discrete Logical. Indicates whether Discrete scale is TRUE.
#' @param palette Character. The specific e61 palette for continuous scales.
#' @param ...
#'
#' @return ggplot2 object
#' @rdname e61_scale
#' @export
#' @import ggplot2
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
                              palette = "light", ...) {
  if (discrete) {
    return(
      ggplot2::scale_colour_manual(...,
                                   values = theme61::e61_palette(n = n,reverse = reverse))
    )
  }

  if (!discrete) {
    pal <- theme61::e61_pal(palette = palette, reverse = reverse)
    return(ggplot2::scale_color_gradientn(colours = pal(256), ...))
  }


}
#' @rdname e61_scale
#' @export
e61_fill_manual <- function(n = 0, reverse = FALSE,
                            discrete = TRUE,
                            palette = "light", ...) {
  if (discrete) {
    return(
      ggplot2::scale_fill_manual(...,
                                 values = theme61::e61_palette(n = n,reverse = reverse))
    )
  }

  if (!discrete) {
    pal <- theme61::e61_pal(palette = palette, reverse = reverse)
    return(ggplot2::scale_fill_gradientn(colours = pal(256), ...))
  }

}

