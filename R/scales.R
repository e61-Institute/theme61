#' Set e61 colour palettes in scale_*_manual/gradient
#'
#' @param n Numeric. The number of colours in your colour scale, required for
#'   discrete scales.
#' @param reverse Logical. TRUE reverses the colour order. Defaults to FALSE.
#' @param discrete Logical. Indicate whether to use a discrete scale. Defaults
#'   to TRUE.
#' @param palette Character. The specific e61 palette for continuous scales.
#'   Must be supplied if a continuous scale is used.
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

e61_colour_manual <- function(n = 0,
                              reverse = FALSE,
                              discrete = TRUE,
                              palette = c("light", "dark", "diverging", "grey"),
                              ...) {

  palette <- match.arg(palette)

  if (discrete) {

      if(n == 1) {
        ret_vals <- e61_tealdark1
      } else if(n == 2){
        ret_vals <- c(e61_skylight1, e61_tealdark1)
      } else if(n == 3) {
        ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50")
      } else if(n == 4) {
        ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50", e61_orangedark1)
      } else if(n == 5) {
        ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50", e61_orangelight1, e61_orangedark1)
      } else if(n == 6) {
        ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50", e61_orangelight1, e61_orangedark1, e61_maroonlight1)
      } else if(n == 7) {
        ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1)
      } else if(n == 8) {
        ret_vals <- c(e61_skylight1, e61_tealdark1, e61_bluedark1, "grey50", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1)
      } else if(n == 9) {
        ret_vals <- c(e61_skylight1, e61_tealdark1, e61_bluedark1, "grey50", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1, e61_maroondark1)
      } else if(n == 10) {
        ret_vals <- c(e61_skylight1, e61_teallight1, e61_tealdark1, e61_bluedark1, "grey50", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1, e61_maroondark1)
      } else if(n == 11) {
        ret_vals <- c(e61_skylight1, e61_teallight1, e61_tealdark1, e61_bluedark1, "grey50", "grey30", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1, e61_maroondark1)
      } else if(n == 12) {
        ret_vals <- c(e61_skylight1, e61_teallight1, e61_tealdark1, e61_bluedark1, "grey70", "grey50", "grey30", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1, e61_maroondark1)
      } else {
        ret_vals <- e61_palette(n = n, reverse = reverse)
      }

    return(ggplot2::scale_fill_manual(values = ret_vals))
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

  palette <- match.arg(palette)

  if (discrete) {

    if(n == 1) {
      ret_vals <- e61_tealdark1
    } else if(n == 2){
      ret_vals <- c(e61_skylight1, e61_tealdark1)
    } else if(n == 3) {
      ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50")
    } else if(n == 4) {
      ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50", e61_orangedark1)
    } else if(n == 5) {
      ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50", e61_orangelight1, e61_orangedark1)
    } else if(n == 6) {
      ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50", e61_orangelight1, e61_orangedark1, e61_maroonlight1)
    } else if(n == 7) {
      ret_vals <- c(e61_skylight1, e61_tealdark1, "grey50", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1)
    } else if(n == 8) {
      ret_vals <- c(e61_skylight1, e61_tealdark1, e61_bluedark1, "grey50", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1)
    } else if(n == 9) {
      ret_vals <- c(e61_skylight1, e61_tealdark1, e61_bluedark1, "grey50", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1, e61_maroondark1)
    } else if(n == 10) {
      ret_vals <- c(e61_skylight1, e61_teallight1, e61_tealdark1, e61_bluedark1, "grey50", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1, e61_maroondark1)
    } else if(n == 11) {
      ret_vals <- c(e61_skylight1, e61_teallight1, e61_tealdark1, e61_bluedark1, "grey50", "grey30", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1, e61_maroondark1)
    } else if(n == 12) {
      ret_vals <- c(e61_skylight1, e61_teallight1, e61_tealdark1, e61_bluedark1, "grey70", "grey50", "grey30", e61_orangelight1, e61_orangedark1, e61_coraldark1, e61_maroonlight1, e61_maroondark1)
    } else {
      ret_vals <- e61_palette(n = n, reverse = reverse)
    }

    return(ggplot2::scale_fill_manual(values = ret_vals))
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
