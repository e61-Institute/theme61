#' Adds a black line at y = 0
#'
#' @param y Numeric. The y-intercept of the line. Defaults to 0 since it is
#'   called \code{add_zeroline} after all. But you can set it to 100 if you have
#'   an index on your y-axis, for example. Or any other value if you're feeling
#'   chaotic.
#' @param colour String. The line is black by default but you can change it if
#'   you need another colour.
#' @param linewidth Numeric. Set the thickness of the line. Default (0.25)
#'   should be appropriate for default graph scaling.
#'
#' @return ggplot object
#' @export
add_zeroline <- function(y = 0, colour = "black", linewidth = 0.25) {
  ggplot2::geom_hline(yintercept = y, linewidth = linewidth, colour = colour)
}
