#' Adds a black line at y = 0
#'
#' @param colour String. The line is black by default but you can change it if
#'   you need another colour.
#' @param linewidth Numeric. Set the thickness of the line. Default (0.25)
#'   should be appropriate for default graph scaling.
#'
#' @export
add_zeroline <- function(colour = "black", linewidth = 0.25) {
  geom_hline(yintercept = 0, linewidth = linewidth, colour = colour)
}
