


#' Get colours for palette functions
#'
#' @param n Numeric.
#'
#' @return Vector of hex codes of the colour palette
#' @export
#'

get_palette <- function(n) {
  if (n == 1) {
    palette <- e61_bluedark
  } else if (n == 2) {
    palette <- c(e61_bluedark,
                 e61_skylight)
  } else if (n == 3) {
    palette <- c(e61_skylight,
                 e61_bluedark,
                 e61_tealdark)
  } else if (n == 4) {
    palette <- c(e61_skylight,
                 e61_bluedark,
                 e61_skydark,
                 e61_tealdark)
  } else if (n == 5) {
    palette <- c(e61_skylight,
                 e61_bluedark,
                 e61_skydark,
                 e61_tealdark,
                 e61_teallight)
  } else if (n == 6) {
    palette <- c(
      e61_bluelight,
      e61_skylight,
      e61_bluedark,
      e61_skydark,
      e61_tealdark,
      e61_teallight
    )
  } else if (n == 7) {
    palette <- c(
      e61_bluelight,
      e61_skylight,
      e61_bluedark,
      e61_skydark,
      e61_tealdark,
      e61_teallight,
      e61_orangelight
    )
  } else if (n == 8) {
    palette <- c(
      e61_bluelight,
      e61_skylight,
      e61_bluedark,
      e61_skydark,
      e61_tealdark,
      e61_teallight,
      e61_orangelight,
      e61_orangedark
    )
  } else if (n == 9) {
    palette <- c(
      e61_bluelight,
      e61_skylight,
      e61_bluedark,
      e61_skydark,
      e61_tealdark,
      e61_teallight,
      e61_orangelight,
      e61_orangedark,
      e61_greylight
    )
  } else if (n == 10) {
    palette <- c(
      e61_bluelight,
      e61_skylight,
      e61_bluedark,
      e61_skydark,
      e61_tealdark,
      e61_teallight,
      e61_orangelight,
      e61_orangedark,
      e61_greylight,
      e61_greydark
    )
  }

  return(palette)

}


#' Create e61 colour palette
#'
#' Creates a discrete e61 themed colour palette.
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 10. Using more than 6 colours is not recommended as it may
#'   make it difficult to distinguish between colours.
#' @param reverse Logical. Reverse the standard colour order, defaults to FALSE.
#' @return A vector of hex codes.
#' @export

e61_palette <- function(n, reverse = FALSE) {

  if (n == 0) stop("You need to specify at least one n.")
  if (n > 10) stop("You cannot request more than 10 colours.")

  palette <- get_palette(n)

  if (isTRUE(reverse)) {
    palette <- rev(palette)
  }

  return(palette)

}


#' Create a continuous palette
#'
#' @param palette Character. e61 colour palette
#' @param reverse Logical. Reverse colour order.
#' @inheritDotParams grDevices::colorRampPalette
#'
#' @return
#' @export

e61_pal <- function(
    palette = c("light", "dark", "diverging", "grey"),
    reverse = FALSE,
    ...) {

  pal <- e61_palette_set[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
