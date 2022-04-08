


#' Get Colours for palette functions
#'
#' @param n Numeric.
#'
#' @return HEX Codes of colour palette
#' @export
#'

get_palette <- function(n) {

  if (n == 1) {
    palette <- theme61::e61_bluedark
  } else if (n == 2) {
    palette <- c(theme61::e61_bluedark,
                 theme61::e61_skylight)
  } else if (n == 3) {
    palette <- c(theme61::e61_skylight,
                 theme61::e61_bluedark,
                 theme61::e61_tealdark)
  } else if (n == 4) {
    palette <- c(theme61::e61_skylight,
                 theme61::e61_bluedark,
                 theme61::e61_skydark,
                 theme61::e61_tealdark)
  } else if (n == 5) {
    palette <- c(theme61::e61_skylight,
                 theme61::e61_bluedark,
                 theme61::e61_skydark,
                 theme61::e61_tealdark,
                 theme61::e61_teallight)
  } else if (n == 6) {
    palette <- c(theme61::e61_bluelight,
                 theme61::e61_skylight,
                 theme61::e61_bluedark,
                 theme61::e61_skydark,
                 theme61::e61_tealdark,
                 theme61::e61_teallight)
  } else if (n == 7) {
    palette <- c(theme61::e61_bluelight,
                 theme61::e61_skylight,
                 theme61::e61_bluedark,
                 theme61::e61_skydark,
                 theme61::e61_tealdark,
                 theme61::e61_teallight,
                 theme61::e61_orangelight)
  } else if (n == 8) {
    palette <- c(theme61::e61_bluelight,
                 theme61::e61_skylight,
                 theme61::e61_bluedark,
                 theme61::e61_skydark,
                 theme61::e61_tealdark,
                 theme61::e61_teallight,
                 theme61::e61_orangelight,
                 theme61::e61_orangedark)
  } else if (n == 9) {
    palette <- c(theme61::e61_bluelight,
                 theme61::e61_skylight,
                 theme61::e61_bluedark,
                 theme61::e61_skydark,
                 theme61::e61_tealdark,
                 theme61::e61_teallight,
                 theme61::e61_orangelight,
                 theme61::e61_orangedark,
                 theme61::e61_greylight)
  } else if (n == 10) {
    palette <- c(theme61::e61_bluelight,
                 theme61::e61_skylight,
                 theme61::e61_bluedark,
                 theme61::e61_skydark,
                 theme61::e61_tealdark,
                 theme61::e61_teallight,
                 theme61::e61_orangelight,
                 theme61::e61_orangedark,
                 theme61::e61_greylight,
                 theme61::e61_greydark)
  }

  return(palette)

}


#' Create e61 colour palette
#'
#' Creates an e61 themes colour palette.
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 10. Using more than 6 colours is not recommended as it may
#'   make it difficult to distinguish between colours.
#' @param reverse Logical. Reverse the standard colour order, defaults to FALSE.
#' @return Returns a vector of HEX codes.
#' @examples
#' @export




e61_palette <- function(n, reverse = FALSE) {

  if (n == 0) stop("You need to specify at least one n.")
  if (n > 10) stop("You cannot request more than 10 colours.")

  palette <- theme61::get_palette(n)

  if (isTRUE(reverse)) {
    palette <- rev(palette)
  }

  return(palette)

}





#' Create Continuous Palette
#'
#' @param palette Character. e61 colour palette
#' @param reverse Logical. Reverse colour order.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
e61_pal <- function(palette = "light", reverse = FALSE, ...) {

  pal <- e61_palette_set[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
