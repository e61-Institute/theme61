#' Create e61 colour palette
#'
#' Creates a discrete e61 themed colour palette.
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 12. Using more than 6 colours is not recommended as it may
#'   make it difficult to distinguish between colours.
#' @param reverse Logical. Reverse the standard colour order, defaults to FALSE.
#' @return A vector of hex codes.
#' @export

palette_e61 <- function(n, reverse = FALSE) {

  if (n == 0) stop("You need to specify the number of colours/fills in your palette.")
  if (n > 12) stop("You cannot request more than 12 colours, consider using a
                   continuous colour scale or reducing the number of groups in
                   your data.")

  palette <- get_palette(n)

  if (isTRUE(reverse)) {
    palette <- rev(palette)
  }

  return(palette)

}


#' Get colours for palette functions
#'
#' @param n Numeric.
#'
#' @return Vector of hex codes of the colour palette
#' @noRd
#'

get_palette <- function(n) {
  if (n == 1) {
    palette <- e61_tealdark1
  } else if (n == 2) {
    palette <- c(e61_skylight1,
                 e61_tealdark1)
  } else if (n == 3) {
    palette <- c(e61_skylight1,
                 e61_tealdark1,
                 e61_bluedark1)
  } else if (n == 4) {
    palette <- c(e61_skylight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_orangedark1)
  } else if (n == 5) {
    palette <- c(e61_skylight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_orangelight1,
                 e61_orangedark1)
  } else if (n == 6) {
    palette <- c(e61_skylight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_orangelight1,
                 e61_orangedark1,
                 e61_maroonlight1)
  } else if (n == 7) {
    palette <- c(e61_skylight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_orangelight1,
                 e61_orangedark1,
                 e61_coraldark1,
                 e61_maroonlight1)
  } else if (n == 8) {
    palette <- c(e61_skylight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_greylight1,
                 e61_orangelight1,
                 e61_orangedark1,
                 e61_coraldark1,
                 e61_maroonlight1)
  } else if (n == 9) {
    palette <- c(e61_skylight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_greylight1,
                 e61_orangelight1,
                 e61_orangedark1,
                 e61_coraldark1,
                 e61_maroonlight1,
                 e61_maroondark1)
  } else if (n == 10) {
    palette <- c(e61_skylight1,
                 e61_teallight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_greylight1,
                 e61_orangelight1,
                 e61_orangedark1,
                 e61_coraldark1,
                 e61_maroonlight1,
                 e61_maroondark1)
  } else if (n == 11) {
    palette <- c(e61_skylight1,
                 e61_teallight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_greylight1,
                 e61_greydark1,
                 e61_orangelight1,
                 e61_orangedark1,
                 e61_coraldark1,
                 e61_maroonlight1,
                 e61_maroondark1)
  } else if (n == 12) {
    palette <- c(e61_skylight1,
                 e61_teallight1,
                 e61_tealdark1,
                 e61_bluedark1,
                 e61_greylight4,
                 e61_greylight1,
                 e61_greydark1,
                 e61_orangelight1,
                 e61_orangedark1,
                 e61_coraldark1,
                 e61_maroonlight1,
                 e61_maroondark1)
  }

  return(palette)

}


#' Create a continuous palette
#'
#' @param palette Character. e61 colour palette
#' @param reverse Logical. Reverse colour order.
#' @inheritDotParams grDevices::colorRampPalette
#'
#' @noRd

e61_pal <- function(
    palette = c("light", "dark", "diverging", "grey"),
    reverse = FALSE,
    ...) {

  palette <- match.arg(palette)

  pal <- e61_palette_set[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
