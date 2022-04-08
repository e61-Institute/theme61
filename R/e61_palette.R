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

  palette <- get_palette(n)

  if (isTRUE(reverse)) {
    palette <- rev(palette)
  }

  return(palette)

}

get_palette <- function(n) {

  if (n == 1) {
    palette <- "bluedark"
  } else if (n == 2) {
    palette <- c("bluedark",
                 "tealdark")
  } else if (n == 3) {
    palette <- c("skylight",
                 "bluedark",
                 "tealdark")
  } else if (n == 4) {
    palette <- c("skylight",
                 "bluedark",
                 "skydark",
                 "tealdark")
  } else if (n == 5) {
    palette <- c("skylight",
                 "bluedark",
                 "skydark",
                 "tealdark",
                 "teallight")
  } else if (n == 6) {
    palette <- c("bluelight",
                 "skylight",
                 "bluedark",
                 "skydark",
                 "tealdark",
                 "teallight")
  } else if (n == 7) {
    palette <- c("bluelight",
                 "skylight",
                 "lightbluedark",
                 "skydark",
                 "tealdark",
                 "teallight",
                 "orangelight")
  } else if (n == 8) {
    palette <- c(
      "bluelight",
      "skylight",
      "lightbluedark",
      "skydark",
      "tealdark",
      "teallight",
      "orangelight",
      "orangedark"
    )
  } else if (n == 9) {
    palette <- c(
      "bluelight",
      "skylight",
      "lightbluedark",
      "skydark",
      "tealdark",
      "teallight",
      "orangelight",
      "orangedark",
      "greylight"
    )
  } else if (n == 10) {
    palette <- c(
      "bluelight",
      "skylight",
      "lightbluedark",
      "skydark",
      "tealdark",
      "teallight",
      "orangelight",
      "orangedark",
      "greylight",
      "greydark"
    )
  }

  # Prepends e61_ to match the data object names
  palette <- sapply(palette, function(x) get(paste0("e61_", x)))

  return(palette)

}


e61_pal <- function(palette = "full", reverse = FALSE, ...) {

  pal <- theme61::e61_palette_set[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
