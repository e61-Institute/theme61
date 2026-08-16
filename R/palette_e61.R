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
#' Validates `n` itself (rather than relying on callers to pre-check it) so
#' that every caller - including scale_colour_e61()/scale_fill_e61(), which
#' pass this function straight into ggplot2::discrete_scale() without any
#' validation of their own - gets a clear error for an out-of-range `n`,
#' rather than a cryptic "object not found" from an unmatched branch.
#'
#' @param n Numeric.
#'
#' @return Vector of hex codes of the colour palette
#' @noRd
#'

get_palette <- function(n) {

  if (!is.numeric(n) || length(n) != 1 || n != round(n) || n < 1 || n > 12) {
    cli::cli_abort(
      "theme61 does not support more than 12 discrete colours/fills automatically ({n} requested). Please supply your own scale (e.g. scale_colour_manual()/scale_fill_manual())."
    )
  }

  palettes <- list(
    `1` = e61_tealdark1,
    `2` = c(e61_skylight1,
            e61_tealdark1),
    `3` = c(e61_skylight1,
            e61_tealdark1,
            e61_orangedark1),
    `4` = c(e61_skylight1,
            e61_tealdark1,
            e61_orangedark1,
            e61_maroonlight1),
    `5` = c(e61_skylight1,
            e61_tealdark1,
            e61_orangelight1,
            e61_orangedark1,
            e61_maroonlight1),
    `6` = c(e61_skylight1,
            e61_tealdark1,
            e61_bluedark1,
            e61_orangelight1,
            e61_orangedark1,
            e61_maroonlight1),
    `7` = c(e61_skylight1,
            e61_tealdark1,
            e61_bluedark1,
            e61_orangelight1,
            e61_orangedark1,
            e61_coraldark1,
            e61_maroonlight1),
    `8` = c(e61_skylight1,
            e61_tealdark1,
            e61_bluedark1,
            e61_greylight1,
            e61_orangelight1,
            e61_orangedark1,
            e61_coraldark1,
            e61_maroonlight1),
    `9` = c(e61_skylight1,
            e61_tealdark1,
            e61_bluedark1,
            e61_greylight1,
            e61_orangelight1,
            e61_orangedark1,
            e61_coraldark1,
            e61_maroonlight1,
            e61_maroondark1),
    `10` = c(e61_skylight1,
             e61_teallight1,
             e61_tealdark1,
             e61_bluedark1,
             e61_greylight1,
             e61_orangelight1,
             e61_orangedark1,
             e61_coraldark1,
             e61_maroonlight1,
             e61_maroondark1),
    `11` = c(e61_skylight1,
             e61_teallight1,
             e61_tealdark1,
             e61_bluedark1,
             e61_greylight1,
             e61_greydark1,
             e61_orangelight1,
             e61_orangedark1,
             e61_coraldark1,
             e61_maroonlight1,
             e61_maroondark1),
    `12` = c(e61_skylight1,
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
  )

  palettes[[as.character(n)]]
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

#' Generates an 8 gradient colour palette
#'
#' @param colour Vector of colours to generate.
#' @param base_gradient A shade of grey that acts as the end of the gradient
#'   ramp.
#' @noRd
gen_palette <- function(colour, base_gradient = "#eaeaea") {
  lapply(colour, function(x) {
    rev(grDevices::colorRampPalette(c(base_gradient, x))(9))[1:8]
    })
}
