#' Add e61 logo to graph
#'
#' @param x,y Numeric. Set the x and y position of the logo. Value needs to be
#'   between 0 and 1.
#' @param size Numeric. Set the height and width of the logo.
#' @return ggplot2 object
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'  geom_point() +
#'  add_e61_logo()
#'
#'
add_e61_logo <- function(x = 0.9, y = 0.9, size = 0.1) {

  img <- t61_get_logo()

  g <-
    grid::rasterGrob(
      img,
      interpolate = TRUE,
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      height = grid::unit(size, "npc"),
      width = grid::unit(size, "npc")
    )

  return(ggplot2::annotation_custom(g))
}

#' Fetch (and cache for the session) the e61 logo image used by
#' add_e61_logo(). Cached in `t61_env` so repeated calls - e.g. across
#' several panels in a multi-panel save - don't re-download it over the
#' network every time, and a network failure raises a clear theme61-specific
#' error instead of an uncaught magick one.
#' @noRd
t61_get_logo <- function() {

  if (!is.null(t61_env$logo)) {
    return(t61_env$logo)
  }

  img <- tryCatch(
    magick::image_read("https://static.wixstatic.com/media/ec9616_a14b627a0e4f45d4905150b9689eba09~mv2.png/v1/fill/w_863,h_572,al_c,usm_0.66_1.00_0.01,enc_auto/e6lnstitute-Black-Logo-PNG_edited.png"),
    error = function(e) {
      cli::cli_abort(
        "Could not download the e61 logo image for add_e61_logo(). Check your internet connection.",
        parent = e
      )
    }
  )

  img <- magick::image_scale(img, "70")

  t61_env$logo <- img

  img
}
