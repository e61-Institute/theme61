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
#' several panels in a multi-panel save - don't re-read it from disk every
#' time, and a missing/corrupt file raises a clear theme61-specific error
#' instead of an uncaught magick one.
#' @noRd
t61_get_logo <- function() {

  if (!is.null(t61_env$logo)) {
    return(t61_env$logo)
  }

  logo_path <- system.file("extdata", "e61-black-logo.png", package = "theme61")

  img <- tryCatch(
    magick::image_read(logo_path),
    error = function(e) {
      cli::cli_abort(
        "Could not read the bundled e61 logo image for add_e61_logo().",
        parent = e
      )
    }
  )

  img <- magick::image_scale(img, "70")

  t61_env$logo <- img

  img
}
