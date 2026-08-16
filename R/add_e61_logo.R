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

  img <- magick::image_read(system.file("extdata", "e61-black-logo.png", package = "theme61"))
  img <- magick::image_scale(img, "70")
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
