#' Add e61 logo to graph
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'  geom_point() +
#'  add_e61_logo(x=5,5.5,30,35)
#'
#'
add_e61_logo <- function() {

  img <- magick::image_read("https://static.wixstatic.com/media/ec9616_a14b627a0e4f45d4905150b9689eba09~mv2.png/v1/fill/w_863,h_572,al_c,usm_0.66_1.00_0.01,enc_auto/e6lnstitute-Black-Logo-PNG_edited.png")
  img <- magick::image_scale(img, "70")
  g <-
    grid::rasterGrob(
      img,
      interpolate = TRUE,
      x = grid::unit(0.9, "npc"),
      y = grid::unit(0.9, "npc"),
      height = grid::unit(0.1, "npc"),
      width = grid::unit(0.1, "npc")
    )

  return(ggplot2::annotation_custom(g))
}
