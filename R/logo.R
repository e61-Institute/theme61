logo_url<-"https://static.wixstatic.com/media/ec9616_a14b627a0e4f45d4905150b9689eba09~mv2.png/v1/fill/w_863,h_572,al_c,usm_0.66_1.00_0.01,enc_auto/e6lnstitute-Black-Logo-PNG_edited.png"
img <- magick::image_read(system.file("img", logo_url, package="theme61"))
g <- grid::rasterGrob(img, interpolate=TRUE)


#' Add e61 Logo to graph
#'
#' @param x1 Minimum x co-ordinates
#' @param x2 Maximum x co-ordinates
#' @param y1 Minimum y co-ordinates
#' @param y2 Maximum y co-ordinates
#'
#' @return ggplot2 object
#' @import ggplot2
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'  geom_point() +
#'  add_e61_logo(x=5,6,30,35)
#'
#'
add_e61_logo<-function(x1=-Inf, x2=Inf, y1=-Inf, y2=Inf){

  return(ggplot2::annotation_custom(g, xmin=x1, xmax=x2, ymin=y1, ymax=y2))

}
