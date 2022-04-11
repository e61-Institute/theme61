

#' Add e61 Logo to graph
#'
#'
#' @return ggplot2 object
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'  geom_point() +
#'  add_e61_logo(x=5,5.5,30,35)
#'
#'
add_e61_logo<-function(){
  logo_url<-"https://static.wixstatic.com/media/ec9616_a14b627a0e4f45d4905150b9689eba09~mv2.png/v1/fill/w_863,h_572,al_c,usm_0.66_1.00_0.01,enc_auto/e6lnstitute-Black-Logo-PNG_edited.png"
  img <- magick::image_read(logo_url)
  img <- magick::image_scale(img, "70")
  g <- grid::rasterGrob(img, interpolate=TRUE,x = unit(0.9, "npc"), y = unit(0.9, "npc"),height=unit(0.1, "npc"),width=unit(0.1, "npc"))
  return(ggplot2::annotation_custom(g))

}
