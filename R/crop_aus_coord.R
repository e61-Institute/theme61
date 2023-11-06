#' Crops maps of Australia to exclude outlying territories
#'
#' The ABS default shapefiles for Australia include outlying islands that are
#' of limited interest in any maps we produce. This function provides sensible
#' co-ordinates via [ggplot2::coord_sf()] that crops out those islands
#' in a map.
#'
#' @export
#' @examples
#'   \dontrun{
#'     ggplot(strayr::read_absmap("aus2021")) +
#'       geom_sf() +
#'       crop_aus_coord()
#'   }
#'
crop_aus_coord <- function() {

  coord_sf(xlim = c(114, 153), ylim = c(-10, -43))

}
