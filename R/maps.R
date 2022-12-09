#' Wrapper function for ggmap to produce a map layer in e61 format
#'
#' Simple wrapper around \code{\link[ggmap]{get_stamenmap}} that makes it easier to
#' overlay a map layer for geographical data in ggplot
#'
#' @details The primary purpose of this function is to add a map layer with major place
#' names and roads from Stamen's map server to plot under geographical data.
#' @param bbox A named vector of coordinates decribing the extent of your map,
#' e.g \code{c(top = -33, right = 151, bottom = -34, left = 150)}
#' @param adjust Adjust default zoom value
#' @param maptype Set as the default map type for the e61 style, you can change to another
#' maptype if needed
#'
#' @export
#' @examples
#'
#' library(sf)
#' library(dplyr)
#' sa3 <- filter(absmapsdata::sa32021, gcc_code_2021 == "1GSYD") # let's just look at Sydney
#'
#'  ggplot(sa3) +
#'  e61_map(bbox = c(left = min(sa3$cent_long), bottom = min(sa3$cent_lat), right = max(sa3$cent_long), top = max(sa3$cent_lat))) +
#'  geom_point(aes(x = cent_long, y = cent_lat)) #plot points
#'
#'

e61_map <-
  function(bbox = c(
    top = -33.757742,
    right = 151.492882,
    bottom = -34.024779,
    left = 150.839539
  ),
  adjust = 0,
  maptype = "toner-lite") {

    lon_range <- bbox[c("left", "right")]
    lat_range <- bbox[c("bottom", "top")]
    lonlength <- diff(lon_range)
    latlength <- diff(lat_range)
    zoomlon <- ceiling(log2(360 * 2 / lonlength))
    zoomlat <- ceiling(log2(180 * 2 / latlength))
    zoom <- max(zoomlon, zoomlat) + ceiling(adjust)

    ggmap <-
      ggmap::get_stamenmap(bbox = bbox,
                           zoom = zoom,
                           maptype = maptype)
    xmin <- attr(ggmap, "bb")$ll.lon
    xmax <- attr(ggmap, "bb")$ur.lon
    ymin <- attr(ggmap, "bb")$ll.lat
    ymax <- attr(ggmap, "bb")$ur.lat
    p <- ggmap::inset_raster(ggmap, xmin, xmax, ymin, ymax)

    return(p)
  }













