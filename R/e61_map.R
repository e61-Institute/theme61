#' Add an underlying base map to ggplot maps
#'
#' Wrapper around [ggmap::get_stadiamap()] that makes it easy to insert a base
#' map underneath a ggplot-based map. The default map type (Stamen's Toner Lite
#' map) is designed to be unintrusive so the focus remains on the data being
#' visualised.
#'
#' @param bbox A named vector of numeric coordinates defining the extent of your
#'   map, e.g `c(top = -33, right = 151, bottom = -34, left = 150)`. Defaults to
#'   bounds for Greater Sydney.
#' @param adjust Numeric. Adjust default zoom value.
#' @param maptype Character. Select a valid map type from
#'   [ggmap::get_stadiamap()]'s `maptype` argument. The default is set to
#'   `stamen_toner_lite`.
#' @return A ggmap object
#' @export
#' @family map functions
#' @examples
#'
#' library(sf)
#' sa3 <- strayr::read_absmap("sa32021")
#' sa3 <- sa3[sa3$gcc_code_2021 == "1GSYD", ] # let's just look at Sydney
#'
#'  ggplot(sa3) +
#'  add_map_e61(bbox = c(
#'    left = min(sa3$cent_long),
#'    bottom = min(sa3$cent_lat),
#'    right = max(sa3$cent_long),
#'    top = max(sa3$cent_lat))
#'    ) +
#'  geom_point(aes(x = cent_long, y = cent_lat)) + #plot points
#'  theme_e61_spatial()
#'
add_map_e61 <-
  function(bbox = c(
    top = -33.757742,
    right = 151.492882,
    bottom = -34.024779,
    left = 150.839539
  ),
  adjust = 0,
  maptype = "stamen_toner_lite") {

    # Check if req version of ggmap is installed
    inst_v <- packageVersion("ggmap")
    if (inst_v < "3.0.2") {
      cli::cli_alert_warning("Your installed version of ggmap does not support Stadia Maps. Running `setup_stadia_maps()` to help you set up...")
      return(setup_stadia_maps())
    }

    # Check if API key is setup
    if (!ggmap::has_stadiamaps_key()) stop("You must provide a Stadia Maps API key to load map tiles, see `setup_stadia_maps()`.")

    # Some guards for common mistakes
    if (bbox[["top"]] > 0 || bbox[["bottom"]] > 0) warning("Your latitude coordinates are >0, are you sure you aren't missing a negative sign somewhere? Australia is south of the equator so latitude is negative.")
    if (bbox[["top"]] < bbox[["bottom"]]) stop("Your top coordinate must be greater than your bottom coordinate.")
    if (bbox[["right"]] < bbox[["left"]]) stop("Your right coordinate must be greater than your left coordinate.")
    if (bbox[["top"]] == bbox[["bottom"]] || bbox[["right"]] == bbox[["left"]]) stop("Your bounding box coordinates can't be equal.")

    # Set up bounding box and other info needed to pull the right tiles
    lon_range <- bbox[c("left", "right")]
    lat_range <- bbox[c("bottom", "top")]
    lonlength <- diff(lon_range)
    latlength <- diff(lat_range)
    zoomlon <- ceiling(log2(360 * 2 / lonlength))
    zoomlat <- ceiling(log2(180 * 2 / latlength))
    zoom <- max(zoomlon, zoomlat) + ceiling(adjust)

    # Load tiles and set up map image
    ggmap <- ggmap::get_stadiamap(bbox = bbox, zoom = zoom, maptype = maptype)
    xmin <- attr(ggmap, "bb")$ll.lon
    xmax <- attr(ggmap, "bb")$ur.lon
    ymin <- attr(ggmap, "bb")$ll.lat
    ymax <- attr(ggmap, "bb")$ur.lat
    p <- ggmap::inset_raster(ggmap, xmin, xmax, ymin, ymax)

    return(p)
  }


#' Setup Stadia Maps API
#'
#' Using Stadia Maps (formerly Stamen Maps) tiles requires setting up a free API
#' key and registering it in your R session, this helper function helps you set
#' up your session accordingly.
#'
#' To get an API key, you must sign up at
#' [https://client.stadiamaps.com/signup/].
#'
#' @export
#' @family map functions
setup_stadia_maps <- function() {

  # Check if ggmap package version has the required functions, install from Github if it doesn't
  inst_v <- packageVersion("ggmap")
  if (inst_v < "3.0.2") {
    install_prompt <- ""

    while (install_prompt == "") {
      install_prompt <- readline("Your installed version of ggmap is does not support downloading Stadia Map tiles. Enter 'Y' to update or 'N' to exit.")
    }

    if (install_prompt == "Y") {
      if (!requireNamespace("remotes")) install.packages("remotes")
      remotes::install_github("dkahle/ggmap")
    } else if (install_prompt == "N") {
      return(cli::cli_alert_info("Stopping Stadia Maps setup process. Note that this means `add_map_e61()` may not work."))
    }

  }

  # Prompt user: do you have an API key from Stadia Maps, provide the URL to navigate to if no...
  api_prompt <- ""

  while (api_prompt == "") {
    api_prompt <- readline("Have you have registered an API key from Stadia Maps? Enter 'Y' for yes or 'N' for no.")
  }

  if (api_prompt == "Y") {
    api <- readline("Please type or paste in your API key:")
    ggmap::register_stadiamaps(key = api, write = FALSE)

    if (ggmap::has_stadiamaps_key()) {
      return(cli::cli_alert_success("You have successfully entered your Stadia Maps API key for this session. This key needs to be registered in each new R session using the `ggmap::register_stadiamaps()` function."))
    } else {
      return(cli::cli_alert_warning("You have not entered your API key correctly, please try again using the `ggmap::register_stadiamaps()` function."))
    }

  } else if (api_prompt == "N") {
    # ...otherwise prompt to enter the key
    return(cli::cli_alert_info("In order to use map files from Stadia Maps, you need to first register a free API key from https://client.stadiamaps.com/signup/ and then run `setup_stadia_maps()` again to register your key in your R session."))

  }
}
