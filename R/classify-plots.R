# as_e61_plot ----

#' Generic to coerce plots to e61_plot class
#' @export
as_e61_plot <- function(x, ...) {
  UseMethod("as_e61_plot")
}

#' Method for plain ggplot
#' @export
as_e61_plot.ggplot <- function(x, ...) {
  if (!inherits(x, "e61_plot")) {
    class(x) <- c("e61_plot", class(x))
  }
  x
}

#' Method for existing e61_plot class objects
#' @export
as_e61_plot.e61_plot <- function(x, ...) {
  x
}

#' Method that works for lists of plots
#' @export
as_e61_plot.list <- function(x, ...) {
  lapply(x, as_e61_plot)
}

#' Method that fails for non-plots
#' @export
as_e61_plot.default <- function(x, ...) {
  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be converted to an e61 plot"
  )
}

# classify_e61_map ----

#' Helper function to check for spatial attributes
#' @noRd
is_spatial <- function(p) {

  is_spatial_chart <- FALSE

  for(i in seq_along(p@layers)){

    layer_class <- class(p@layers[[i]]$geom)

    if(any(data.table::like(layer_class, "*Sf"))) {
      is_spatial_chart <- TRUE

      break
    }
  }

  is_spatial_chart

}

#' Generic to coerce plots to e61_map class
#' @export
classify_e61_map <- function(x, ..., force = NULL) {
  UseMethod("classify_e61_map")
}

#' Method for e61_plot
#' @export
classify_e61_map.e61_plot <- function(x, ..., force = NULL) {

  if (identical(force, TRUE) || (is.null(force) && is_spatial(x))) {
    class(x) <- c("e61_map", class(x))
  }

  x
}

#' Method for ggplot
#' @export
classify_e61_map.ggplot <- function(x, ..., force = NULL) {

  if (identical(force, TRUE) || (is.null(force) && is_spatial(x))) {

    # Adds e61_plot if not already present, then prepends e61_map
    x <- as_e61_plot(x)
    class(x) <- c("e61_map", class(x))
  }

  x
}

#' Method for existing e61_map class objects
#' @export
classify_e61_map.e61_map <- function(x, ...) {

  x
}

#' Method that works for lists of plots
#' @export
classify_e61_map.list <- function(x, ...) {
  lapply(x, classify_e61_map)
}

#' Method that fails for non-plots
#' @export
classify_e61_map.default <- function(x, ...) {
  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be converted to an e61 map"
  )
}

# finalise_e61_plot ----

#' Classify a plot as map/non-map, correcting axis chrome for maps.
#' Idempotent - safe to call repeatedly on the same plot.
#' @noRd
finalise_e61_plot <- function(plot) {
  plot <- classify_e61_map(plot)

  # Corrects a map built with plain theme_e61() instead of theme_e61_spatial(),
  # without overriding elements the user set explicitly.
  if (inherits(plot, "e61_map")) {
    plot <- plot + map_axis_correction(plot)
  }

  plot
}

# panel.grid.major.x/.y specifically, not the parent panel.grid.major - a
# child element theme_e61() sets wins over a later parent-level override
# regardless of merge order, so the parent key alone would do nothing.
map_axis_correction <- function(plot) {
  map_args <- list(
    axis.text = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

  baseline <- theme_e61()

  unchanged <- vapply(names(map_args), function(el) {
    identical(plot@theme[[el]], baseline[[el]])
  }, logical(1))

  do.call(theme, map_args[unchanged])
}
