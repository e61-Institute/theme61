# as_e61_plot ----

#' Tag a plot (or a list of plots) with the e61_plot class.
#'
#' The e61_plot tag has to sit first in the class vector so theme61's S3
#' methods (print, ggplot_build) take priority over ggplot2's own.
#' @noRd
as_e61_plot <- function(x) {
  if (inherits(x, "e61_plot")) return(x)

  if (inherits(x, "ggplot")) {
    class(x) <- c("e61_plot", class(x))
    return(x)
  }

  # Checked after ggplot, since a ggplot is itself list-backed.
  if (is.list(x)) return(lapply(x, as_e61_plot))

  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be converted to an e61 plot"
  )
}

# classify_e61_map ----

#' Helper function to check for spatial attributes
#' @noRd
is_spatial <- function(p) {
  any(vapply(p@layers, function(ly) any(data.table::like(class(ly$geom), "*Sf")), logical(1)))
}

#' Whether a plot should be classified as a map, given a `force` override
#' @noRd
should_be_map <- function(x, force) {
  identical(force, TRUE) || (is.null(force) && is_spatial(x))
}

#' Tag a spatial plot (or a list of plots) with the e61_map class.
#'
#' `force = TRUE` bypasses the GeomSf heuristic; `NULL` (the default) leaves
#' the decision to it.
#' @noRd
classify_e61_map <- function(x, force = NULL) {
  if (inherits(x, "e61_map")) return(x)

  if (inherits(x, "ggplot")) {
    if (should_be_map(x, force)) {
      # e61_map sits ahead of e61_plot, which as_e61_plot() adds if missing.
      x <- as_e61_plot(x)
      class(x) <- c("e61_map", class(x))
    }
    return(x)
  }

  # Checked after ggplot, since a ggplot is itself list-backed.
  if (is.list(x)) return(lapply(x, classify_e61_map))

  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be converted to an e61 map"
  )
}

# finalise_e61_plot ----

#' Classify a plot as map/non-map, correcting axis chrome for maps, and
#' apply format_flip()'s theme changes to coord_flip() plots.
#' Idempotent - safe to call repeatedly on the same plot.
#' @noRd
finalise_e61_plot <- function(plot) {
  plot <- classify_e61_map(plot)

  # Corrects a map built with plain theme_e61() instead of theme_e61_spatial(),
  # without overriding elements the user set explicitly.
  if (inherits(plot, "e61_map")) {
    plot <- plot + map_axis_correction(plot)
  }

  # Horizontal bar graphs made with coord_flip() need axis/gridline changes
  # to look right - format_flip() already skips any element the user has
  # customised away from the theme_e61() default via current_theme.
  if (inherits(plot@coordinates, "CoordFlip")) {
    plot <- plot + format_flip(current_theme = plot@theme)
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
