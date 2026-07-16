# Distance from a candidate label position to the nearest point of a data
# series, in physical cm -- so that "close" is comparable on the x and y
# axes even when they're scaled very differently (e.g. a wide date axis vs
# a narrow percentage axis).
#
# v1 scope: "point" and "line" geoms only (bar/area/waterfall etc. would
# need their own distance functions, added later -- see
# get_distance_series_type() in arphit for the shape such a dispatch table
# would eventually take).

#' Physical cm per one data-unit of x and y, for the panel described by a
#' mask (see t61_render_mask()). Used to put x/y distances on a common,
#' comparable scale.
#' @noRd
t61_mask_units_cm <- function(mask) {
  list(
    x_per_unit_cm = (mask$panel$width_px / mask$px_per_cm_x) / diff(mask$x_range),
    y_per_unit_cm = (mask$panel$height_px / mask$px_per_cm_y) / diff(mask$y_range)
  )
}

#' Shortest distance from point P to the segment AB, all already in a
#' common physical unit (e.g. cm), plus the closest point on the segment.
#' @noRd
t61_point_segment_distance <- function(px, py, ax, ay, bx, by) {
  abx <- bx - ax
  aby <- by - ay
  len2 <- abx^2 + aby^2

  if (len2 == 0) {
    return(list(distance = sqrt((px - ax)^2 + (py - ay)^2), x = ax, y = ay))
  }

  t <- ((px - ax) * abx + (py - ay) * aby) / len2
  t <- max(0, min(1, t))

  cx <- ax + t * abx
  cy <- ay + t * aby

  list(distance = sqrt((px - cx)^2 + (py - cy)^2), x = cx, y = cy)
}

#' Distance from a candidate (x, y) (data space) to the nearest point of a
#' series, in cm, plus the data-space coordinate of that nearest point
#' (used later for line-of-sight testing).
#'
#' @param series_x,series_y Data-space coordinates of the series, in draw
#'   order (as ggplot_build() would return them for the layer).
#' @param geom_type "point" or "line".
#' @param units Output of t61_mask_units_cm().
#' @noRd
t61_distance_to_series <- function(x, y, series_x, series_y, geom_type, units) {

  # Scale to a common physical unit (cm). This is a diagonal linear map, so
  # segment geometry computed in this space is exactly the physical
  # distance you'd measure with a ruler on the rendered chart, and the
  # inverse scaling recovers exact data-space coordinates afterwards.
  sx <- units$x_per_unit_cm
  sy <- units$y_per_unit_cm

  px <- x * sx
  py <- y * sy
  qx <- series_x * sx
  qy <- series_y * sy

  if (identical(geom_type, "point")) {
    d <- sqrt((px - qx)^2 + (py - qy)^2)
    i <- which.min(d)
    return(list(distance = d[i], x = series_x[i], y = series_y[i]))
  }

  if (identical(geom_type, "line")) {
    n <- length(qx)
    if (n == 1) {
      d <- sqrt((px - qx)^2 + (py - qy)^2)
      return(list(distance = d, x = series_x, y = series_y))
    }

    best <- NULL
    for (i in seq_len(n - 1)) {
      seg <- t61_point_segment_distance(px, py, qx[i], qy[i], qx[i + 1], qy[i + 1])
      if (is.null(best) || seg$distance < best$distance) best <- seg
    }

    return(list(distance = best$distance, x = best$x / sx, y = best$y / sy))
  }

  stop("t61_distance_to_series: unsupported geom_type '", geom_type, "' (v1 scope: point, line)")
}
