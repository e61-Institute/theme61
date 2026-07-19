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

#' Shortest distance from point P to the axis-aligned rectangle
#' [xmin,xmax] x [ymin,ymax], all in a common physical unit (e.g. cm). 0 if
#' P is inside (or on the boundary of) the rectangle.
#' @noRd
t61_point_rect_distance <- function(px, py, xmin, xmax, ymin, ymax) {
  dx <- pmax(xmin - px, 0, px - xmax)
  dy <- pmax(ymin - py, 0, py - ymax)
  sqrt(dx^2 + dy^2)
}

#' Convert a candidate's pixel bounding box (see t61_text_box_px()) back to
#' a data-space rectangle, via the inverse of t61_data_to_px().
#' @noRd
t61_box_data_rect <- function(box, mask) {
  panel <- mask$panel
  col_to_x <- function(col) mask$x_range[1] + (col - panel$left_px) / panel$width_px * diff(mask$x_range)
  row_to_y <- function(row) mask$y_range[2] - (row - panel$top_px) / panel$height_px * diff(mask$y_range)

  x1 <- col_to_x(min(box$col_range)); x2 <- col_to_x(max(box$col_range))
  y1 <- row_to_y(min(box$row_range)); y2 <- row_to_y(max(box$row_range))

  list(xmin = min(x1, x2), xmax = max(x1, x2), ymin = min(y1, y2), ymax = max(y1, y2))
}

#' Distance from a candidate's actual rendered footprint (its text bounding
#' box, not just its anchor point) to the nearest point of a series, in cm,
#' plus the data-space coordinate of that nearest point (used later for
#' line-of-sight testing).
#'
#' t61_distance_to_series() measures from the anchor (x, y) alone, which is
#' only the box's own corner/edge when hjust/vjust is 0.5 -- for the
#' default hjust = 0, the box extends a full label-width away from the
#' anchor, so an anchor that clears the buffer comfortably can still leave
#' the box's near edge sitting right on top of a point (confirmed: an
#' anchor 1.4cm from the nearest point in a dense cluster left only 0.02cm
#' of actual box clearance). Using the box's footprint instead makes the
#' buffer represent real visual clearance regardless of justification.
#'
#' For "point", this is exact (point-to-rectangle distance per point). For
#' "line", the true minimum distance between a segment and a rectangle --
#' both convex shapes -- is always realised either at a rectangle corner
#' (distance to the segment) or a segment endpoint (distance to the
#' rectangle), so checking both is exact, not an approximation. For
#' "column", each bar is itself a rectangle (xmin/xmax/ymin/ymax, already
#' position-adjusted -- e.g. dodged -- since it comes from
#' ggplot_build()$data), so this is an exact rectangle-to-rectangle
#' distance per bar.
#'
#' @param series For "point"/"line": list(x=, y=) in draw order. For
#'   "column": list(xmin=, xmax=, ymin=, ymax=), one entry per bar.
#' @noRd
t61_box_distance_to_series <- function(box, mask, series, geom_type, units) {
  rect <- t61_box_data_rect(box, mask)
  sx <- units$x_per_unit_cm
  sy <- units$y_per_unit_cm

  rxmin <- rect$xmin * sx; rxmax <- rect$xmax * sx
  rymin <- rect$ymin * sy; rymax <- rect$ymax * sy

  if (identical(geom_type, "point")) {
    qx <- series$x * sx
    qy <- series$y * sy
    d <- t61_point_rect_distance(qx, qy, rxmin, rxmax, rymin, rymax)
    i <- which.min(d)
    return(list(distance = d[i], x = series$x[i], y = series$y[i]))
  }

  if (identical(geom_type, "line")) {
    series_x <- series$x; series_y <- series$y
    n <- length(series_x)
    if (n == 1) {
      qx <- series_x * sx; qy <- series_y * sy
      d <- t61_point_rect_distance(qx, qy, rxmin, rxmax, rymin, rymax)
      return(list(distance = d, x = series_x, y = series_y))
    }

    corners <- expand.grid(x = c(rxmin, rxmax), y = c(rymin, rymax))
    best <- NULL

    update_best <- function(d, x, y) {
      if (is.null(best) || d < best$distance) best <<- list(distance = d, x = x, y = y)
    }

    for (i in seq_len(n - 1)) {
      ax <- series_x[i] * sx;     ay <- series_y[i] * sy
      bx <- series_x[i + 1] * sx; by <- series_y[i + 1] * sy

      # Segment endpoints against the rectangle.
      update_best(t61_point_rect_distance(ax, ay, rxmin, rxmax, rymin, rymax), series_x[i], series_y[i])
      update_best(t61_point_rect_distance(bx, by, rxmin, rxmax, rymin, rymax), series_x[i + 1], series_y[i + 1])

      # Rectangle corners against the segment.
      for (k in seq_len(nrow(corners))) {
        seg <- t61_point_segment_distance(corners$x[k], corners$y[k], ax, ay, bx, by)
        update_best(seg$distance, seg$x / sx, seg$y / sy)
      }
    }

    return(best)
  }

  if (identical(geom_type, "column")) {
    xmin <- series$xmin * sx; xmax <- series$xmax * sx
    ymin <- series$ymin * sy; ymax <- series$ymax * sy

    # A representative "nearest point" for line-of-sight purposes only
    # (not used for the distance itself): the closest point on each bar's
    # boundary to the candidate box's centre.
    box_cx <- (rxmin + rxmax) / 2
    box_cy <- (rymin + rymax) / 2

    n <- length(xmin)
    best <- NULL
    for (i in seq_len(n)) {
      dx <- max(0, rxmin - xmax[i], xmin[i] - rxmax)
      dy <- max(0, rymin - ymax[i], ymin[i] - rymax)
      d <- sqrt(dx^2 + dy^2)

      if (is.null(best) || d < best$distance) {
        near_x <- min(max(box_cx, xmin[i]), xmax[i])
        near_y <- min(max(box_cy, ymin[i]), ymax[i])
        best <- list(distance = d, x = near_x / sx, y = near_y / sy)
      }
    }

    return(best)
  }

  if (identical(geom_type, "area")) {
    # For ambiguity/collision purposes against OTHER labels, treat the
    # area's upper (ymax) boundary as its relevant edge -- the same
    # simplification t61_place_label_area()'s outside-placement fallback
    # uses when a label can't fit inside the band anywhere.
    return(t61_box_distance_to_series(box, mask, list(x = series$x, y = series$ymax), "line", units))
  }

  if (identical(geom_type, "pointbar")) {
    # Each x has its own independent vertical segment (the error bar, from
    # ymin to ymax) -- unlike "line", these aren't connected to their
    # neighbours, so each is checked on its own rather than as segments
    # between consecutive points. Same exact corner/endpoint reasoning as
    # "line" otherwise: both shapes convex, so the true minimum is always
    # at a rectangle corner or a segment endpoint.
    x    <- series$x * sx
    ymin <- series$ymin * sy
    ymax <- series$ymax * sy

    corners <- expand.grid(x = c(rxmin, rxmax), y = c(rymin, rymax))
    best <- NULL

    update_best <- function(d, xx, yy) {
      if (is.null(best) || d < best$distance) best <<- list(distance = d, x = xx, y = yy)
    }

    for (i in seq_along(x)) {
      update_best(t61_point_rect_distance(x[i], ymin[i], rxmin, rxmax, rymin, rymax), series$x[i], series$ymin[i])
      update_best(t61_point_rect_distance(x[i], ymax[i], rxmin, rxmax, rymin, rymax), series$x[i], series$ymax[i])

      for (k in seq_len(nrow(corners))) {
        seg <- t61_point_segment_distance(corners$x[k], corners$y[k], x[i], ymin[i], x[i], ymax[i])
        update_best(seg$distance, seg$x / sx, seg$y / sy)
      }
    }

    return(best)
  }

  stop("t61_box_distance_to_series: unsupported geom_type '", geom_type, "' (v1 scope: point, line, column, area, pointbar)")
}
