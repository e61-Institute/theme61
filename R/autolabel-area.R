# Label placement for geom_area()/geom_ribbon() series (issue #159).
#
# Areas need a different placement strategy from point/line/column: the
# preference is to sit the label fully INSIDE the coloured band, in a
# colour that contrasts with the fill (white or black, whichever reads
# better), rather than pointing at an edge like the other geoms. Only when
# the band is too narrow anywhere to fit the label does this fall back to
# the same edge-hugging placement used for lines -- against the area's top
# (ymax) boundary -- in the area's own fill colour, matching what the user
# would see if auto-positioning were switched off.

#' Remove all GeomArea/GeomRibbon layers from a plot, for the collision
#' mask used by t61_place_label_area()'s inside-the-band search: a label
#' is meant to sit inside an area's own fill, so the fill itself must not
#' register as blocking "ink" the way it would for every other geom --
#' unlike text/points/lines/bars, which should still block a label from
#' landing on top of them. Drops every area layer, not just the one being
#' labelled: another (possibly overlapping) area's fill shouldn't block
#' placement either, for the same reason.
#' @noRd
t61_strip_area_layers <- function(plot) {
  keep <- vapply(plot@layers, function(ly) {
    geom_class <- class(ly$geom)
    !("GeomArea" %in% geom_class || "GeomRibbon" %in% geom_class)
  }, logical(1))

  plot@layers <- plot@layers[keep]
  plot
}

#' Interpolate a series' ymin/ymax at arbitrary x, via piecewise-linear
#' interpolation between its (already sorted) data points -- the same
#' shape ggplot2 draws the area's edges as. NA outside the series' x range
#' (extrapolation isn't meaningful: the area doesn't exist there).
#' @noRd
t61_area_interpolate <- function(series, x) {
  list(
    ymin = stats::approx(series$x, series$ymin, xout = x)$y,
    ymax = stats::approx(series$x, series$ymax, xout = x)$y
  )
}

#' The tightest [ymin, ymax] band that holds across the whole x interval
#' [x_left, x_right] -- i.e. max(ymin) and min(ymax) over that interval.
#' Since ymin(x)/ymax(x) are piecewise-linear, their extrema over any
#' sub-interval are always attained at that sub-interval's own endpoints or
#' at one of the series' own breakpoints strictly inside it (a linear
#' segment is monotonic, so its extreme values over a sub-range are always
#' at the sub-range's ends) -- sampling exactly those points is therefore
#' exact, not an approximation.
#'
#' Returns NULL if the interval isn't fully covered by the series' x range.
#' @noRd
t61_area_band_extent <- function(series, x_left, x_right) {
  x_range <- range(series$x)
  if (x_left < x_range[1] || x_right > x_range[2]) return(NULL)

  breaks <- series$x[series$x > x_left & series$x < x_right]
  sample_x <- c(x_left, breaks, x_right)

  band <- t61_area_interpolate(series, sample_x)
  list(ymin = max(band$ymin), ymax = min(band$ymax))
}

#' Pick white or black for text over a fill colour, whichever contrasts
#' better -- relative luminance (ITU-R BT.601 weights, the standard
#' quick heuristic for text-on-background contrast) below the midpoint
#' reads as a "dark" fill (use white text), at or above reads as "light"
#' (use black text).
#' @noRd
t61_contrast_colour <- function(fill) {
  rgb <- tryCatch(grDevices::col2rgb(fill), error = function(e) NULL)
  if (is.null(rgb)) return("black")

  luminance <- 0.299 * rgb["red", 1] + 0.587 * rgb["green", 1] + 0.114 * rgb["blue", 1]
  if (luminance < 128) "white" else "black"
}

#' Blend a (possibly semi-transparent) fill colour against a solid
#' background, e.g. for deciding text contrast against an alpha < 1 area
#' fill as it actually renders, not its nominal (opaque) colour -- a dark
#' fill at alpha = 0.6 reads much closer to the white panel background
#' than the raw fill colour would suggest.
#' @noRd
t61_blend_with_background <- function(colour, alpha, background = "white") {
  if (is.na(alpha) || alpha >= 1) return(colour)

  fg <- grDevices::col2rgb(colour)[, 1]
  bg <- grDevices::col2rgb(background)[, 1]
  blended <- alpha * fg + (1 - alpha) * bg

  grDevices::rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}

#' Search for a spot fully inside an area series' band for a label,
#' preferring the most comfortable (highest-slack) fit. Unlike
#' t61_place_label(), there's no "own-series buffer" here -- the label is
#' meant to sit inside the fill, not clear of it -- so this doesn't reuse
#' t61_place_label()'s candidate-scoring machinery, just a simpler
#' geometric search plus the usual hard collision/off-panel checks and
#' edge/gridline soft penalties for the final tiebreak.
#'
#' @param series list(x=, ymin=, ymax=) in draw order (as
#'   ggplot_build() would return them for a GeomArea/GeomRibbon layer).
#' @return list(x=, y=, box=) on success, or NULL if no x position has
#'   enough vertical room anywhere for the label to fit fully inside.
#' @noRd
t61_place_label_area <- function(series, mask, label_cm, hjust = 0, vjust = 0.5,
                                  n_x = 40, margin = 0.05, padding_cm = 0.05) {
  units <- t61_mask_units_cm(mask)
  width_data  <- (label_cm$width_cm  + 2 * padding_cm) / units$x_per_unit_cm
  height_data <- (label_cm$height_cm + 2 * padding_cm) / units$y_per_unit_cm

  x_range <- range(series$x)
  x_pad <- diff(x_range) * margin
  x_seq <- seq(x_range[1] + x_pad, x_range[2] - x_pad, length.out = n_x)

  rows <- vector("list", length(x_seq))

  for (i in seq_along(x_seq)) {
    x <- x_seq[i]
    x_left <- x - hjust * width_data
    x_right <- x_left + width_data

    band <- t61_area_band_extent(series, x_left, x_right)
    if (is.null(band)) next

    slack <- (band$ymax - band$ymin) - height_data
    if (slack < 0) next

    y <- (band$ymin + band$ymax) / 2

    box <- t61_text_box_px(x, y, label_cm, mask, hjust = hjust, vjust = vjust)
    if (!t61_box_in_bounds(box$row_range, box$col_range, mask$occupancy)) next
    if (t61_test_collision(mask$occupancy, box$row_range, box$col_range)) next

    edge_penalty_cm <- t61_edge_penalty_cm(box, mask, label_cm)
    gridline_penalty_cm <- t61_gridline_penalty_cm(box, mask, label_cm)

    rows[[i]] <- data.frame(x = x, y = y, slack = slack,
                             edge_penalty_cm = edge_penalty_cm,
                             gridline_penalty_cm = gridline_penalty_cm)
  }

  candidates <- do.call(rbind, rows)
  if (is.null(candidates) || nrow(candidates) == 0) return(NULL)

  # Most comfortable fit first (largest slack -- sits in the thickest part
  # of the band); edge/gridline penalty only breaks ties within that.
  ord <- order(-candidates$slack, candidates$edge_penalty_cm + candidates$gridline_penalty_cm)
  best <- candidates[ord[1], ]

  box <- t61_text_box_px(best$x, best$y, label_cm, mask, hjust = hjust, vjust = vjust)
  list(x = best$x, y = best$y, box = box)
}
