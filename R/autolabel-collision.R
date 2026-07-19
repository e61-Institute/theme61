# Collision detection: does a candidate label's bounding box overlap
# existing ink (data geoms or already-placed labels) in the occupancy mask?

#' Measure a text label's rendered size in cm, independent of where it will
#' be placed. Opens a throwaway svglite device to get accurate font
#' metrics (grid text-measurement functions need a live device), sized to
#' match the real chart so cm <-> pixel conversion stays consistent.
#'
#' v1 scope: axis-aligned labels only (angle = 0). Rotated text has a
#' rotated bounding box which the caller would need to handle separately;
#' callers should skip auto-placement for angled labels for now.
#'
#' @noRd
t61_measure_label_cm <- function(label, size_mm = 3.5, width_cm = 16, height_cm = 12) {
  svg_file <- tempfile(fileext = ".svg")
  on.exit(unlink(svg_file), add = TRUE)

  svglite::svglite(svg_file, width = width_cm / 2.54, height = height_cm / 2.54)
  on.exit(grDevices::dev.off(), add = TRUE)

  # ggplot2's geom_text/geom_label "size" aesthetic is in mm; convert to pt
  # the same way ggplot2 does internally (.pt = 72.27 / 25.4).
  fontsize_pt <- size_mm * (72.27 / 25.4)

  grob <- grid::textGrob(label, gp = grid::gpar(fontsize = fontsize_pt))

  list(
    width_cm  = grid::convertWidth(grid::grobWidth(grob), "cm", valueOnly = TRUE),
    height_cm = grid::convertHeight(grid::grobHeight(grob), "cm", valueOnly = TRUE)
  )
}

#' Convert a measured label size + anchor position + justification into a
#' pixel row/col bounding box within a mask (see t61_render_mask()).
#'
#' hjust follows ggplot2 convention: 0 = anchor is the left edge of the text,
#' 0.5 = centred, 1 = right edge. vjust similarly for the vertical edge.
#' @noRd
t61_text_box_px <- function(x, y, label_cm, mask, hjust = 0, vjust = 0.5, padding_cm = 0.05) {
  anchor <- t61_data_to_px(x, y, mask)

  width_px  <- (label_cm$width_cm + 2 * padding_cm) * mask$px_per_cm_x
  height_px <- (label_cm$height_cm + 2 * padding_cm) * mask$px_per_cm_y

  col_left <- anchor$col - hjust * width_px
  row_top  <- anchor$row - (1 - vjust) * height_px

  list(
    row_range = c(row_top, row_top + height_px),
    col_range = c(col_left, col_left + width_px)
  )
}

#' Whether a pixel bounding box stays entirely within the panel -- a
#' stricter check than t61_test_collision(), which clips a partially
#' off-raster box down to whatever's visible rather than rejecting it.
#' Callers that care whether a label would actually render fully on-panel
#' (as opposed to just "not overlapping ink in the visible portion") should
#' use this too.
#'
#' Checked against the panel's own box (mask$panel), not the raster's full
#' dimensions: the raster also covers the axis-title/tick-label gutter
#' around the panel (t61_strip_chrome() blanks that chrome with
#' colour = NA rather than element_blank(), specifically so the space
#' stays reserved -- see its own docs), and that gutter reads as ink-free
#' in the mask, same as genuinely empty panel space. A box can be fully
#' inside the raster while still hanging off the actual plotting area into
#' that gutter -- checking raster bounds alone would silently accept a
#' box that will render overlapping the real axis text/title once the
#' chrome comes back for the actual chart.
#' @noRd
t61_box_in_bounds <- function(row_range, col_range, mask) {
  panel <- mask$panel
  min(row_range) >= panel$top_px && max(row_range) <= panel$top_px + panel$height_px &&
    min(col_range) >= panel$left_px && max(col_range) <= panel$left_px + panel$width_px
}

#' Does a pixel bounding box overlap any ink in the occupancy mask?
#' Boxes (partially or fully) outside the raster are clipped to it; a box
#' entirely outside the raster is treated as a collision (can't be placed).
#' Use t61_box_in_bounds() first if a fully on-panel box is required.
#' @noRd
t61_test_collision <- function(occupancy, row_range, col_range) {
  nr <- nrow(occupancy); nc <- ncol(occupancy)

  r0 <- max(1, floor(min(row_range)))
  r1 <- min(nr, ceiling(max(row_range)))
  c0 <- max(1, floor(min(col_range)))
  c1 <- min(nc, ceiling(max(col_range)))

  if (r0 > r1 || c0 > c1) return(TRUE) # entirely off the raster

  any(occupancy[r0:r1, c0:c1])
}

#' Stamp a placed label's bounding box into the occupancy mask, so
#' subsequent labels are placed around it too.
#' @noRd
t61_stamp_box <- function(occupancy, row_range, col_range) {
  nr <- nrow(occupancy); nc <- ncol(occupancy)

  r0 <- max(1, floor(min(row_range)))
  r1 <- min(nr, ceiling(max(row_range)))
  c0 <- max(1, floor(min(col_range)))
  c1 <- min(nc, ceiling(max(col_range)))

  if (r0 <= r1 && c0 <= c1) occupancy[r0:r1, c0:c1] <- TRUE

  occupancy
}
