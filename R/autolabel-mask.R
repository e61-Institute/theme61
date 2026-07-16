# Low-res occupancy mask + data-space <-> pixel-space coordinate mapping.
#
# This is the foundation the rest of the autolabel-* modules build on: given
# a plot, produce (a) a boolean matrix of where "ink" (geoms, already-placed
# labels) already is, at low resolution, and (b) a mapping from data
# coordinates to pixel coordinates in that matrix, so candidate label
# positions and distance-to-series calculations can be evaluated against it.
#
# Unlike base-graphics approaches (e.g. arphit), ggplot2/grid have no
# equivalent of par("usr")/par("pin") to read this off directly, so the
# panel's pixel bounding box is derived from the plot's gtable layout.

#' Work out which gtable width/height units are elastic ("null") vs fixed.
#' Fixed units (points/cm/grobwidth/grobheight/sum) are resolvable without a
#' device or layout context; "null" units only get an absolute size once
#' rendered into a viewport of known size. Since save_e61()/the mask render
#' both use a known, fixed physical page size, the "null" cell(s) can be
#' assigned the remainder directly instead of needing to actually draw
#' anything.
#' @noRd
t61_panel_box_cm <- function(gt, width_cm, height_cm) {

  width_types  <- grid::unitType(gt$widths)
  height_types <- grid::unitType(gt$heights)

  is_fixed_w <- width_types != "null"
  is_fixed_h <- height_types != "null"

  col_widths_cm  <- rep(0, length(gt$widths))
  row_heights_cm <- rep(0, length(gt$heights))

  col_widths_cm[is_fixed_w] <- vapply(
    gt$widths[is_fixed_w], grid::convertWidth, numeric(1), unitTo = "cm", valueOnly = TRUE
  )
  row_heights_cm[is_fixed_h] <- vapply(
    gt$heights[is_fixed_h], grid::convertHeight, numeric(1), unitTo = "cm", valueOnly = TRUE
  )

  null_col_weight <- ifelse(is_fixed_w, 0, as.numeric(gt$widths))
  null_row_weight <- ifelse(is_fixed_h, 0, as.numeric(gt$heights))

  remaining_width_cm  <- width_cm  - sum(col_widths_cm)
  remaining_height_cm <- height_cm - sum(row_heights_cm)

  # v1 scope: single-panel (unfacetted) plots have exactly one null column
  # and one null row (the panel itself), so this divides the remainder
  # 100% to that cell. Multiple null cells (e.g. facets) would share it
  # proportional to their null weight.
  col_widths_cm[!is_fixed_w] <- remaining_width_cm *
    null_col_weight[!is_fixed_w] / sum(null_col_weight[!is_fixed_w])
  row_heights_cm[!is_fixed_h] <- remaining_height_cm *
    null_row_weight[!is_fixed_h] / sum(null_row_weight[!is_fixed_h])

  panel_col <- gt$layout$l[gt$layout$name == "panel"]
  panel_row <- gt$layout$t[gt$layout$name == "panel"]

  if (length(panel_col) != 1 || length(panel_row) != 1) {
    return(NULL) # faceted / unexpected layout: not v1 scope, caller should bail out
  }

  list(
    left_cm   = sum(col_widths_cm[seq_len(panel_col - 1)]),
    top_cm    = sum(row_heights_cm[seq_len(panel_row - 1)]),
    width_cm  = col_widths_cm[panel_col],
    height_cm = row_heights_cm[panel_row]
  )
}

#' Strip decorative chrome from a plot for mask rendering, WITHOUT changing
#' its gtable layout. Uses colour = NA (invisible, space still reserved),
#' never element_blank() (which would collapse e.g. the axis-text column
#' and shift the panel's offset relative to the real chart).
#' @noRd
t61_strip_chrome <- function(plot) {
  plot + ggplot2::theme(
    panel.grid       = ggplot2::element_line(colour = NA),
    panel.border     = ggplot2::element_rect(colour = NA, fill = NA),
    axis.line        = ggplot2::element_line(colour = NA),
    axis.ticks       = ggplot2::element_line(colour = NA),
    axis.text        = ggplot2::element_text(colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    legend.position  = "none"
  )
}

#' Render a low-res occupancy mask for a plot, plus everything needed to map
#' data coordinates into it.
#'
#' @param plot A ggplot object, already fully built (scales resolved etc.)
#' @param width_cm,height_cm Physical size the real chart will be saved at.
#' @param px_width Pixel width of the low-res mask raster. Height is derived
#'   from the aspect ratio of width_cm/height_cm.
#' @return A list with: occupancy (logical matrix, TRUE = ink, [row, col],
#'   row 1 = top), panel (list of left/top/width/height in px),
#'   px_per_cm_x, px_per_cm_y, x_range, y_range (panel data ranges). Returns
#'   NULL if the plot's layout isn't v1 scope (e.g. facets).
#' @noRd
t61_render_mask <- function(plot, width_cm, height_cm, px_width = 400L) {

  gt <- ggplot2::ggplotGrob(t61_strip_chrome(plot))
  panel_cm <- t61_panel_box_cm(gt, width_cm, height_cm)
  if (is.null(panel_cm)) return(NULL)

  px_height <- round(px_width * height_cm / width_cm)

  svg_file <- tempfile(fileext = ".svg")
  on.exit(unlink(svg_file), add = TRUE)
  svglite::svglite(svg_file, width = width_cm / 2.54, height = height_cm / 2.54, bg = "white")
  print(t61_strip_chrome(plot))
  grDevices::dev.off()

  png_file <- tempfile(fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)
  rsvg::rsvg_png(svg_file, png_file, width = px_width, height = px_height)

  img <- magick::image_read(png_file)
  raster <- as.raster(img)

  rgb_vals <- col2rgb(raster)
  is_ink <- colSums(rgb_vals) < (255 * 3 - 10) # small tolerance for anti-aliasing

  # IMPORTANT: raster objects store pixels row-major (see
  # grDevices:::as.matrix.raster), not R's default column-major, so the
  # reconstruction must use byrow = TRUE or every pixel silently misaligns.
  occupancy <- matrix(is_ink, nrow = nrow(raster), byrow = TRUE)
  storage.mode(occupancy) <- "logical"

  px_per_cm_x <- ncol(raster) / width_cm
  px_per_cm_y <- nrow(raster) / height_cm

  built <- ggplot2::ggplot_build(plot)
  pp <- built$layout$panel_params
  if (length(pp) != 1) return(NULL) # faceted: not v1 scope

  list(
    occupancy   = occupancy,
    panel       = list(
      left_px   = panel_cm$left_cm * px_per_cm_x,
      top_px    = panel_cm$top_cm * px_per_cm_y,
      width_px  = panel_cm$width_cm * px_per_cm_x,
      height_px = panel_cm$height_cm * px_per_cm_y
    ),
    px_per_cm_x = px_per_cm_x,
    px_per_cm_y = px_per_cm_y,
    x_range     = pp[[1]]$x.range,
    y_range     = pp[[1]]$y.range
  )
}

#' Map a data-space (x, y) coordinate to (row, col) pixel space in a mask
#' produced by t61_render_mask().
#' @noRd
t61_data_to_px <- function(x, y, mask) {
  panel <- mask$panel
  col <- panel$left_px +
    (x - mask$x_range[1]) / diff(mask$x_range) * panel$width_px
  # row 1 is the top of the image; y increases upward in data space
  row <- panel$top_px +
    (mask$y_range[2] - y) / diff(mask$y_range) * panel$height_px
  list(row = row, col = col)
}
