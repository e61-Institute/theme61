# Low-res occupancy mask + data-space <-> pixel-space coordinate mapping.
#
# This is the foundation the rest of the autolabel-* modules build on: given
# a plot, produce (a) a boolean matrix of where "ink" (geoms, already-placed
# labels) already is, at low resolution, and (b) a mapping from data
# coordinates to pixel coordinates in that matrix, so candidate label
# positions and distance-to-series calculations can be evaluated against it.
#
# ggplot2/grid have no equivalent of par("usr")/par("pin") to read this off
# directly, so the panel's pixel bounding box is derived from the plot's
# gtable layout instead.

#' Strip the e61_ggplot class before print()-ing a throwaway render (an
#' occupancy raster, a panel-box marker): print.e61_ggplot() has side
#' effects (a Viewer preview, console output) meant for a plot the user is
#' actually looking at, not an internal measurement render -- dispatching
#' to it here would fire those side effects once per panel, per mask
#' render, stomping on the real preview/save this call is itself part of.
#'
#' ggplot_build.e61_ggplot() (see ggplot_build-method.R) applies its own
#' mutations -- default scales, facet spacing, discrete y-text alignment --
#' before building, and every other read of this plot (e.g. t61_render_mask()'s
#' own ggplot_build() call for panel_params) goes through that dispatch.
#' Applying the same mutations here before stripping the class keeps this
#' raster's coordinate system consistent with those reads, rather than
#' silently reverting to plain ggplot2 defaults for this render alone.
#' @noRd
t61_drop_e61_class <- function(plot) {
  if (inherits(plot, "e61_ggplot")) {
    plot <- maybe_add_default_scales(plot)
    plot <- maybe_adjust_facet_spacing(plot)
    plot <- maybe_leftalign_discrete_y_text(plot)
    class(plot) <- setdiff(class(plot), "e61_ggplot")
  }
  plot
}

#' Classify gtable width/height units as elastic ("null") vs fixed, so the
#' null cell(s) -- normally only resolved by actually drawing into a
#' viewport -- can instead be assigned the remainder directly, since
#' save_e61()/the mask render both use a known, fixed physical page size.
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
#'
#' Blanks the .major/.minor/.x/.y variants explicitly, not just their generic
#' parents (panel.grid, axis.line, axis.ticks): ggplot2's theme merging keeps
#' an already-explicit specific element (as theme_e61() sets) over a more
#' generic one, so blanking only the parent would leave it as mask "ink".
#' Also blanks axis.title (rotated axis title text is real ink too).
#' @noRd
t61_strip_chrome <- function(plot) {
  plot + ggplot2::theme(
    panel.grid          = ggplot2::element_line(colour = NA),
    panel.grid.major    = ggplot2::element_line(colour = NA),
    panel.grid.minor    = ggplot2::element_line(colour = NA),
    panel.grid.major.x  = ggplot2::element_line(colour = NA),
    panel.grid.major.y  = ggplot2::element_line(colour = NA),
    panel.grid.minor.x  = ggplot2::element_line(colour = NA),
    panel.grid.minor.y  = ggplot2::element_line(colour = NA),
    panel.border        = ggplot2::element_rect(colour = NA, fill = NA),
    axis.line           = ggplot2::element_line(colour = NA),
    axis.line.x         = ggplot2::element_line(colour = NA),
    axis.line.y         = ggplot2::element_line(colour = NA),
    axis.ticks          = ggplot2::element_line(colour = NA),
    axis.ticks.x        = ggplot2::element_line(colour = NA),
    axis.ticks.y        = ggplot2::element_line(colour = NA),
    axis.text           = ggplot2::element_text(colour = NA),
    axis.title          = ggplot2::element_text(colour = NA),
    panel.background    = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background     = ggplot2::element_rect(fill = "white", colour = NA),
    legend.position      = "none"
  )
}

#' Render a low-res occupancy mask for a plot, plus everything needed to map
#' data coordinates into it.
#'
#' @param plot A ggplot object, already fully built (scales resolved etc.)
#' @param width_cm,height_cm Physical size the real chart will be saved at.
#' @param px_width Pixel width of the low-res mask raster. Height is derived
#'   from the aspect ratio of width_cm/height_cm.
#' @return A list with: occupancy (logical matrix, TRUE = ink, \[row, col\],
#'   row 1 = top), panel (list of left/top/width/height in px),
#'   px_per_cm_x, px_per_cm_y, x_range, y_range (panel data ranges). Returns
#'   NULL if the plot's layout isn't v1 scope (e.g. facets).
#' @noRd
t61_render_mask <- function(plot, width_cm, height_cm, px_width = 400L) {

  built <- ggplot2::ggplot_build(plot)

  # panel_params (used below for x_range/y_range/breaks) already reflects
  # coord_flip() -- it describes the screen's rendered axes, not the raw
  # aesthetics -- so the mask itself needs no special handling here. The
  # caller (t61_autolabel_plot()) still needs to know a flip happened,
  # since series data and the label's own x/y come from ggplot_build()$data
  # in pre-flip (data-aesthetic) space and have to be remapped onto these
  # screen axes.
  flipped <- inherits(built$layout$coord, "CoordFlip")

  gt <- ggplot2::ggplotGrob(t61_strip_chrome(plot))
  # Still used for its facet bail-out (exactly one panel cell, structurally
  # checked via the gtable layout) -- but NOT for the cm box it would
  # otherwise compute; see t61_render_panel_box_px() for why.
  if (is.null(t61_panel_box_cm(gt, width_cm, height_cm))) return(NULL)

  px_height <- round(px_width * height_cm / width_cm)

  svg_file <- tempfile(fileext = ".svg")
  on.exit(unlink(svg_file), add = TRUE)
  svglite::svglite(svg_file, width = width_cm / 2.54, height = height_cm / 2.54, bg = "white")

  print(t61_strip_chrome(t61_drop_e61_class(plot)))
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

  panel_px <- t61_render_panel_box_px(plot, width_cm, height_cm, px_width, px_height)
  if (is.null(panel_px)) return(NULL)

  pp <- built$layout$panel_params
  if (length(pp) != 1) return(NULL) # faceted: not v1 scope

  # panel_params$y.range is the visible viewport -- wider than the scale's
  # own hard limits whenever coord_cartesian(ylim = ...) zooms out beyond
  # them (e.g. to show a narrow band with room to breathe). When explicit
  # numeric limits were supplied, scale_y_continuous_e61() installs a
  # train()-time check (see axes.R) that hard-errors if a value outside
  # those limits is later trained into the scale -- which a label placed
  # anywhere in the wider coord_cartesian() viewport can trigger once it's
  # actually added to the plot. Only intersect in that case: a scale with
  # no explicit limits has no such check, and get_limits() would otherwise
  # just be its auto-trained (often data-degenerate) range, which
  # coord_cartesian() is legitimately free to show more of.
  y_scale <- built$layout$panel_scales_y[[1]]
  y_range <- pp[[1]]$y.range
  if (!is.null(y_scale) && !is.null(y_scale$limits) && is.numeric(y_scale$limits)) {
    y_limits <- tryCatch(y_scale$get_limits(), error = function(e) NULL)
    if (!is.null(y_limits) && all(is.finite(y_limits))) {
      y_range <- c(max(y_range[1], min(y_limits)), min(y_range[2], max(y_limits)))
    }
  }

  x_breaks <- pp[[1]]$x$breaks
  y_breaks <- pp[[1]]$y$breaks
  x_breaks <- x_breaks[is.finite(x_breaks) & x_breaks >= pp[[1]]$x.range[1] & x_breaks <= pp[[1]]$x.range[2]]
  y_breaks <- y_breaks[is.finite(y_breaks) & y_breaks >= y_range[1] & y_breaks <= y_range[2]]

  list(
    occupancy   = occupancy,
    panel       = panel_px,
    px_per_cm_x = px_per_cm_x,
    px_per_cm_y = px_per_cm_y,
    x_range     = pp[[1]]$x.range,
    y_range     = y_range,
    x_breaks    = x_breaks,
    y_breaks    = y_breaks,
    flipped     = flipped
  )
}

#' The panel's pixel bounding box, measured from an actual render rather
#' than predicted from the gtable's declared row/column units (see
#' t61_panel_box_cm()). ggplot2 sometimes sizes a row/column with a
#' compound unit -- e.g. axis-label height expressed as
#' sum(fixed, ..., 1null, ...), when the axis needs to dynamically
#' accommodate label sizing -- and a "null" embedded like that only
#' resolves correctly once actually rendered into a real device.
#' t61_panel_box_cm()'s offline resolution treats anything not literally
#' typed "null" as already-fixed, silently collapsing an embedded null (and
#' the real space it represents) to zero -- underestimating that row/column
#' and, in turn, mis-sizing the panel itself. Measuring the render directly
#' avoids that failure mode entirely, since it never has to predict how a
#' compound unit resolves.
#'
#' Renders a throwaway version of the plot with every geom layer removed
#' and the panel background set to a colour that won't otherwise appear,
#' then locates that colour's bounding box in the raster -- same
#' svglite -> rsvg pipeline as the occupancy raster itself, so this is only
#' ever wrong if that raster is too.
#'
#' @return list(left_px=, top_px=, width_px=, height_px=), or NULL if the
#'   marker colour doesn't appear at all (shouldn't happen for a genuine
#'   single-panel plot).
#' @noRd
t61_render_panel_box_px <- function(plot, width_cm, height_cm, px_width, px_height) {
  marker_colour <- "#FF00FF"

  marker <- t61_strip_chrome(plot)
  marker@layers <- list()
  marker <- marker + ggplot2::theme(panel.background = ggplot2::element_rect(fill = marker_colour, colour = NA))
  marker <- t61_drop_e61_class(marker)

  svg_file <- tempfile(fileext = ".svg")
  on.exit(unlink(svg_file), add = TRUE)
  svglite::svglite(svg_file, width = width_cm / 2.54, height = height_cm / 2.54, bg = "white")
  print(marker)
  grDevices::dev.off()

  png_file <- tempfile(fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)
  rsvg::rsvg_png(svg_file, png_file, width = px_width, height = px_height)

  img <- magick::image_read(png_file)
  raster <- as.raster(img)

  rgb_vals <- col2rgb(raster)
  is_marker <- colSums(abs(rgb_vals - as.vector(col2rgb(marker_colour)))) < 30
  occ <- matrix(is_marker, nrow = nrow(raster), byrow = TRUE)

  rows_with <- which(rowSums(occ) > 0)
  cols_with <- which(colSums(occ) > 0)
  if (length(rows_with) == 0 || length(cols_with) == 0) return(NULL)

  list(
    left_px   = min(cols_with),
    top_px    = min(rows_with),
    width_px  = max(cols_with) - min(cols_with),
    height_px = max(rows_with) - min(rows_with)
  )
}

#' Swap x/y between pre-flip data-aesthetic space and the mask's screen
#' space under coord_flip() -- self-inverse, so the same call converts
#' either direction. Point/line positions are a plain (x, y) swap; a
#' "column" bar's rectangle swaps its whole axis pairing, (xmin,xmax) with
#' (ymin,ymax). Area/pointbar aren't handled -- see their callers.
#' @noRd
t61_flip_xy <- function(x, y) list(x = y, y = x)

#' @noRd
t61_flip_rect <- function(xmin, xmax, ymin, ymax) {
  list(xmin = ymin, xmax = ymax, ymin = xmin, ymax = xmax)
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

#' Soft penalty for a candidate box sitting close to the panel's edge --
#' text hugging the axis reads as cramped even when it's not actually
#' clipped (the hard floor is t61_box_in_bounds(), which this does not
#' replace). Zero once the box clears the edge by a full label-height;
#' grows linearly as that clearance shrinks toward zero. Never excludes a
#' candidate outright -- it only feeds into the tiebreak score alongside
#' the buffer/gridline penalties, so an edge position is still chosen when
#' nothing better is on offer.
#' @noRd
t61_edge_penalty_cm <- function(box, mask, label_cm) {
  panel <- mask$panel

  left_clear_cm   <- (min(box$col_range) - panel$left_px) / mask$px_per_cm_x
  right_clear_cm  <- ((panel$left_px + panel$width_px) - max(box$col_range)) / mask$px_per_cm_x
  top_clear_cm    <- (min(box$row_range) - panel$top_px) / mask$px_per_cm_y
  bottom_clear_cm <- ((panel$top_px + panel$height_px) - max(box$row_range)) / mask$px_per_cm_y

  min_clear_cm <- min(left_clear_cm, right_clear_cm, top_clear_cm, bottom_clear_cm)

  max(0, label_cm$height_cm - min_clear_cm)
}

#' Whether a candidate box's row_range crosses a y-axis gridline break --
#' the hard-avoidance check behind t61_place_label()'s/
#' t61_place_label_fallback()'s avoid_gridline pass (see their docs): a
#' label should only ever touch a y gridline when every candidate does,
#' not merely lose a tiebreak for it. Scoped to y only (not x) because
#' theme_e61()'s default style shows y gridlines but blanks x ones -- x
#' gridlines only exist under non-default theme variants, where the softer
#' t61_gridline_penalty_cm() tiebreak (which still covers both axes) is
#' judged sufficient.
#' @noRd
t61_touches_y_gridline <- function(box, mask) {
  for (brk in mask$y_breaks) {
    row <- t61_data_to_px(mean(mask$x_range), brk, mask)$row
    if (row >= min(box$row_range) && row <= max(box$row_range)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Soft penalty for a candidate box straddling a gridline break (x or y),
#' one label-height's worth of penalty per axis it crosses. Like
#' t61_edge_penalty_cm(), this only ever feeds the tiebreak score -- a
#' candidate that overlaps a gridline is still chosen when it's the only
#' option, it just loses to an otherwise-equal candidate that doesn't.
#' The y component reuses t61_touches_y_gridline(), the same hard check
#' t61_place_label() uses to avoid y gridlines outright; this penalty stays
#' relevant on top of that for x gridlines, and for tiebreaking among
#' candidates once avoidance has already been exhausted.
#' @noRd
t61_gridline_penalty_cm <- function(box, mask, label_cm) {
  x_hit <- FALSE
  for (brk in mask$x_breaks) {
    col <- t61_data_to_px(brk, mean(mask$y_range), mask)$col
    if (col >= min(box$col_range) && col <= max(box$col_range)) {
      x_hit <- TRUE
      break
    }
  }

  y_hit <- t61_touches_y_gridline(box, mask)

  (as.numeric(x_hit) + as.numeric(y_hit)) * 0.5 * label_cm$height_cm
}
