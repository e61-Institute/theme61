# Orchestration: ties the mask/collision/distance/line-of-sight/selection
# modules together to place one or more labels on a plot.
#
# v1 scope: single-panel (unfacetted) plots, "point"/"line"/"column"/"area"/
# "pointbar" geoms, 2-3 series -- kept deliberately narrow as a starting
# point rather than solving the general case up front.

#' Evaluate every candidate position in the grid against the mask. Returns
#' list(x, y, box) on success, or NULL if nothing is placeable.
#'
#' Candidates too close to their own series (`min_buffer_cm`) or touching a
#' y-axis gridline are excluded on a first pass -- the tiebreak in
#' t61_select_best_candidate() only chooses *among* whatever candidates
#' survive, so a constraint has to be enforced here to have any effect.
#' Relaxed one constraint at a time (buffer, then gridline avoidance) only
#' if a stricter pass finds nothing, so a label degrades gracefully rather
#' than disappearing or touching a gridline when it didn't have to.
#'
#' @param series The label's own series: list(x=, y=) for "point"/"line",
#'   list(xmin=, xmax=, ymin=, ymax=) for "column", or list(x=, y=, ymin=,
#'   ymax=) for "pointbar".
#' @param other_series Other series (same shape, plus geom_type=) to score
#'   "ambiguity" against.
#' @noRd
t61_place_label <- function(series, geom_type, other_series, mask, label_cm,
                            hjust = 0, vjust = 0.5, n_x = 24, n_y = 32, margin = 0.08,
                            min_buffer_cm = NULL) {

  if (is.null(min_buffer_cm)) {
    # Points need a much higher multiplier than lines to clear a scattered
    # cluster rather than just the one nearest point.
    mult <- if (identical(geom_type, "point")) 3.4 else 1.13
    min_buffer_cm <- mult * label_cm$height_cm
  }

  units <- t61_mask_units_cm(mask)
  grid <- t61_candidate_grid(mask$x_range, mask$y_range, n_x = n_x, n_y = n_y, margin = margin)

  build_candidates <- function(enforce_min_buffer, avoid_gridline) {
    rows <- vector("list", nrow(grid))

    for (i in seq_len(nrow(grid))) {
      x <- grid$x[i]; y <- grid$y[i]

      box <- t61_text_box_px(x, y, label_cm, mask, hjust = hjust, vjust = vjust)
      # Rejected outright here (unlike t61_test_collision(), which just
      # clips a partially off-raster box to what's visible) -- ggplot2
      # clips drawn text to the panel, so a partially off-panel label would
      # render visibly truncated.
      if (!t61_box_in_bounds(box$row_range, box$col_range, mask)) next
      if (t61_test_collision(mask$occupancy, box$row_range, box$col_range)) next
      if (avoid_gridline && t61_touches_y_gridline(box, mask)) next

      # Measured from the box's footprint, not the (x, y) anchor -- with
      # hjust = 0 the box extends a full label-width from the anchor, so an
      # anchor clearing the buffer can still leave the box sitting right on
      # top of a point or line.
      own <- t61_box_distance_to_series(box, mask, series, geom_type, units)
      if (enforce_min_buffer && own$distance < min_buffer_cm) next

      next_closest <- Inf
      for (s in other_series) {
        d <- t61_box_distance_to_series(box, mask, s, s$geom_type, units)
        if (d$distance < next_closest) next_closest <- d$distance
      }

      anchor <- t61_data_to_px(x, y, mask)
      nearest_px <- t61_data_to_px(own$x, own$y, mask)
      los <- t61_line_of_sight(mask$occupancy, anchor$row, anchor$col, nearest_px$row, nearest_px$col)

      edge_penalty_cm <- t61_edge_penalty_cm(box, mask, label_cm)
      gridline_penalty_cm <- t61_gridline_penalty_cm(box, mask, label_cm)

      rows[[i]] <- data.frame(x = x, y = y, distance_cm = own$distance,
                              next_closest_cm = next_closest, los = los,
                              edge_penalty_cm = edge_penalty_cm,
                              gridline_penalty_cm = gridline_penalty_cm)
    }

    do.call(rbind, rows)
  }

  # Gridline avoidance is exhausted across both buffer levels before a
  # touch is ever allowed: buffered+clear, tight+clear, buffered+touching,
  # tight+touching.
  candidates <- build_candidates(enforce_min_buffer = TRUE, avoid_gridline = TRUE)
  if (is.null(candidates) || nrow(candidates) == 0) {
    candidates <- build_candidates(enforce_min_buffer = FALSE, avoid_gridline = TRUE)
  }
  if (is.null(candidates) || nrow(candidates) == 0) {
    candidates <- build_candidates(enforce_min_buffer = TRUE, avoid_gridline = FALSE)
  }
  if (is.null(candidates) || nrow(candidates) == 0) {
    candidates <- build_candidates(enforce_min_buffer = FALSE, avoid_gridline = FALSE)
  }
  if (is.null(candidates) || nrow(candidates) == 0) return(NULL)

  best <- t61_select_best_candidate(candidates, label_cm$height_cm, geom_type = geom_type)
  box <- t61_text_box_px(best$x, best$y, label_cm, mask, hjust = hjust, vjust = vjust)

  list(x = best$x, y = best$y, box = box)
}

#' Fallback when no grid candidate is collision-free: spiral outward from
#' the panel centre, looking for *any* clear spot, ignoring
#' distance/line-of-sight.
#'
#' A box must fit fully in the raster before it's accepted -- unlike
#' t61_test_collision() alone, which clips a partially off-raster box to
#' what's visible, so an edge-hugging box over empty margin space would
#' otherwise pass as "clear" and render truncated. x_steps/y_steps reach
#' the data range's own edges, so this comes up often here (unlike
#' t61_place_label()'s grid, which already keeps a margin off the edges).
#'
#' Mirrors t61_place_label()'s gridline-avoidance pass: a touching step is
#' skipped on a first sweep and only allowed on a second if nothing else
#' cleared.
#' @noRd
t61_place_label_fallback <- function(mask, label_cm, hjust = 0, vjust = 0.5, n_steps = 12) {
  x_mid <- mean(mask$x_range); y_mid <- mean(mask$y_range)

  x_up   <- seq(x_mid, mask$x_range[2], length.out = n_steps)
  x_down <- seq(x_mid, mask$x_range[1], length.out = n_steps)
  y_up   <- seq(y_mid, mask$y_range[2], length.out = n_steps)
  y_down <- seq(y_mid, mask$y_range[1], length.out = n_steps)

  x_steps <- c(rbind(x_up, x_down))
  y_steps <- c(rbind(y_up, y_down))

  search <- function(avoid_gridline) {
    for (x in x_steps) {
      for (y in y_steps) {
        box <- t61_text_box_px(x, y, label_cm, mask, hjust = hjust, vjust = vjust)
        if (!t61_box_in_bounds(box$row_range, box$col_range, mask)) next
        if (avoid_gridline && t61_touches_y_gridline(box, mask)) next
        if (!t61_test_collision(mask$occupancy, box$row_range, box$col_range)) {
          return(list(x = x, y = y, box = box))
        }
      }
    }
    NULL
  }

  result <- search(avoid_gridline = TRUE)
  if (is.null(result)) result <- search(avoid_gridline = FALSE)
  result
}

#' Cheap, render-free placement used when `fast = TRUE` (see
#' t61_autolabel_plot()): a fixed offset from the label's own matched
#' series -- near its last data point, nudged up and staggered by the
#' label's position in the loop to reduce (not guarantee) label-on-label
#' overlap. No mask, no collision search, no coord_flip handling needed --
#' it only ever touches the series' own data-space values, which are
#' already in the same aesthetic space plot_label() writes to regardless
#' of coord_flip().
#'
#' The y offset is clamped to the series' own actual [min, max] -- by the
#' time this runs, the y-axis scale's limits are usually already fixed
#' (see update_scales(), called earlier in save_single()'s pipeline), sized
#' to comfortably contain the real data but not by a guaranteed or
#' predictable margin. An unclamped offset could land outside those
#' limits and hard-error at render time instead of just rendering
#' slightly imprecisely -- clamping to the data's own extent is always
#' safe, since a scale's limits can never be narrower than the data they
#' were built from.
#'
#' The anchor also steps back from the series' actual last point (by
#' index, not distance, so it works for unevenly-spaced data too) when
#' `hjust` is low -- with the default hjust = 0 (left-justified text
#' extends rightward from the anchor), anchoring right at the last point
#' pushes the label past the panel's right edge and off the visible chart.
#' No step-back at hjust = 1 (right-justified text extends leftward, so
#' the last point is already a safe anchor there); linearly less step-back
#' as hjust rises toward 1.
#'
#' How far back is estimated from the label's own measured width (via
#' t61_measure_label_cm() -- cheap, ~2ms, a throwaway svglite device with
#' no rsvg/raster conversion, unlike the mask render this whole `fast`
#' path exists to avoid), converted to data-space x units against a rough
#' estimate of the panel's plotted width. That estimate is deliberately
#' conservative (80% of the chart's total width, and assuming the
#' series' own x-span roughly matches the panel's plotted x-range, true
#' whenever auto-scaling is on): erring toward stepping back further than
#' strictly necessary is harmless, stepping back too little is the actual
#' bug this exists to fix. Falls back to a fixed fraction if measurement
#' fails for any reason.
#'
#' @param own The label's own series, same shapes as t61_place_label().
#' @param index The label's 1-based position among all labels being placed
#'   this call, used only to stagger repeated placements apart.
#' @param hjust The label's horizontal justification (0 = left, 1 = right).
#' @param text,size_mm,width_cm,height_cm Passed through to
#'   t61_measure_label_cm() to estimate the label's own width.
#' @noRd
t61_place_label_fast <- function(own, geom_type, index = 1, hjust = 0,
                                  text = "", size_mm = 3.5, width_cm = 16, height_cm = 12) {
  stagger <- function(y, y_all) {
    yr <- diff(range(y_all, na.rm = TRUE))
    if (!is.finite(yr) || yr == 0) yr <- max(abs(y), 1)
    offset <- 0.06 * yr + (index %% 3) * 0.05 * yr
    min(max(y + offset, min(y_all, na.rm = TRUE)), max(y_all, na.rm = TRUE))
  }

  step_from_end <- function(x_all) {
    n <- length(x_all)
    if (n <= 1 || hjust >= 1) return(n) # hjust = 1: anchor at the end is already safe

    xr <- diff(range(x_all, na.rm = TRUE))
    if (!is.finite(xr) || xr == 0) return(n)

    label_cm <- tryCatch(
      t61_measure_label_cm(text, size_mm = size_mm, width_cm = width_cm, height_cm = height_cm),
      error = function(e) NULL
    )
    needed_x <- if (is.null(label_cm)) {
      0.15 * (1 - min(max(hjust, 0), 1)) * xr # measurement failed: crude fallback
    } else {
      approx_panel_width_cm <- 0.80 * width_cm
      (label_cm$width_cm / approx_panel_width_cm) * xr * (1 - min(max(hjust, 0), 1))
    }
    if (needed_x <= 0) return(n)

    target_x <- max(x_all, na.rm = TRUE) - needed_x
    idx <- which(x_all <= target_x)
    if (length(idx) == 0) return(1) # label wider than the whole series: use the first point
    max(idx)
  }

  if (identical(geom_type, "column")) {
    x_all <- (own$xmin + own$xmax) / 2
    j <- step_from_end(x_all)
    x <- x_all[j]
    y <- stagger(own$ymax[j], c(own$ymin, own$ymax))
  } else if (identical(geom_type, "area")) {
    j <- step_from_end(own$x)
    x <- own$x[j]
    y <- stagger(own$ymax[j], own$ymax)
  } else {
    # line / point / pointbar
    j <- step_from_end(own$x)
    x <- own$x[j]
    y <- stagger(own$y[j], own$y)
  }

  list(x = x, y = y)
}

#' Place a set of labels on a single-panel plot, one at a time, updating
#' the occupancy mask after each so later labels avoid earlier ones.
#'
#' An explicit fallback_x/fallback_y always wins outright -- the
#' scored/inside-band search never runs for that label, so a position the
#' caller deliberately chose can't be second-guessed. Failing that (no
#' user position given), a label falls back through, in order: (1) the
#' scored/inside-band search, if `fast = FALSE`; (2) any collision-free
#' spot at all, ignoring buffer/distance preferences; (3) the panel
#' centre, so the label stays visible rather than vanishing. Under
#' `fast = TRUE`, (1)-(3) are all replaced by a single cheap, render-free
#' placement (see t61_place_label_fast()) -- used for the automatic Viewer
#' preview on print(), where speed matters more than an optimal spot.
#'
#' @param plot A ggplot object, fully built (scales resolved etc.), not yet
#'   containing the labels to be placed.
#' @param labels A data frame with columns: text, series (list-column of
#'   list(x=,y=)), geom_type, hjust, size_mm, fallback_x, fallback_y
#'   (fallback_x/fallback_y may be NA -- x/y are optional when
#'   auto_position = TRUE). A row with no matched series (colour didn't
#'   match anything) has geom_type = NA and series = list() -- skips
#'   straight to the fallback tiers below.
#' @param width_cm,height_cm Physical size the chart will be saved at.
#' @param fast Logical. Skip the mask render and scored search entirely,
#'   using t61_place_label_fast() instead for any label without an
#'   explicit position. Much cheaper, at the cost of placement quality (no
#'   collision avoidance against other content or other labels).
#' @return `labels` with x/y columns added (resolved position), a `placed`
#'   logical column (FALSE only when the resolved position is exactly the
#'   caller's own unchanged fallback_x/fallback_y, so the caller can skip a
#'   redundant write-back), and a `colour` character column
#'   (NA_character_ unless an "area" label got placed inside its band, in
#'   which case it holds the contrast colour to render it in).
#' @noRd
t61_autolabel_plot <- function(plot, labels, width_cm, height_cm, px_width = 400L, fast = FALSE) {

  labels$x <- labels$fallback_x
  labels$y <- labels$fallback_y
  labels$placed <- FALSE
  labels$colour <- NA_character_

  mask <- NULL
  area_mask <- NULL
  flipped <- FALSE

  if (!fast) {
    mask <- t61_render_mask(plot, width_cm = width_cm, height_cm = height_cm, px_width = px_width)
    if (is.null(mask)) return(labels) # not v1 scope (e.g. facets): keep fallbacks

    # Everything below (mask-dependent branches only) operates in the
    # mask's screen space, which panel_params has already remapped for
    # coord_flip() -- but series data and the label's own x/y come from
    # ggplot_build()$data in pre-flip data-aesthetic space. Only
    # point/line/column reduce to a plain axis swap under flip; area's
    # band search and pointbar's error-bar orientation would each need
    # their own flipped-geometry logic, so under a flip they're treated as
    # unmatched (skip to the fallback tiers) rather than placed
    # incorrectly.
    flipped <- isTRUE(mask$flipped)

    # An area label sitting inside its own fill can't use `mask` for
    # collision -- the fill itself reads as ink there, so every inside
    # candidate would "collide". Rendered lazily (a second full mask
    # render) only when an area label needs it, and skipped entirely under
    # coord_flip() (see above).
    if (!flipped && any(labels$geom_type == "area", na.rm = TRUE)) {
      area_mask <- t61_render_mask(t61_strip_area_layers(plot), width_cm = width_cm,
                                    height_cm = height_cm, px_width = px_width)
    }
  }

  to_screen <- function(x, y) if (flipped) t61_flip_xy(x, y) else list(x = x, y = y)
  to_data   <- to_screen # the swap is its own inverse

  for (i in seq_len(nrow(labels))) {
    own <- labels$series[[i]]
    geom_type <- labels$geom_type[i]
    if (!fast && flipped && geom_type %in% c("area", "pointbar")) {
      own <- list(); geom_type <- NA_character_
    }
    # series = list() (geom_type = NA) is the "no series matched" sentinel
    # -- nothing to place a "good" spot against, so skip straight to the
    # fallback tiers below.
    has_series <- length(own) > 0

    # An explicit position always wins outright: the search below never
    # even runs for this label, so it can't be overridden by something the
    # algorithm merely scores "better".
    has_user_position <- !is.na(labels$fallback_x[i]) && !is.na(labels$fallback_y[i])
    used_user_fallback <- FALSE

    result <- NULL
    result_colour <- NA_character_

    if (has_user_position) {
      fx <- labels$fallback_x[i]; fy <- labels$fallback_y[i]
      box <- NULL
      if (!is.null(mask)) {
        label_cm <- t61_measure_label_cm(labels$text[i], size_mm = labels$size_mm[i],
                                         width_cm = width_cm, height_cm = height_cm)
        screen_xy <- to_screen(fx, fy)
        box <- t61_text_box_px(screen_xy$x, screen_xy$y, label_cm, mask, hjust = labels$hjust[i])
      }
      result <- list(x = fx, y = fy, box = box)
      used_user_fallback <- TRUE

    } else if (fast) {
      # No mask, no search -- a cheap offset from the label's own series if
      # one matched, otherwise nothing to derive a spot from cheaply, so
      # the label keeps its (possibly NA) fallback, same as the "not v1
      # scope" case above.
      if (has_series) {
        fp <- t61_place_label_fast(own, geom_type, index = i, hjust = labels$hjust[i],
                                   text = labels$text[i], size_mm = labels$size_mm[i],
                                   width_cm = width_cm, height_cm = height_cm)
        result <- list(x = fp$x, y = fp$y, box = NULL)
      }

    } else {
      label_cm <- t61_measure_label_cm(labels$text[i], size_mm = labels$size_mm[i],
                                       width_cm = width_cm, height_cm = height_cm)

      if (has_series && identical(geom_type, "area") && !is.null(area_mask)) {
        # Prefer a spot fully inside the band, in a colour that contrasts
        # with the fill -- only fall back to the usual edge-hugging line
        # treatment (against the area's top boundary, in the fill's own
        # colour, unchanged) when the band is too narrow anywhere to fit
        # the label inside it.
        result <- t61_place_label_area(own, mask = area_mask, label_cm = label_cm, hjust = labels$hjust[i])
        if (!is.null(result)) {
          alpha <- if (is.null(own$alpha)) 1 else own$alpha
          result_colour <- t61_contrast_colour(t61_blend_with_background(own$fill, alpha))
        }
      }

      if (has_series && is.null(result)) {
        other_series <- lapply(setdiff(seq_len(nrow(labels)), i), function(j) {
          s <- labels$series[[j]]
          if (length(s) == 0) return(NULL)
          gt <- labels$geom_type[j]
          if (flipped && gt %in% c("area", "pointbar")) return(NULL)
          s <- c(s, list(geom_type = gt))
          if (flipped) {
            if (identical(gt, "column")) {
              s[c("xmin", "xmax", "ymin", "ymax")] <- t61_flip_rect(s$xmin, s$xmax, s$ymin, s$ymax)
            } else {
              s[c("x", "y")] <- t61_flip_xy(s$x, s$y)
            }
          }
          s
        })
        other_series <- other_series[!vapply(other_series, is.null, logical(1))]

        series_for_line <- if (identical(geom_type, "area")) list(x = own$x, y = own$ymax) else own
        line_geom_type <- if (identical(geom_type, "area")) "line" else geom_type

        if (flipped) {
          if (identical(line_geom_type, "column")) {
            series_for_line[c("xmin", "xmax", "ymin", "ymax")] <-
              t61_flip_rect(series_for_line$xmin, series_for_line$xmax, series_for_line$ymin, series_for_line$ymax)
          } else {
            series_for_line[c("x", "y")] <- t61_flip_xy(series_for_line$x, series_for_line$y)
          }
        }

        result <- t61_place_label(
          series = series_for_line,
          geom_type = line_geom_type,
          other_series = other_series,
          mask = mask,
          label_cm = label_cm,
          hjust = labels$hjust[i]
        )
        if (!is.null(result)) {
          xy <- to_data(result$x, result$y)
          result$x <- xy$x; result$y <- xy$y
        }
      }

      # No user position and no scored/inside-band spot -- any
      # collision-free spot at all, ignoring buffer/distance preferences.
      if (is.null(result)) {
        result <- t61_place_label_fallback(mask, label_cm, hjust = labels$hjust[i])
        if (!is.null(result)) {
          xy <- to_data(result$x, result$y)
          result$x <- xy$x; result$y <- xy$y
        }
      }

      # Last resort: nothing worked and there's no user position -- the
      # panel centre, so the label is still visible rather than vanishing.
      if (is.null(result)) {
        cx <- mean(mask$x_range); cy <- mean(mask$y_range)
        box <- t61_text_box_px(cx, cy, label_cm, mask, hjust = labels$hjust[i])
        xy <- to_data(cx, cy)
        result <- list(x = xy$x, y = xy$y, box = box)
      }
    }

    if (!is.null(result)) {
      labels$x[i] <- result$x
      labels$y[i] <- result$y
      labels$placed[i] <- !used_user_fallback
      labels$colour[i] <- result_colour
      if (!is.null(mask) && !is.null(result$box)) {
        mask$occupancy <- t61_stamp_box(mask$occupancy, result$box$row_range, result$box$col_range)
        if (!is.null(area_mask)) {
          area_mask$occupancy <- t61_stamp_box(area_mask$occupancy, result$box$row_range, result$box$col_range)
        }
      }
    }
  }

  labels
}
