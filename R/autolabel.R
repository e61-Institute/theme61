# Orchestration: ties the mask/collision/distance/line-of-sight/selection
# modules together to place one or more labels on a plot (issue #159).
#
# v1 scope: single-panel (unfacetted) plots, "point"/"line"/"column"/"area"/
# "pointbar" geoms, 2-3 series -- see the architecture discussion on issue
# #159 for why this scope was chosen as the starting point.

#' Try to place a single label, evaluating every candidate in the grid
#' against the current mask. Returns a list(x, y, box) on success, or NULL
#' if every candidate either collides or there's nothing to evaluate.
#'
#' Candidates closer to their own series than `min_buffer_cm` are excluded
#' on a first pass, so a comfortably-buffered spot anywhere on the panel is
#' always preferred over a technically-closer but cramped one nearby --
#' the selection tiebreak in t61_select_best_candidate() only chooses
#' *among* whatever candidates are on offer, so it can't claim buffer that
#' was never in the running to begin with. Only if that first pass finds
#' nothing (e.g. a busy chart with no clear buffered spot anywhere) does
#' this fall back to allowing tighter candidates, so a label degrades to
#' "close" rather than disappearing.
#'
#' @param series The series this label belongs to (its own "home" series):
#'   list(x=, y=) for "point"/"line", list(xmin=, xmax=, ymin=, ymax=) for
#'   "column", or list(x=, y=, ymin=, ymax=) for "pointbar" -- see
#'   t61_box_distance_to_series().
#' @param other_series A list of other series (as above, each with a
#'   geom_type= element added) to measure "ambiguity" against -- see
#'   t61_selection_score().
#' @noRd
t61_place_label <- function(series, geom_type, other_series, mask, label_cm,
                            hjust = 0, vjust = 0.5, n_x = 24, n_y = 32, margin = 0.08,
                            min_buffer_cm = NULL) {

  if (is.null(min_buffer_cm)) {
    # Points get a much higher floor than lines -- see t61_target_buffer_cm()
    # for why "distance to the nearest point" needs a bigger multiplier to
    # actually clear a scattered cluster rather than just one point in it.
    # Reduced by a third from the original tuning, to match
    # t61_target_buffer_cm() -- see its comment for why.
    mult <- if (identical(geom_type, "point")) 3.4 else 1.13
    min_buffer_cm <- mult * label_cm$height_cm
  }

  units <- t61_mask_units_cm(mask)
  grid <- t61_candidate_grid(mask$x_range, mask$y_range, n_x = n_x, n_y = n_y, margin = margin)

  build_candidates <- function(enforce_min_buffer) {
    rows <- vector("list", nrow(grid))

    for (i in seq_len(nrow(grid))) {
      x <- grid$x[i]; y <- grid$y[i]

      box <- t61_text_box_px(x, y, label_cm, mask, hjust = hjust, vjust = vjust)
      # Unlike t61_test_collision() -- which silently clips a partially
      # off-raster box to whatever's visible and only checks ink there --
      # a box that hangs off the panel at all is rejected outright here,
      # since ggplot2 clips drawn text to the panel and a partially
      # off-panel label would render visibly truncated.
      if (!t61_box_in_bounds(box$row_range, box$col_range, mask)) next
      if (t61_test_collision(mask$occupancy, box$row_range, box$col_range)) next

      # Measured from the box's actual footprint, not just the (x, y)
      # anchor: with the default hjust = 0 the box extends a full
      # label-width away from the anchor, so an anchor that clears the
      # buffer comfortably can still leave the box sitting right on top of
      # a point or line -- see t61_box_distance_to_series().
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

  candidates <- build_candidates(enforce_min_buffer = TRUE)
  if (is.null(candidates) || nrow(candidates) == 0) {
    candidates <- build_candidates(enforce_min_buffer = FALSE)
  }
  if (is.null(candidates) || nrow(candidates) == 0) return(NULL)

  best <- t61_select_best_candidate(candidates, label_cm$height_cm, geom_type = geom_type)
  box <- t61_text_box_px(best$x, best$y, label_cm, mask, hjust = hjust, vjust = vjust)

  list(x = best$x, y = best$y, box = box)
}

#' Fallback placement when no candidate in the grid is collision-free:
#' spiral outward from the panel centre (alternating toward each edge),
#' looking only for *any* clear spot, ignoring distance/line-of-sight.
#' Mirrors arphit's autolabel_fallback().
#'
#' Like t61_place_label()'s candidate loop, a box must fit fully in the
#' mask's raster before it's accepted -- t61_test_collision() alone isn't
#' enough here, since it silently clips a box that hangs off the edge to
#' whatever's visible and only checks *that* for ink, so an edge-hugging
#' box over empty margin space would otherwise pass as "clear" and render
#' visibly truncated. x_steps/y_steps reach all the way to the data range's
#' own edges (mask$x_range[2] etc.), so this is routinely hit here, unlike
#' t61_place_label()'s grid, which already keeps a margin off the edges.
#' @noRd
t61_place_label_fallback <- function(mask, label_cm, hjust = 0, vjust = 0.5, n_steps = 12) {
  x_mid <- mean(mask$x_range); y_mid <- mean(mask$y_range)

  x_up   <- seq(x_mid, mask$x_range[2], length.out = n_steps)
  x_down <- seq(x_mid, mask$x_range[1], length.out = n_steps)
  y_up   <- seq(y_mid, mask$y_range[2], length.out = n_steps)
  y_down <- seq(y_mid, mask$y_range[1], length.out = n_steps)

  x_steps <- c(rbind(x_up, x_down))
  y_steps <- c(rbind(y_up, y_down))

  for (x in x_steps) {
    for (y in y_steps) {
      box <- t61_text_box_px(x, y, label_cm, mask, hjust = hjust, vjust = vjust)
      if (!t61_box_in_bounds(box$row_range, box$col_range, mask)) next
      if (!t61_test_collision(mask$occupancy, box$row_range, box$col_range)) {
        return(list(x = x, y = y, box = box))
      }
    }
  }

  NULL
}

#' Place a set of labels on a single-panel plot, one at a time, updating
#' the occupancy mask after each placement so later labels avoid earlier
#' ones.
#'
#' A label that the scored/inside-band placement can't resolve a good spot
#' for falls back through, in order: (1) `fallback_x`/`fallback_y`, if the
#' caller supplied one (`plot_label(x=, y=)`, i.e. `!is.na(fallback_x)`);
#' (2) otherwise, any collision-free spot on the chart at all, ignoring
#' buffer/distance preferences (t61_place_label_fallback(), effectively
#' "empty space somewhere"); (3) if even that fails (the mask has no clear
#' space anywhere) and there's no fallback position either, the panel
#' centre, so the label is still visible somewhere rather than vanishing.
#'
#' @param plot A ggplot object, fully built (scales resolved etc.), not yet
#'   containing the labels to be placed.
#' @param labels A data frame with columns: text, series (list-column of
#'   list(x=,y=)), geom_type, hjust, size_mm, fallback_x, fallback_y
#'   (fallback_x/fallback_y may be NA -- see ?plot_label, x/y are optional
#'   when auto_position = TRUE). A row with no matched series (colour
#'   didn't match anything) has geom_type = NA and series = list() --
#'   skips straight to the fallback tiers below.
#' @param width_cm,height_cm Physical size the chart will be saved at.
#' @return `labels` with x/y columns added (resolved position), a `placed`
#'   logical column (FALSE only when the resolved position is exactly the
#'   caller's own unchanged fallback_x/fallback_y, so the caller can skip a
#'   redundant write-back), and a `colour` character column
#'   (NA_character_ unless an "area" label got placed inside its band, in
#'   which case it holds the contrast colour to render it in -- see
#'   t61_place_label_area()).
#' @noRd
t61_autolabel_plot <- function(plot, labels, width_cm, height_cm, px_width = 400L) {

  mask <- t61_render_mask(plot, width_cm = width_cm, height_cm = height_cm, px_width = px_width)

  labels$x <- labels$fallback_x
  labels$y <- labels$fallback_y
  labels$placed <- FALSE
  labels$colour <- NA_character_

  if (is.null(mask)) return(labels) # not v1 scope (e.g. facets): keep fallbacks

  # A label meant to sit inside an area's own fill can't use `mask` for its
  # collision check -- the fill itself is real rendered ink there, so every
  # inside candidate would "collide" and placement would always fall back
  # to outside. Rendered once, lazily, only when there's an area label to
  # place, since it's a second full mask render.
  area_mask <- NULL
  if (any(labels$geom_type == "area", na.rm = TRUE)) {
    area_mask <- t61_render_mask(t61_strip_area_layers(plot), width_cm = width_cm,
                                  height_cm = height_cm, px_width = px_width)
  }

  for (i in seq_len(nrow(labels))) {
    label_cm <- t61_measure_label_cm(labels$text[i], size_mm = labels$size_mm[i],
                                     width_cm = width_cm, height_cm = height_cm)

    own <- labels$series[[i]]
    geom_type <- labels$geom_type[i]
    # series = list() (geom_type = NA) is the "no series matched" sentinel
    # from t61_collect_autolabel_targets() -- nothing to score a "good"
    # placement against, so skip straight to the fallback tiers below.
    has_series <- length(own) > 0

    other_series <- lapply(setdiff(seq_len(nrow(labels)), i), function(j) {
      s <- labels$series[[j]]
      if (length(s) == 0) return(NULL)
      c(s, list(geom_type = labels$geom_type[j]))
    })
    other_series <- other_series[!vapply(other_series, is.null, logical(1))]

    result <- NULL
    result_colour <- NA_character_

    if (has_series && identical(geom_type, "area") && !is.null(area_mask)) {
      # Prefer a spot fully inside the band, in a colour that contrasts
      # with the fill -- only fall back to the usual edge-hugging line
      # treatment (against the area's top boundary, in the fill's own
      # colour, unchanged) when the band is too narrow anywhere to fit the
      # label inside it.
      result <- t61_place_label_area(own, mask = area_mask, label_cm = label_cm, hjust = labels$hjust[i])
      if (!is.null(result)) {
        alpha <- if (is.null(own$alpha)) 1 else own$alpha
        result_colour <- t61_contrast_colour(t61_blend_with_background(own$fill, alpha))
      }
    }

    if (has_series && is.null(result)) {
      series_for_line <- if (identical(geom_type, "area")) list(x = own$x, y = own$ymax) else own
      line_geom_type <- if (identical(geom_type, "area")) "line" else geom_type

      result <- t61_place_label(
        series = series_for_line,
        geom_type = line_geom_type,
        other_series = other_series,
        mask = mask,
        label_cm = label_cm,
        hjust = labels$hjust[i]
      )
    }

    # Tier 2: the caller's own fallback position, if they gave one --
    # preferred over a blind "any empty space" spot, since it's a position
    # the caller actually chose.
    has_user_position <- !is.na(labels$fallback_x[i]) && !is.na(labels$fallback_y[i])
    used_user_fallback <- FALSE

    if (is.null(result) && has_user_position) {
      fx <- labels$fallback_x[i]; fy <- labels$fallback_y[i]
      result <- list(x = fx, y = fy, box = t61_text_box_px(fx, fy, label_cm, mask, hjust = labels$hjust[i]))
      used_user_fallback <- TRUE
    }

    # Tier 3: no user position to fall back on -- any collision-free spot
    # at all, ignoring buffer/distance preferences.
    if (is.null(result)) {
      result <- t61_place_label_fallback(mask, label_cm, hjust = labels$hjust[i])
    }

    # Tier 4 (last resort): nothing worked and there's no user position --
    # the panel centre, so the label is still visible rather than vanishing.
    if (is.null(result)) {
      cx <- mean(mask$x_range); cy <- mean(mask$y_range)
      result <- list(x = cx, y = cy, box = t61_text_box_px(cx, cy, label_cm, mask, hjust = labels$hjust[i]))
    }

    labels$x[i] <- result$x
    labels$y[i] <- result$y
    labels$placed[i] <- !used_user_fallback
    labels$colour[i] <- result_colour
    mask$occupancy <- t61_stamp_box(mask$occupancy, result$box$row_range, result$box$col_range)
    if (!is.null(area_mask)) {
      area_mask$occupancy <- t61_stamp_box(area_mask$occupancy, result$box$row_range, result$box$col_range)
    }
  }

  labels
}
