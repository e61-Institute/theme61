# Orchestration: ties the mask/collision/distance/line-of-sight/selection
# modules together to place one or more labels on a plot (issue #159).
#
# v1 scope: single-panel (unfacetted) plots, "point"/"line" geoms, 2-3
# series -- see the architecture discussion on issue #159 for why this
# scope was chosen as the starting point.

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
#' @param series Data-space x/y vectors for the series this label belongs
#'   to (its own "home" series).
#' @param other_series A list of other series (each list(x=, y=, geom_type=))
#'   to measure "ambiguity" against -- see t61_selection_score().
#' @noRd
t61_place_label <- function(series, geom_type, other_series, mask, label_cm,
                            hjust = 0, vjust = 0.5, n_x = 24, n_y = 32, margin = 0.08,
                            min_buffer_cm = NULL) {

  if (is.null(min_buffer_cm)) min_buffer_cm <- 0.85 * label_cm$height_cm

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
      if (!t61_box_in_bounds(box$row_range, box$col_range, mask$occupancy)) next
      if (t61_test_collision(mask$occupancy, box$row_range, box$col_range)) next

      own <- t61_distance_to_series(x, y, series$x, series$y, geom_type, units)
      if (enforce_min_buffer && own$distance < min_buffer_cm) next

      next_closest <- Inf
      for (s in other_series) {
        d <- t61_distance_to_series(x, y, s$x, s$y, s$geom_type, units)
        if (d$distance < next_closest) next_closest <- d$distance
      }

      anchor <- t61_data_to_px(x, y, mask)
      nearest_px <- t61_data_to_px(own$x, own$y, mask)
      los <- t61_line_of_sight(mask$occupancy, anchor$row, anchor$col, nearest_px$row, nearest_px$col)

      rows[[i]] <- data.frame(x = x, y = y, distance_cm = own$distance,
                              next_closest_cm = next_closest, los = los)
    }

    do.call(rbind, rows)
  }

  candidates <- build_candidates(enforce_min_buffer = TRUE)
  if (is.null(candidates) || nrow(candidates) == 0) {
    candidates <- build_candidates(enforce_min_buffer = FALSE)
  }
  if (is.null(candidates) || nrow(candidates) == 0) return(NULL)

  best <- t61_select_best_candidate(candidates, label_cm$height_cm)
  box <- t61_text_box_px(best$x, best$y, label_cm, mask, hjust = hjust, vjust = vjust)

  list(x = best$x, y = best$y, box = box)
}

#' Fallback placement when no candidate in the grid is collision-free:
#' spiral outward from the panel centre (alternating toward each edge),
#' looking only for *any* clear spot, ignoring distance/line-of-sight.
#' Mirrors arphit's autolabel_fallback().
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
      if (!t61_test_collision(mask$occupancy, box$row_range, box$col_range)) {
        return(list(x = x, y = y, box = box))
      }
    }
  }

  NULL
}

#' Place a set of labels on a single-panel plot, one at a time, updating
#' the occupancy mask after each placement so later labels avoid earlier
#' ones. Failures degrade silently: a label that can't be placed keeps
#' its `fallback_x`/`fallback_y` position (the Phase-1 placeholder a
#' caller would already have drawn) rather than erroring or vanishing.
#'
#' @param plot A ggplot object, fully built (scales resolved etc.), not yet
#'   containing the labels to be placed.
#' @param labels A data frame with columns: text, series (list-column of
#'   list(x=,y=)), geom_type, hjust, size_mm, fallback_x, fallback_y.
#' @param width_cm,height_cm Physical size the chart will be saved at.
#' @return `labels` with x/y columns added (resolved position) and a
#'   `placed` logical column (FALSE means fallback_x/fallback_y was kept).
#' @noRd
t61_autolabel_plot <- function(plot, labels, width_cm, height_cm, px_width = 400L) {

  mask <- t61_render_mask(plot, width_cm = width_cm, height_cm = height_cm, px_width = px_width)

  labels$x <- labels$fallback_x
  labels$y <- labels$fallback_y
  labels$placed <- FALSE

  if (is.null(mask)) return(labels) # not v1 scope (e.g. facets): keep fallbacks

  for (i in seq_len(nrow(labels))) {
    label_cm <- t61_measure_label_cm(labels$text[i], size_mm = labels$size_mm[i],
                                     width_cm = width_cm, height_cm = height_cm)

    own <- labels$series[[i]]
    other_series <- lapply(setdiff(seq_len(nrow(labels)), i), function(j) {
      c(labels$series[[j]], list(geom_type = labels$geom_type[j]))
    })

    result <- t61_place_label(
      series = own,
      geom_type = labels$geom_type[i],
      other_series = other_series,
      mask = mask,
      label_cm = label_cm,
      hjust = labels$hjust[i]
    )

    if (is.null(result)) {
      result <- t61_place_label_fallback(mask, label_cm, hjust = labels$hjust[i])
    }

    if (!is.null(result)) {
      labels$x[i] <- result$x
      labels$y[i] <- result$y
      labels$placed[i] <- TRUE
      mask$occupancy <- t61_stamp_box(mask$occupancy, result$box$row_range, result$box$col_range)
    }
    # else: result still NULL even after fallback (mask has no clear space
    # left at all) -- keep the Phase-1 fallback_x/fallback_y untouched.
  }

  labels
}
