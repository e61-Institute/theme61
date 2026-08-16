# Candidate position generation, and picking the best of a set of already-
# evaluated candidates.

#' A grid of candidate (x, y) anchor positions to try for a label, spread
#' across the panel's data ranges. Anchors near the very edges are excluded
#' -- a label anchored right at the axis limit tends to look cramped and is
#' likely to get clipped.
#'
#' v1 scope: a uniform grid is enough for simple 2-3 series charts -- revisit
#' if placements come out landing awkwardly close to gridlines/ticks (e.g.
#' snapping y-anchors to axis-break subdivisions instead).
#' @noRd
t61_candidate_grid <- function(x_range, y_range, n_x = 9, n_y = 12, margin = 0.08) {
  x_pad <- diff(x_range) * margin
  y_pad <- diff(y_range) * margin

  x_seq <- seq(x_range[1] + x_pad, x_range[2] - x_pad, length.out = n_x)
  y_seq <- seq(y_range[1] + y_pad, y_range[2] - y_pad, length.out = n_y)

  expand.grid(x = x_seq, y = y_seq)
}

#' Tier a candidate's distance-to-series into "near/mid/far", relative to
#' the label's own height in cm (so the thresholds scale with font size and
#' chart size, rather than being fixed absolute distances).
#' @noRd
t61_distance_tier <- function(distance_cm, label_height_cm) {
  if (distance_cm < 0.5 * label_height_cm) return(1L)
  if (distance_cm < 1.0 * label_height_cm) return(2L)
  if (distance_cm < 1.5 * label_height_cm) return(3L)
  4L
}

#' Group a candidate's distance into a coarse bucket for *selection*
#' purposes (as opposed to t61_distance_tier(), which is a fixed,
#' independently-tested near/mid/far classification used elsewhere).
#' Unlike t61_distance_tier(), these cutoffs scale with the target buffer
#' (t61_target_buffer_cm()) rather than being fixed multiples of label
#' height: the target must always fall well inside group 1, or the buffer
#' tiebreak below would never get to prefer a candidate near the target
#' over one that merely hugs the series, since group is compared before
#' the tiebreak. A fixed cutoff would silently stop guaranteeing that
#' the moment the target buffer was tuned past it.
#' @noRd
t61_selection_group <- function(distance_cm, label_height_cm, geom_type = "line") {
  target <- t61_target_buffer_cm(label_height_cm, geom_type)
  if (distance_cm < 1.5 * target) return(1L)
  if (distance_cm < 2.5 * target) return(2L)
  3L
}

#' Score a single candidate for ranking against others. Lower is better.
#' Returns a numeric vector suitable for lexicographic ordering (see
#' t61_select_best_candidate()):
#'   1. ambiguous: is this candidate actually closer to a DIFFERENT series
#'      than the one it's meant to label? (1 = yes, worse; 0 = no)
#'   2. group: coarse near/mid/far bucket (t61_selection_group())
#'   3. no_los: 1 if the line back to the series is blocked, else 0
#'   4. buffer_penalty: final tiebreak, smaller wins -- see below
#' Expressed as an orderable key (for lexicographic sort) rather than
#' enumerated cases, since the tiebreak priority is itself the point.
#'
#' The final tiebreak prefers a candidate close to a target buffer
#' (t61_target_buffer_cm()) rather than literally the closest point
#' available: among a set of otherwise-equal "close enough" candidates
#' (i.e. when the grid offers a choice), hugging the series as tightly as
#' possible reads as cramped, so claim some breathing room instead when
#' it's on offer. The target sits inside the merged near group (see
#' t61_selection_group()), so this never pulls a candidate into a worse
#' group, and it has no effect on the far groups (there, preferring
#' proximity to the target and preferring proximity to the series are the
#' same ordering, since every candidate is already past the target).
#'
#' edge_penalty_cm/gridline_penalty_cm (t61_edge_penalty_cm(),
#' t61_gridline_penalty_cm()) fold into this same final tiebreak, not a
#' separate ordering key: hugging the panel edge or straddling a gridline
#' should make a candidate less attractive relative to others, but must
#' never exclude it outright or promote a worse-tier candidate over it --
#' the only hard exclusions are collision/off-panel, checked earlier in
#' t61_place_label(). Both are optional (default 0, i.e. no penalty), so
#' a caller that doesn't have them to hand can still score candidates on
#' distance/line-of-sight alone.
#'
#' geom_type gets a much bigger multiplier for "point" than "line": a
#' line has one "own" direction to step away from, so a buffer scaled off
#' label height reads fine, but a scattered point cluster has no single
#' direction -- "distance to the nearest point" collapses back down to a
#' small value the moment a candidate sits anywhere near the cloud, no
#' matter which way it moves, since some other point in the cluster is
#' always nearby. A far larger target is needed for the result to actually
#' read as clear of the cluster rather than merely clear of one point in it.
#'
#' Distance here is measured from the box's actual footprint
#' (t61_box_distance_to_series()), not just the anchor point -- a strictly
#' larger, more truthful distance for the common hjust != 0.5 case -- so
#' these multipliers are correspondingly smaller than a naive visual buffer
#' would suggest.
#' @noRd
t61_target_buffer_cm <- function(label_height_cm, geom_type = "line") {
  mult <- if (identical(geom_type, "point")) 3.8 else 1.27
  mult * label_height_cm
}

#' @noRd
t61_selection_score <- function(distance_cm, next_closest_cm, los, label_height_cm,
                                 edge_penalty_cm = 0, gridline_penalty_cm = 0,
                                 geom_type = "line") {
  c(
    ambiguous       = as.numeric(next_closest_cm <= distance_cm),
    tier            = t61_selection_group(distance_cm, label_height_cm, geom_type),
    no_los          = as.numeric(!los),
    buffer_penalty  = abs(distance_cm - t61_target_buffer_cm(label_height_cm, geom_type)) +
      edge_penalty_cm + gridline_penalty_cm
  )
}

#' Pick the best row from a data frame of scored candidates (must have
#' columns x, y, distance_cm, next_closest_cm, los; edge_penalty_cm and
#' gridline_penalty_cm are optional, treated as 0 if absent). Returns NULL
#' if there are no candidates to choose from.
#' @noRd
t61_select_best_candidate <- function(candidates, label_height_cm, geom_type = "line") {
  if (nrow(candidates) == 0) return(NULL)

  edge_penalty_cm <- if (!is.null(candidates$edge_penalty_cm)) {
    candidates$edge_penalty_cm
  } else {
    rep(0, nrow(candidates))
  }
  gridline_penalty_cm <- if (!is.null(candidates$gridline_penalty_cm)) {
    candidates$gridline_penalty_cm
  } else {
    rep(0, nrow(candidates))
  }

  scores <- t(mapply(
    t61_selection_score,
    distance_cm          = candidates$distance_cm,
    next_closest_cm      = candidates$next_closest_cm,
    los                  = candidates$los,
    edge_penalty_cm      = edge_penalty_cm,
    gridline_penalty_cm  = gridline_penalty_cm,
    MoreArgs = list(label_height_cm = label_height_cm, geom_type = geom_type)
  ))

  ord <- order(scores[, "ambiguous"], scores[, "tier"], scores[, "no_los"], scores[, "buffer_penalty"])

  candidates[ord[1], ]
}
