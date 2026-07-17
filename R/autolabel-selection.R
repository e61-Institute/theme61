# Candidate position generation, and picking the best of a set of already-
# evaluated candidates (issue #159).

#' A grid of candidate (x, y) anchor positions to try for a label, spread
#' across the panel's data ranges. Anchors near the very edges are excluded
#' (a label anchored right at the axis limit tends to look cramped and is
#' likely to get clipped), mirroring arphit's x_anchors margin.
#'
#' v1 scope: a uniform grid is enough for simple 2-3 series charts, unlike
#' arphit's y-anchors (snapped to axis-break subdivisions) -- revisit if
#' placements come out landing awkwardly close to gridlines/ticks.
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

#' Group a candidate's distance tier into a coarser bucket for *selection*
#' purposes (as opposed to t61_distance_tier(), which is the finer-grained
#' near/mid/far classification used elsewhere/tested directly). Tiers 1
#' and 2 (i.e. anything closer than one full label-height) are merged into
#' a single "close enough" bucket: within that bucket the buffer tiebreak
#' below is free to prefer a candidate with real breathing room over one
#' that's technically closer but uncomfortably tight, instead of always
#' being forced to accept the tightest tier-1 spot regardless of how
#' cramped it is. Tiers 3/4 are kept apart, since by then "closer is
#' better" is unambiguously true and there's no buffer to claim.
#' @noRd
t61_selection_group <- function(distance_cm, label_height_cm) {
  tier <- t61_distance_tier(distance_cm, label_height_cm)
  if (tier <= 2L) return(1L)
  tier - 1L
}

#' Score a single candidate for ranking against others. Lower is better.
#' Returns a numeric vector suitable for lexicographic ordering (see
#' t61_select_best_candidate()):
#'   1. ambiguous: is this candidate actually closer to a DIFFERENT series
#'      than the one it's meant to label? (1 = yes, worse; 0 = no)
#'   2. group: coarse near/mid/far bucket (t61_selection_group())
#'   3. no_los: 1 if the line back to the series is blocked, else 0
#'   4. buffer_penalty: final tiebreak, smaller wins -- see below
#' This plays the same role as arphit's assign_selection_group(), just
#' expressed as an orderable key instead of ~20 enumerated cases.
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
#' @noRd
t61_target_buffer_cm <- function(label_height_cm) 0.85 * label_height_cm

#' @noRd
t61_selection_score <- function(distance_cm, next_closest_cm, los, label_height_cm) {
  c(
    ambiguous       = as.numeric(next_closest_cm <= distance_cm),
    tier            = t61_selection_group(distance_cm, label_height_cm),
    no_los          = as.numeric(!los),
    buffer_penalty  = abs(distance_cm - t61_target_buffer_cm(label_height_cm))
  )
}

#' Pick the best row from a data frame of scored candidates (must have
#' columns x, y, distance_cm, next_closest_cm, los). Returns NULL if there
#' are no candidates to choose from.
#' @noRd
t61_select_best_candidate <- function(candidates, label_height_cm) {
  if (nrow(candidates) == 0) return(NULL)

  scores <- t(mapply(
    t61_selection_score,
    candidates$distance_cm,
    candidates$next_closest_cm,
    candidates$los,
    MoreArgs = list(label_height_cm = label_height_cm)
  ))

  ord <- order(scores[, "ambiguous"], scores[, "tier"], scores[, "no_los"], scores[, "buffer_penalty"])

  candidates[ord[1], ]
}
