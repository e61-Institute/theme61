#' Add automatic percentage labels to column charts
#'
#' @description Adds a text label above (or inside) each column showing its
#'   share of a total, as a percentage. Percentages are calculated
#'   automatically from the data, so there is no need to pre-compute a
#'   percentage column:
#'   \itemize{
#'     \item For \strong{stacked} columns (i.e. more than one value sharing
#'     the same `x`, typically via a `fill` aesthetic), each label shows that
#'     segment's share of its column's stack.
#'     \item For \strong{single} (non-stacked) columns, each label shows that
#'     column's share of the total across all columns in the panel.
#'   }
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   Requires `x` and `y`; add `fill` (as you would for [ggplot2::geom_col()])
#'   to create stacked segments.
#' @param data The data to be displayed in this layer.
#' @param accuracy Numeric. Passed to [scales::label_percent()] to control
#'   rounding, e.g. `accuracy = 0.1` shows one decimal place. Defaults to `1`
#'   (whole percentages).
#' @param align Where to position the label. One of `"top"`, `"middle"`,
#'   `"bottom"`, or a number from 0 (bottom of the column/segment) to 1 (top
#'   of the column/segment). Defaults to `"top"`. For single (non-stacked)
#'   columns, `"top"` floats the label just above the column, and `"bottom"`
#'   sits it just inside the column above its base - both leave a small gap
#'   rather than sitting flush against the column. For stacked columns,
#'   `"top"`/`"bottom"` sit just inside **each segment's own** top/bottom
#'   edge (not just the outer edge of the stack as a whole) with the same
#'   gap. Any other value centres the label inside the column/segment at
#'   that fraction of its height.
#' @param reverse Logical. Reverse the stacking order used to position labels
#'   within stacked columns. Set this to match `position_stack(reverse =
#'   TRUE)` if you used that for your `geom_col()`. Defaults to FALSE.
#' @param position One of `"stack"` (the default), `"dodge"`, or `"dodge2"`.
#'   Set this to match the `position` you used on the corresponding
#'   [ggplot2::geom_col()] call. `"dodge"`/`"dodge2"` split each bar's width
#'   the same way [ggplot2::position_dodge()]/[ggplot2::position_dodge2()]
#'   do (based on the `fill`/`group` aesthetic), and each label shows that
#'   bar's own share of the panel-wide total rather than a share of its
#'   (non-existent) stack.
#' @param na.rm If FALSE, the default, missing values are removed with a
#'   warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics.
#' @param ... Other arguments passed on to [ggplot2::geom_text()], e.g.
#'   `colour` or `size`.
#'
#' @details Works the same way with [ggplot2::coord_flip()]: the labels are
#'   computed in data space (before the flip is applied), so `align = "top"`
#'   still means "furthest from zero", which renders past the end of a
#'   horizontal column once flipped.
#'
#'   Every edge-aligned label leaves a gap between itself and the column -
#'   scaled to the data's own range, so it looks proportionate whether the
#'   y-axis runs from 0 to 10 or 0 to 100,000 - and only one of them needs
#'   any extra space *beyond* the columns to do that: a single (non-stacked)
#'   column's `align = "top"` label genuinely floats outside the column, so
#'   a small amount of headroom is reserved automatically beyond the tallest
#'   column for it (this matters because theme61's default
#'   `scale_y_continuous_e61()` has no expansion at the data max/min).
#'   `"bottom"` on a single column, and both ends of a stacked column, sit
#'   just inside the column with their own gap nudged inward from the edge
#'   instead, so they need no reserved space beyond the column at all.
#'
#'   An explicit `scale_y_continuous_e61(limits = ...)` always takes
#'   precedence: the reserved top headroom and the single-column gap are
#'   capped at the supplied limit rather than nudging past it, since
#'   `scale_y_continuous_e61()` errors if data falls outside a limit you've
#'   set. If your limit sits exactly at (or inside) the data's own range,
#'   the label may end up flush against the edge again - widen the limit if
#'   you want the gap back.
#'
#' @return Object to add to a ggplot (via `+`).
#'
#' @examples
#' library(ggplot2)
#'
#' # Single columns: label shows each column's share of the total
#' df <- data.frame(grp = c("A", "B", "C"), value = c(10, 30, 60))
#'
#' ggplot(df, aes(grp, value)) +
#'   geom_col() +
#'   geom_col_label()
#'
#' # Stacked columns: label shows each segment's share of its column
#' df2 <- data.frame(
#'   x = rep(c("2023", "2024"), each = 2),
#'   grp = rep(c("Group 1", "Group 2"), 2),
#'   value = c(30, 70, 45, 55)
#' )
#'
#' ggplot(df2, aes(x, value, fill = grp)) +
#'   geom_col() +
#'   geom_col_label(align = "middle", colour = "white")
#'
#' # Works the same way flipped
#' ggplot(df, aes(grp, value)) +
#'   geom_col() +
#'   geom_col_label() +
#'   coord_flip()
#'
#' @export
geom_col_label <- function(mapping = NULL,
                           data = NULL,
                           ...,
                           accuracy = 1,
                           align = "top",
                           reverse = FALSE,
                           position = "stack",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  position <- match.arg(position, c("stack", "dodge", "dodge2"))
  align_num <- .resolve_col_label_align(align)
  edge_align <- align_num <= 0 || align_num >= 1

  layers <- list()

  # Build extra geom_text() params from ... once, so an explicit user vjust
  # (unusual, but possible) overwrites our default exactly once rather than
  # producing a duplicate named argument.
  dots <- list(...)
  base_params <- function(default_vjust) {
    params <- dots
    params$accuracy <- accuracy
    params$align <- align_num
    params$na.rm <- na.rm
    if (is.null(params$vjust)) params$vjust <- default_vjust
    params
  }

  if (position %in% c("dodge", "dodge2")) {

    # Mirror geom_col(position = "dodge"/"dodge2")'s own width-splitting so
    # labels land on the bar geom_col actually draws, instead of reimplementing
    # that logic - position_dodge()/position_dodge2() only move x (never y),
    # so StatColLabelDodge computes each label's own y/percentage directly,
    # unlike the stacked path below which leans on position_stack()'s vjust.
    dodge_position <- if (position == "dodge") {
      ggplot2::position_dodge(width = NULL)
    } else {
      ggplot2::position_dodge2(width = NULL, padding = 0.1)
    }

    layers[[1]] <- ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatColLabelDodge,
      geom = GeomTextFlipAware,
      position = dodge_position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = base_params(if (edge_align) 0 else 0.5)
    )

    layers[[length(layers) + 1]] <- ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatColLabelSpacer,
      geom = ggplot2::GeomBlank,
      position = "identity",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(align = align_num, na.rm = na.rm, position = position)
    )

    return(layers)
  }

  interior_params <- base_params(0.5)

  # Interior labels: always for stacked columns; for single columns only
  # when align is strictly between 0 and 1 (centred inside the bar at that
  # fraction of its height) - edge alignment on a single column is instead
  # handled by the "float" layer below, which can add a scale-relative gap
  # that position_stack() has no way to express for a lone (unstacked) row.
  layers[[1]] <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatColLabel,
    geom = GeomTextFlipAware,
    position = ggplot2::position_stack(vjust = align_num, reverse = reverse),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = interior_params
  )

  if (edge_align) {
    float_params <- base_params(0)

    layers[[2]] <- ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatColLabelFloat,
      geom = GeomTextFlipAware,
      # position_stack(), not "identity": for a lone (unstacked) row it's
      # trivial (ymin = 0, ymax = the nudged y set by the stat below), but
      # unlike "identity" it also carries the coord_flip()-aware handling
      # geom_text() needs to stay correctly positioned once flipped.
      position = ggplot2::position_stack(vjust = 1, reverse = reverse),
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = float_params
    )
  }

  # Invisible layer that reserves headroom at align = "top"/"bottom" so a
  # label sitting flush with the panel edge isn't clipped. Kept as its own
  # geom_blank() layer (rather than extra rows in a label layer above) so it
  # can't get caught up in position_stack() and change the real labels'
  # positions.
  layers[[length(layers) + 1]] <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatColLabelSpacer,
    geom = ggplot2::GeomBlank,
    position = "identity",
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(align = align_num, na.rm = na.rm, position = position)
  )

  layers
}

# Internal helpers ----

# geom_text(), but swaps hjust/vjust under coord_flip(). GeomText's own
# draw_panel() applies coord$transform() to (x, y) - correctly swapping the
# rendered position - but then passes hjust/vjust straight through
# unswapped, so they keep their pre-flip screen meaning (vjust still
# controls the screen-vertical axis, which post-flip is the *category*
# axis, not the value axis "top"/"bottom" are meant to move along). Without
# this, align = "top"/"bottom" on a coord_flip()'d chart renders the label
# centred on its anchor instead of offset from it.
GeomTextFlipAware <- ggplot2::ggproto("GeomTextFlipAware", ggplot2::GeomText,
  draw_panel = function(self, data, panel_params, coord, ...) {
    if (inherits(coord, "CoordFlip")) {
      tmp <- data$hjust
      data$hjust <- data$vjust
      data$vjust <- tmp
    }
    ggplot2::ggproto_parent(ggplot2::GeomText, self)$draw_panel(data, panel_params, coord, ...)
  }
)

# Fraction of the data's own (0, max) range used to space a floating label
# off the column it sits above/below, and to reserve headroom for it. Both
# expressed as a fraction of the range (rather than a fixed data unit or
# plot unit) so the gap looks proportionate whether the y-axis runs 0-10 or
# 0-100,000.
.COL_LABEL_GAP_FRAC <- 0.025
.COL_LABEL_HEADROOM_FRAC <- 0.08

# Rows sharing each x value (>1 = stacked column).
col_label_n_group <- function(data) {
  n_per_x <- tapply(data$y, data$x, length)
  as.numeric(n_per_x[match(data$x, names(n_per_x))])
}

col_label_percent <- function(y, total, accuracy) {
  scales::label_percent(accuracy = accuracy)(y / total)
}

.resolve_col_label_align <- function(align) {

  if (is.character(align)) {
    align <- match.arg(align, c("top", "middle", "bottom"))
    return(switch(align, top = 1, middle = 0.5, bottom = 0))
  }

  if (!is.numeric(align) || length(align) != 1 || is.na(align)) {
    stop('`align` must be "top", "middle", "bottom", or a single number between 0 and 1.')
  }

  min(max(align, 0), 1)
}

# Interior labels: stacked columns (always) and single columns at a
# fractional align (centred at that fraction of the bar's height, via
# position_stack()'s own vjust interpolation - unchanged and correct for
# this case). Edge-aligned single columns are excluded here; the "float"
# stat below handles those instead, so they aren't drawn twice.
StatColLabel <- ggplot2::ggproto("StatColLabel", ggplot2::Stat,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(label = ggplot2::after_stat(label)),

  compute_panel = function(data, scales, accuracy = 1, align = 1) {

    # Number of rows (segments) sharing each x: >1 means this x is stacked.
    data$n_group <- col_label_n_group(data)

    # Stacked columns: share of that x's stack. Single columns: share of the
    # panel-wide total (e.g. each category's share of an overall total).
    per_x_total <- tapply(data$y, data$x, sum, na.rm = TRUE)
    data$total <- ifelse(
      data$n_group > 1,
      as.numeric(per_x_total[match(data$x, names(per_x_total))]),
      sum(data$y, na.rm = TRUE)
    )

    data$label <- col_label_percent(data$y, data$total, accuracy)

    if (align <= 0 || align >= 1) {
      data <- data[data$n_group > 1, , drop = FALSE]
    }

    data
  },

  # Runs after position_stack() has placed every segment, so data$y here is
  # each segment's own actual rendered anchor (its own top for align =
  # "top", its own bottom for align = "bottom" - position_stack() already
  # interpolates per segment, not just for the outer one). Push every
  # segment's text fully inward from that anchor (vjust = 1 for "top",
  # 0 for "bottom") with a scale-relative gap, rather than centring it
  # (vjust = 0.5) flush on the boundary - matching the single-column
  # "float" layer's gap, and applied uniformly whether or not a given
  # segment's boundary happens to also be the panel edge. Nudging inward
  # (rather than the float layer's outward nudge) can't push data outside a
  # user-supplied scale_y_continuous_e61(limits = ...), so unlike that
  # layer this needs no clamping.
  finish_layer = function(self, data, params) {

    align <- params$align

    if (is.null(align) || (align > 0 && align < 1) || nrow(data) == 0) return(data)

    gap <- diff(range(c(0, data$y), na.rm = TRUE)) * .COL_LABEL_GAP_FRAC
    if (!is.finite(gap)) gap <- 0

    if (align >= 1) {
      data$y <- data$y - gap
      data$vjust <- 1
    } else {
      data$y <- data$y + gap
      data$vjust <- 0
    }

    data
  }
)

# Edge-aligned (align = "top"/"bottom") labels on single (non-stacked)
# columns. For a lone row, position_stack()'s ymin/ymax collapse to 0/y, so
# the layer's shared vjust = 0/1 alone can't express "y + a gap" or "a gap
# above 0" - there's no per-row way to vary it. Instead this stat nudges the
# row's own y to whatever value makes position_stack(vjust = 1) (set by the
# calling geom_col_label()) land the anchor exactly where it's wanted:
# y + gap for "top", or a constant gap above the base for "bottom". Both get
# a scale-relative gap this way, and - because position_stack() is still
# doing the positioning, unlike a hand-rolled position = "identity" - the
# usual coord_flip()-aware handling keeps working (position_stack() flips
# and un-flips the data around its own computation).
StatColLabelFloat <- ggplot2::ggproto("StatColLabelFloat", ggplot2::Stat,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(label = ggplot2::after_stat(label)),

  compute_panel = function(data, scales, accuracy = 1, align = 1) {

    n_group <- col_label_n_group(data)

    data <- data[n_group <= 1, , drop = FALSE]
    if (nrow(data) == 0) return(data)

    total <- sum(data$y, na.rm = TRUE)
    data$label <- col_label_percent(data$y, total, accuracy)

    gap <- diff(range(c(0, data$y), na.rm = TRUE)) * .COL_LABEL_GAP_FRAC
    if (!is.finite(gap)) gap <- 0

    # Respect explicit user limits (e.g. scale_y_continuous_e61(limits =
    # ...)) rather than nudging past them - scale_y_continuous_e61() errors
    # if trained data falls outside a user-supplied limit, and unlike the
    # spacer layer, this label's own position is real (rendered) data the
    # scale gets trained on.
    user_limits <- scales$y$limits
    if (length(user_limits) < 2) user_limits <- c(NA, NA)

    base <- min(0, min(data$y, na.rm = TRUE))

    if (align >= 1) {
      new_y <- data$y + gap
      if (!is.na(user_limits[2])) new_y <- pmin(new_y, user_limits[2])
    } else {
      new_y <- base + gap
      if (!is.na(user_limits[1])) new_y <- pmax(new_y, user_limits[1])
    }
    data$y <- new_y

    data
  }
)

# Dodged/grouped columns (position = "dodge"/"dodge2"): each bar is drawn
# independently, side by side, rather than stacked - so unlike StatColLabel
# each label's percentage is the bar's own share of the panel-wide total, and
# unlike StatColLabelFloat every bar needs this treatment, not just lone
# (unstacked) x values. x-positioning is left entirely to the caller's
# position_dodge()/position_dodge2() (passed as this layer's `position`),
# since those only move x and never touch y - so this stat computes the
# final y (and vjust) directly, the same way StatColLabelFloat does for a
# lone row, rather than leaning on position_stack()'s vjust interpolation
# (which position_dodge() has no equivalent for).
StatColLabelDodge <- ggplot2::ggproto("StatColLabelDodge", ggplot2::Stat,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(label = ggplot2::after_stat(label)),

  compute_panel = function(data, scales, accuracy = 1, align = 1) {

    # position_dodge()/position_dodge2() only look at xmin/xmax (or their own
    # `width` param) to find each bar's pre-dodge width, never a data$width
    # column - so without these they'd see xmin == xmax == x and collapse
    # every bar to the same spot. Same formula GeomBar$setup_data() uses, so
    # bars and labels dodge into the same slots.
    if (is.null(data$xmin) && is.null(data$xmax)) {
      width <- ggplot2::resolution(data$x, zero = FALSE) * 0.9
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
    }

    total <- sum(data$y, na.rm = TRUE)
    data$label <- col_label_percent(data$y, total, accuracy)

    # Interior (0 < align < 1): centre the label at that fraction of the
    # bar's own height - the direct equivalent of position_stack(vjust =
    # align)'s interpolation for a bar that isn't part of a stack.
    if (align > 0 && align < 1) {
      data$y <- data$y * align
      return(data)
    }

    gap <- diff(range(c(0, data$y), na.rm = TRUE)) * .COL_LABEL_GAP_FRAC
    if (!is.finite(gap)) gap <- 0

    user_limits <- scales$y$limits
    if (length(user_limits) < 2) user_limits <- c(NA, NA)

    if (align >= 1) {
      new_y <- data$y + gap
      if (!is.na(user_limits[2])) new_y <- pmin(new_y, user_limits[2])
    } else {
      base <- min(0, min(data$y, na.rm = TRUE))
      new_y <- base + gap
      if (!is.na(user_limits[1])) new_y <- pmax(new_y, user_limits[1])
    }
    data$y <- new_y

    data
  }
)

# Reserves headroom for single (non-stacked) columns' floating "top" label,
# via an invisible geom_blank() layer whose (x, y) still counts towards the
# y scale's trained range. This is the only remaining case that needs it:
# the "top" float layer's label genuinely sits outside the column. Every
# other edge-aligned label (single "bottom", and both ends of a stacked
# column via StatColLabel's finish_layer) is pushed fully inside its column
# instead of straddling the boundary, so none of them need extra room.
# Interior labels (align strictly between 0 and 1) don't touch an edge
# either way.
StatColLabelSpacer <- ggplot2::ggproto("StatColLabelSpacer", ggplot2::Stat,
  required_aes = c("x", "y"),

  compute_panel = function(data, scales, align = 1, position = "stack") {

    if (align < 1) return(data[0, , drop = FALSE])

    if (identical(position, "stack")) {
      n_per_x <- tapply(data$y, data$x, length)
      single <- n_per_x <= 1
      if (!any(single)) return(data[0, , drop = FALSE])

      per_x_total <- tapply(data$y, data$x, sum, na.rm = TRUE)
      single_totals <- per_x_total[single]

      pad <- diff(range(c(0, per_x_total), na.rm = TRUE)) * .COL_LABEL_HEADROOM_FRAC
      if (!is.finite(pad) || pad == 0) pad <- 1

      padded <- max(single_totals, na.rm = TRUE) + pad
    } else {
      # Dodged bars each float above their own top individually (there's no
      # stack to sum), so headroom only needs to clear the tallest bar.
      pad <- diff(range(c(0, data$y), na.rm = TRUE)) * .COL_LABEL_HEADROOM_FRAC
      if (!is.finite(pad) || pad == 0) pad <- 1

      padded <- max(data$y, na.rm = TRUE) + pad
    }

    # Respect an explicit user limit (e.g. scale_y_continuous_e61(limits =
    # ...)) rather than padding past it - scale_y_continuous_e61() errors if
    # trained data falls outside a user-supplied limit.
    user_limits <- scales$y$limits
    if (length(user_limits) >= 2 && !is.na(user_limits[2])) {
      padded <- min(padded, user_limits[2])
    }

    # A single extra point is enough to widen the panel's trained y range;
    # reuse an existing x so no spurious category is introduced.
    spacer <- data[1, , drop = FALSE]
    spacer$y <- padded

    spacer
  }
)
