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
#'   rather than sitting flush against the column; any other value centres
#'   the label inside the column at that fraction of its height.
#' @param reverse Logical. Reverse the stacking order used to position labels
#'   within stacked columns. Set this to match `position_stack(reverse =
#'   TRUE)` if you used that for your `geom_col()`. Defaults to FALSE.
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
#'   At `align = "top"`/`"bottom"`, a small amount of headroom is reserved
#'   automatically beyond the tallest/lowest column (or, for stacked columns,
#'   beyond the tallest/lowest stack total) so the label isn't clipped by the
#'   panel edge - this matters because theme61's default
#'   `scale_y_continuous_e61()` has no expansion at the data max/min. An
#'   explicit `scale_y_continuous_e61(limits = ...)` takes precedence over
#'   this reserved headroom.
#'
#'   For single (non-stacked) columns specifically, `align = "top"`/
#'   `"bottom"` also leaves a small gap between the column and its label -
#'   `"top"` floats just clear of the column, `"bottom"` sits just inside it
#'   above the base - rather than the label sitting flush against the
#'   column's edge. Both the gap and the reserved headroom scale with the
#'   data's own range, so they look proportionate whether the y-axis runs
#'   from 0 to 10 or 0 to 100,000.
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
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  align_num <- .resolve_col_label_align(align)
  edge_align <- align_num <= 0 || align_num >= 1

  layers <- list()

  # Build extra geom_text() params from ... once, so an explicit user vjust
  # (unusual, but possible) overwrites our default exactly once rather than
  # producing a duplicate named argument.
  interior_params <- list(...)
  interior_params$accuracy <- accuracy
  interior_params$align <- align_num
  interior_params$na.rm <- na.rm
  if (is.null(interior_params$vjust)) interior_params$vjust <- 0.5

  # Interior labels: always for stacked columns; for single columns only
  # when align is strictly between 0 and 1 (centred inside the bar at that
  # fraction of its height) - edge alignment on a single column is instead
  # handled by the "float" layer below, which can add a scale-relative gap
  # that position_stack() has no way to express for a lone (unstacked) row.
  layers[[1]] <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatColLabel,
    geom = ggplot2::GeomText,
    position = ggplot2::position_stack(vjust = align_num, reverse = reverse),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = interior_params
  )

  if (edge_align) {
    float_params <- list(...)
    float_params$accuracy <- accuracy
    float_params$align <- align_num
    float_params$na.rm <- na.rm
    if (is.null(float_params$vjust)) float_params$vjust <- 0

    layers[[2]] <- ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatColLabelFloat,
      geom = ggplot2::GeomText,
      position = "identity",
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
    params = list(align = align_num, na.rm = na.rm)
  )

  layers
}

# Internal helpers ----

# Fraction of the data's own (0, max) range used to space a floating label
# off the column it sits above/below, and to reserve headroom for it. Both
# expressed as a fraction of the range (rather than a fixed data unit or
# plot unit) so the gap looks proportionate whether the y-axis runs 0-10 or
# 0-100,000.
.COL_LABEL_GAP_FRAC <- 0.025
.COL_LABEL_HEADROOM_FRAC <- 0.08

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
    n_per_x <- tapply(data$y, data$x, length)
    data$n_group <- as.numeric(n_per_x[match(data$x, names(n_per_x))])

    # Stacked columns: share of that x's stack. Single columns: share of the
    # panel-wide total (e.g. each category's share of an overall total).
    per_x_total <- tapply(data$y, data$x, sum, na.rm = TRUE)
    data$total <- ifelse(
      data$n_group > 1,
      as.numeric(per_x_total[match(data$x, names(per_x_total))]),
      sum(data$y, na.rm = TRUE)
    )

    data$label <- scales::label_percent(accuracy = accuracy)(data$y / data$total)

    if (align <= 0 || align >= 1) {
      data <- data[data$n_group > 1, , drop = FALSE]
    }

    data
  }
)

# Edge-aligned (align = "top"/"bottom") labels on single (non-stacked)
# columns. Computed directly rather than via position_stack(), because for a
# lone row position_stack()'s ymin/ymax collapse to 0/y - there's no vjust
# that can express "y + a gap" or "a gap above 0" from that alone. Both ends
# get a symmetric scale-relative gap: "top" floats just outside the column,
# "bottom" sits just inside it above the base, rather than either sitting
# flush against the column's edge.
StatColLabelFloat <- ggplot2::ggproto("StatColLabelFloat", ggplot2::Stat,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(label = ggplot2::after_stat(label)),

  compute_panel = function(data, scales, accuracy = 1, align = 1) {

    n_per_x <- tapply(data$y, data$x, length)
    n_group <- as.numeric(n_per_x[match(data$x, names(n_per_x))])

    data <- data[n_group <= 1, , drop = FALSE]
    if (nrow(data) == 0) return(data)

    total <- sum(data$y, na.rm = TRUE)
    data$label <- scales::label_percent(accuracy = accuracy)(data$y / total)

    gap <- diff(range(c(0, data$y), na.rm = TRUE)) * .COL_LABEL_GAP_FRAC
    if (!is.finite(gap)) gap <- 0

    base <- min(0, min(data$y, na.rm = TRUE))
    data$y <- if (align >= 1) data$y + gap else base + gap

    data
  }
)

# Reserves headroom for geom_col_label()'s floating/edge-aligned labels, via
# an invisible geom_blank() layer whose (x, y) still counts towards the y
# scale's trained range. Top headroom is needed whenever align = "top",
# whether columns are single (the label floats just outside the column) or
# stacked (the topmost segment's label straddles the panel's top edge, since
# it's drawn with vjust = 0.5 there). Bottom headroom is only needed for
# stacked columns at align = "bottom" - single columns' "bottom" label now
# sits inside the column, well clear of the panel's bottom edge. Interior
# labels (align strictly between 0 and 1) never touch a panel edge, so no
# room is reserved for those.
StatColLabelSpacer <- ggplot2::ggproto("StatColLabelSpacer", ggplot2::Stat,
  required_aes = c("x", "y"),

  compute_panel = function(data, scales, align = 1) {

    if (align > 0 && align < 1) return(data[0, , drop = FALSE])

    per_x_total <- tapply(data$y, data$x, sum, na.rm = TRUE)
    n_per_x <- tapply(data$y, data$x, length)

    pad <- diff(range(c(0, per_x_total), na.rm = TRUE)) * .COL_LABEL_HEADROOM_FRAC
    if (!is.finite(pad) || pad == 0) pad <- 1

    # Respect explicit user limits (e.g. scale_y_continuous_e61(limits =
    # ...)) rather than padding past them - scale_y_continuous_e61() errors
    # if trained data falls outside a user-supplied limit.
    user_limits <- scales$y$limits
    if (length(user_limits) < 2) user_limits <- c(NA, NA)

    # A single extra point is enough to widen the panel's trained y range;
    # reuse an existing x so no spurious category is introduced.
    spacer <- data[1, , drop = FALSE]

    if (align >= 1) {
      padded <- max(per_x_total, na.rm = TRUE) + pad
      if (!is.na(user_limits[2])) padded <- min(padded, user_limits[2])
      spacer$y <- padded
    } else {
      if (!any(n_per_x > 1)) return(data[0, , drop = FALSE])
      padded <- min(0, min(data$y, na.rm = TRUE)) - pad
      if (!is.na(user_limits[1])) padded <- max(padded, user_limits[1])
      spacer$y <- padded
    }

    spacer
  }
)
