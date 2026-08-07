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
#'   columns, `"top"` places the label just above the column; any other value
#'   places it inside the column at that fraction of its height.
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

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatColLabel,
    geom = ggplot2::GeomText,
    position = ggplot2::position_stack(vjust = align_num, reverse = reverse),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      accuracy = accuracy,
      align = align_num,
      na.rm = na.rm,
      ...
    )
  )
}

# Internal helpers ----

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

StatColLabel <- ggplot2::ggproto("StatColLabel", ggplot2::Stat,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(label = ggplot2::after_stat(label),
                             vjust = ggplot2::after_stat(t61_vjust)),

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

    # Single (non-stacked) columns at "top" alignment: nudge the label above
    # the column instead of centring it on the (single) segment's edge.
    data$t61_vjust <- ifelse(data$n_group > 1, 0.5,
                       ifelse(align >= 1, 0,
                       ifelse(align <= 0, 1, 0.5)))

    data
  }
)
