#' Highlight one or more groups in a colour or fill scale
#'
#' @description Colours the specified group(s) using the e61 palette and
#'   greys out every other level of the mapped `colour`/`fill` variable. This
#'   is useful for drawing attention to one series (or a handful of series)
#'   in a chart without needing a legend, in keeping with the e61 house style
#'   of labelling directly on the graph (see [plot_label()]).
#'
#' @param highlight Character (or coercible) vector. The value(s) of the
#'   mapped `colour`/`fill` variable to highlight in colour. Every other
#'   level present in the data is greyed out.
#' @param unhighlighted Character. Hex code or e61 colour to use for the
#'   non-highlighted levels. Defaults to `e61_greylight3`.
#' @inheritDotParams ggplot2::scale_colour_manual
#'
#' @return Object to add to a ggplot (via `+`).
#' @rdname scale_e61_highlight
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   year = rep(2020:2023, 3),
#'   region = rep(c("NSW", "VIC", "QLD"), each = 4),
#'   value = c(4, 5, 6, 7, 3, 3, 4, 4, 2, 2, 3, 3)
#' )
#'
#' ggplot(df, aes(year, value, colour = region)) +
#'   geom_line() +
#'   scale_colour_e61_highlight(highlight = "NSW")
scale_colour_e61_highlight <- function(highlight, unhighlighted = e61_greylight3, ...) {

  structure(
    list(highlight = as.character(highlight),
         unhighlighted = unhighlighted,
         aesthetics = "colour",
         dots = list(...)),
    class = "e61_scale_highlight"
  )
}

#' @rdname scale_e61_highlight
#' @export
scale_fill_e61_highlight <- function(highlight, unhighlighted = e61_greylight3, ...) {

  structure(
    list(highlight = as.character(highlight),
         unhighlighted = unhighlighted,
         aesthetics = "fill",
         dots = list(...)),
    class = "e61_scale_highlight"
  )
}

# Internal helpers ----

.build_highlight_scale <- function(object, plot) {

  if (length(object$highlight) == 0) {
    stop("`highlight` must contain at least one value.")
  }

  aes_name <- object$aesthetics

  info <- find_aes(plot, aes_name)

  if (is.null(info)) {
    stop(
      "No `", aes_name, "` aesthetic is mapped; scale_", aes_name,
      "_e61_highlight() needs a mapped ", aes_name,
      " to know which levels to grey out."
    )
  }

  val <- safe_eval_tidy(info$quo, data = info$data)
  levels_all <- if (is.factor(val)) levels(val) else sort(unique(as.character(val)))

  values <- rep(object$unhighlighted, length(levels_all))
  names(values) <- levels_all

  highlight <- object$highlight
  pal <- palette_e61(length(highlight))
  matched <- intersect(highlight, levels_all)

  if (length(matched) == 0) {
    warning(
      "None of the values supplied to `highlight` were found in the ",
      aes_name, " data. All levels have been greyed out."
    )
  }

  values[matched] <- pal[match(matched, highlight)]

  scale_fn <- if (aes_name == "colour") ggplot2::scale_colour_manual else ggplot2::scale_fill_manual

  retval <- do.call(scale_fn, c(list(values = values), object$dots))

  class(retval) <- c(class(retval), "scale_col_e61")

  retval
}

# New methods ----

# ggplot2 v4+ hook
#' @exportS3Method ggplot2::update_ggplot
update_ggplot.e61_scale_highlight <- function(object, plot, ...) {
  plot + .build_highlight_scale(object, plot)
}

# Back-compat hook (still used by ggplot2 add_ggplot path)
#' @export
ggplot_add.e61_scale_highlight <- function(object, plot, object_name, ...) {
  update_ggplot.e61_scale_highlight(object, plot, ...)
}
