#' @include s7_classes.R

.update_ggplot_e61_theme_spec <- function(object, plot, ...) {
  if (!inherits(plot, "ggplot")) {
    stop("Can't add theme_e61() spec to a non-ggplot object.", call. = FALSE)
  }

  attr(plot, "e61_theme_spec") <- object
  plot
}
