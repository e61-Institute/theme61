prepare_plot <- function(plot,
                         chart_type = NULL,
                         auto_scale = TRUE,
                         base_size = 10,
                         bg_colour = "white",
                         ...) {
  UseMethod("prepare_plot")
}

#' @export
prepare_plot.e61_plot <- function(plot,
                                  chart_type = NULL,
                                  auto_scale = TRUE,
                                  base_size = 10,
                                  bg_colour = "white",
                                  ...) {

  if (is.null(chart_type)) chart_type <- "normal"

  # Plot is assumed already classified (map vs non-map) and to have had its
  # theme_e61() spec realised into a real theme by finalise_e61_plot(), which
  # save_e61() runs on every plot before dispatching here.

  # Text sizing/margins/legend-position are handled by save_single() (via
  # resolve_text_size() + update_margins()), so they're not duplicated here.

  # Background fill
  plot <- plot + ggplot2::theme(rect = ggplot2::element_rect(fill = bg_colour))

  list(plot = plot, chart_type = chart_type, auto_scale = auto_scale)
}

#' @export
prepare_plot.e61_map <- function(plot,
                                 chart_type = NULL,
                                 auto_scale = TRUE,
                                 base_size = 10,
                                 bg_colour = "white",
                                 ...) {

  # maps: force settings
  auto_scale <- FALSE
  chart_type <- "custom"

  # Plot is assumed already classified (map vs non-map) and to have had its
  # theme_e61() spec realised into a real theme by finalise_e61_plot(), which
  # save_e61() runs on every plot before dispatching here.

  plot <- plot + ggplot2::theme(rect = ggplot2::element_rect(fill = bg_colour))

  list(plot = plot, chart_type = chart_type, auto_scale = auto_scale)
}

#' Method that fails for non-plots
#' @export
prepare_plot.default <- function(x, ...) {
  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be prepared into a e61 plot."
  )
}
