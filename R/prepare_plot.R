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

  # Existing sizing/margins logic for non-map plots (moved from save_single)
  legendTitle <- plot@theme$legend.title
  legendPosition <- plot@theme$legend.position

  plot <- plot + ggplot2::theme(text = ggplot2::element_text(size = base_size))
  plot <- plot + update_margins(base_size = base_size, legend_title = legendTitle)

  if (!is.null(legendPosition)) {
    plot <- plot + ggplot2::theme(legend.position = legendPosition)
  }

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
