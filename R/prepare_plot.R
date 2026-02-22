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

  # Auto-inject default theme spec only if user didn't add one
  if (isTRUE(getOption("theme61.auto_theme", TRUE))) {
    if (is.null(attr(plot, "e61_theme_spec", exact = TRUE))) {
      attr(plot, "e61_theme_spec") <- theme_e61()
    }
  }

  # Realise spec (after classification)
  plot <- realise_e61_theme(plot)

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

  if (isTRUE(getOption("theme61.auto_theme", TRUE))) {
    if (is.null(attr(plot, "e61_theme_spec", exact = TRUE))) {
      attr(plot, "e61_theme_spec") <- theme_e61()
    }
  }

  plot <- realise_e61_theme(plot)

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
