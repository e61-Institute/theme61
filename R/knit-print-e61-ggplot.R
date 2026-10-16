#' Knit print method for theme61 plots
#'
#' Plots shown in knitr/Quarto documents never go through [save_e61()], so
#' knitr would otherwise draw them on its own device at the chunk's
#' `fig.width` (7in by default) - about twice the width `save_e61()` lays a
#' chart out for, which leaves the text and axes far too small relative to the
#' panel. Instead, the chart is laid out exactly as `save_e61()` would, written
#' to the chunk's figure directory and embedded at its saved size.
#'
#' The file is SVG for HTML output, PDF for LaTeX/PDF output and PNG for
#' everything else (e.g. Word). The chunk's `out.width` still controls the
#' displayed size (e.g. `out.width = "100%"` to fill the page); `fig.width`
#' and `fig.height` are ignored. Set the `theme61.knit_as_saved` option to
#' FALSE to go back to knitr's default rendering.
#'
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.e61_plot <- function(x, options = knitr::opts_current$get(), ...) {

  if (isTRUE(getOption("theme61.iterate_mode", FALSE)) ||
      isFALSE(getOption("theme61.knit_as_saved", TRUE))) {
    return(NextMethod())
  }

  fmt <- if (knitr::is_latex_output()) "pdf" else if (knitr::is_html_output()) "svg" else "png"

  # Mirrors save_e61()'s single-panel defaults.
  plot <- finalise_e61_plot(as_e61_plot(x))
  is_map <- inherits(plot, "e61_map")
  plot <- plot + theme(rect = element_rect(fill = "white"))

  # Messages raised here (e.g. autolabel fallbacks) would otherwise be
  # captured into the rendered document.
  saved <- suppressMessages(
    save_single(filename = NULL, plot = plot, chart_type = if (is_map) "custom" else "normal",
                auto_scale = !is_map && t61_preview_auto_scale(x),
                width = NULL, height = NULL, max_height = NULL, format = fmt,
                base_size = 10, pad_width = 0, pad_height = 0, bg_colour = "white")
  )

  tmp <- tempfile()
  on.exit(unlink(paste0(tmp, ".", fmt)), add = TRUE)
  suppressMessages(
    save_graph(graph = saved$graph, format = fmt, filename = tmp,
               width = saved$width, height = saved$height, bg_colour = "white", res = 1)
  )
  path <- t61_knit_fig_path(paste0(tmp, ".", fmt), options)

  ggplot2::set_last_plot(x)

  # PNGs are rasterised at a multiple of their physical size, and pandoc would
  # otherwise size them by pixel count. save_graph() pads the width by 0.1cm.
  if (is.null(options$out.width)) {
    options$out.width <- sprintf("%.3fin", cm_to_in(saved$width + 0.1))
  }

  knitr::knit_print(knitr::include_graphics(path), options = options, ...)
}

#' Copy a rendered chart into the chunk's figure directory. Named by content
#' hash so several charts in one chunk don't overwrite each other, and the
#' suffix keeps it clear of the files knitr records itself.
#' @noRd
t61_knit_fig_path <- function(file, options) {
  hash <- substr(unname(tools::md5sum(file)), 1, 8)
  options$label <- paste0(options$label %||% "unnamed-chunk", "-e61")

  path <- knitr::fig_path(paste0(".", tools::file_ext(file)), options, number = hash)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file.copy(file, path, overwrite = TRUE)

  path
}
