#' Saves ggplot graphs with sensible defaults
#'
#' @description Designed to save ggplot graphs made with the e61 theme with
#'   sensible defaults that ensure the text size is appropriately proportioned
#'   given default sizing.
#'
#'   Currently the only file formats supported are \code{.pdf} or \code{.svg}
#'   (preferred), and \code{.png}. PDF and SVG are modern vector graphics file
#'   formats which can be scaled up and down in size without blurring or
#'   becoming pixelated. Use the PNG file format in the rare case that vector
#'   graphics are not supported.
#'
#'   See \code{\link[ggplot2]{ggsave}} for details on custom function arguments.
#'
#' @param filename File name to create on disk. Providing the file format
#'   extension (e.g. .svg) is optional. The file extension must be lowercase. If
#'   you want to save multiple files with different formats, see the
#'   \code{format} argument for details.
#' @param plot Plot object to save. Defaults to the last plot displayed so
#'   usually you do not need to provide this explicitly.
#' @param ... (multi-panel specific) Plot objects to put on the panel.
#' @param chart_type String. Type of chart. This is used to set sensible chart
#'   widths based on the width of text in each document. Options are 'MN' ( for
#'   micronotes), 'RN' (research notes), 'PPT' (PowerPoints).
#' @param auto_scale Logical. Should the y-axis be scaled automatically. Default
#'   is TRUE.
#' @param width Numeric. Plot width in cm. Defaults to NULL which means the
#'   width will be set based on the chart type.
#' @param height Numeric. Plot height in cm. If you do not specify a height, the
#'   function will calculate an appropriate height.
#' @param max_height Numeric. The maximum height of your plot in cm. This is
#'   used to constrain the plot resizing algorithm in cases where you want to
#'   limit the height of your charts.
#' @param format An optional vector of file formats to save as. For example
#'   \code{c("svg", "pdf")} will save 2 files with the same name to the same
#'   location to SVG and PDF formats. If the file format is specified in
#'   \code{filename}, then this argument is ignored.
#' @param save_data Logical. Set to TRUE if you want to save a .csv with the
#'   same name as the graph that contains the data needed to recreate the graph
#'   (defaults to FALSE).
#' @param resize Numeric. Only used when PNG is the file format. Resize the
#'   graph width and height. You may also need to adjust the \code{pointsize}
#'   and \code{res} to ensure the text is readable.
#' @param plotlist (multi-panel specific) List of plots to combine as an
#'   multi-panel and save. You can also enter the charts individually as
#'   arguments to the function.
#' @param title_adj (multi-panel specific) Rescales the size of the title text
#'   to be slightly larger than the titles of the subplots (default is 1.1). 2
#'   doubles the font size.
#' @param height_adj (multi-panel specific) Rescales the height of the
#'   multi-panel. The function sets sensible defaults but this provides you with
#'   manual control if you need it.
#' @param base_size (multi-panel specific) Numeric. Chart font size. Default is
#'   8.
#' @param title_spacing_adj (multi-panel specific) Rescales the size of the
#'   space give to the multi-panel title. Use if you think the title looks too
#'   cramped on the chart.
#' @param subtitle_spacing_adj (multi-panel specific) Rescales the size of the
#'   space give to the multi-panel subtitle. Use if you think the subtitle looks
#'   too cramped on the chart.
#' @param rel_heights (multi-panel specific) A numeric vector giving the
#'   relative proportions of each graph component (title, plots, footer
#'   (optional)).
#' @param test For development use only.
#' @return ggplot2 object
#' @inheritParams labs_e61
#' @inheritParams cowplot::plot_grid
#' @inheritParams grDevices::png
#' @return Invisibly returns the file name.
#' @export

save_e61 <- function(filename,
                     ..., # specific for mpanel plots
                     plot = ggplot2::last_plot(),
                     chart_type = c("MN", "RN", "PPT"),
                     auto_scale = TRUE, # manual control over whether y-axis is scaled
                     width = NULL, # manual control over the width of the chart
                     height = NULL, # manual control over the height of the chart
                     max_height = NULL, # manual control over the maximum height of the chart
                     format = c("svg", "pdf", "eps", "png"),
                     save_data = FALSE,
                     resize = NULL,
                     pointsize = 12,
                     res = 72,
                     test = !isTRUE(getOption("test_save")),
                     # mpanel specific arguments
                     plotlist = NULL,
                     title = NULL,
                     subtitle = NULL,
                     footnotes = NULL,
                     sources = NULL,
                     title_spacing_adj = 1, # adjust the amount of space given to the title
                     subtitle_spacing_adj = 1, # adjust the amount of space given to the subtitle
                     base_size = 10, # set the base size for the theme61 font size call
                     height_adj = NULL, # adjust the vertical spacing of the mpanel charts
                     ncol = 2,
                     nrow = NULL,
                     align = c("v", "none", "h", "hv"),
                     axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
                     rel_heights = NULL
) {

  plots <- c(list(...), plotlist)

  # check whether the plots are ggplot2 objects
  plots <- check_plots(plots)

  # Enforce chart type
  chart_type <- match.arg(chart_type)

  # Check whether to save an mpanel or a single planel chart - these require
  # different approaches
  if(length(plots) > 1) {
    save_multi(
      filename,
      format = format,
      plotlist = plots,
      chart_type = chart_type,
      title = title,
      subtitle = subtitle,
      footnotes = footnotes,
      sources = sources,
      width = width, # manual control over the width of the chart
      height = height, # manual control over the height of the chart
      max_height = max_height, # manual control over the maximum height of the chart
      auto_scale = auto_scale,
      title_spacing_adj = title_spacing_adj, # adjust the amount of space given to the title
      subtitle_spacing_adj = subtitle_spacing_adj, # adjust the amount of space given to the subtitle
      height_adj = height_adj, # adjust the vertical spacing of the mpanel charts
      base_size = base_size,
      ncol = ncol,
      nrow = nrow,
      align = align,
      axis = axis,
      rel_heights = rel_heights,
      pointsize = pointsize,
      res = res
    )

  } else {

    save_single(
      filename = filename,
      plot = plot,
      chart_type = chart_type,
      auto_scale = auto_scale, # manual control over whether y-axis is scaled
      width = width, # manual control over the width of the chart
      height = height, # manual control over the height of the chart
      max_height = max_height, # manual control over the maximum height of the chart
      format = format,
      base_size = base_size,
      save_data = save_data,
      resize = resize,
      pointsize = pointsize,
      res = res,
      test = test
    )
  }
}

#' Set option to automatically open files created by \code{save_e61}
#'
#' These functions set and unset a session-wide option to automatically open
#' files created by \code{save_e61}. This is useful when you want to look at the
#' graph you have just created, such as when you are trying to figure out label
#' locations or graph dimensions and don't want to manually navigate to the file
#' location every time.
#'
#' @return This function is used for its side effects.
#' @rdname set_open_graph
#' @export
set_open_graph <- function() {
  options(open_e61_graph = TRUE)

  invisible(TRUE)
}

#' @rdname set_open_graph
#' @export
unset_open_graph <- function() {
  options(open_e61_graph = FALSE)

  invisible(FALSE)
}
