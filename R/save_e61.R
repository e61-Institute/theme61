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
#' @details Setting the correct height and width parameters is quite difficult
#'   due to the way that ggplot translates ggplot objects from pixels into
#'   physical dimensions (inches or centimetres). Font sizes are also
#'   transformed differently to other graph elements which adds another
#'   dimension of difficulty. This is why \code{save_e61} generally requires you
#'   to provide your own height and width arguments after some trial and error,
#'   and produces loud messages if you stick to the defaults, which I have tried
#'   to make not terrible.
#'
#'   If the width/height arguments are off, then the file you output will have
#'   excess space on the left/right or top/bottom (if the values are too high),
#'   or the graph itself will be shrunk and look weird (if the values are too
#'   low).
#'
#'   This function tries to support multi-panel graphs generated using
#'   \code{mpanel_e61} by doubling the default width to 17, but you will need to
#'   make adjustments to the dimensions to ensure the graph is sized
#'   appropriately.
#' @param filename File name to create on disk. Providing the file format
#'   extension (e.g. .svg) is optional. The file extension must be lowercase. If
#'   you want to save multiple files with different formats, see the
#'   \code{format} argument for details.
#' @param plot Plot object to save. Defaults to the last plot displayed so
#'   usually you do not need to provide this explicitly.
#' @param chart_type Type of chart. This is used to set sensible chart widths
#'   based on the width of text in each document. Options include 'MN' (
#'   for micronote charts), 'RN' (research notes), 'PPT' (powerpoints).
#'   research note),'PPT
#' @param auto_scale Logical. Should the y-axis be scaled automatically. Default is TRUE.
#' @param width Plot width in cm. Defaults to NULL which means the width will
#'   be set based on the chart type.
#' @param height Plot height in cm. If you do not specify a height, the function
#'   will calculate an appropriate height based on the labels you have provided.
#' @param max_height The maximum height of your plot. This is used to constrain
#'   the plot resizing algorithm in cases where you want to limit the height of
#'   your charts.
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
#' @param test For development use only.
#' @param ... (Mpanel specific) Plot objects to put on the panel.
#' @param plotlist (Mpanel specific) List of plots to combine as an mpanel and save. You can also
#'   enter the charts individually as arguments to the function.
#' @param title_adj (Mpanel specific) Rescales the size of the title text to be slightly larger
#'   than the titles of the subplots (default is 1.1). 2 doubles the font size.
#' @param height_adj (Mpanel specific) Rescales the height of the mpanel. The function sets sensible
#'   defaults but this provides you with manial control if you need it.
#' @param base_size (Mpanel specific) Numeric. Chart font size. Default is 8.
#' @param title_spacing_adj (Mpanel specific) Rescales the size of the space give to the mpanel
#'   title. Use if you think the title looks too cramped on the chart.
#' @param subtitle_spacing_adj (Mpanel specific) Rescales the size of the space give to the mpanel
#'   subtitle. Use if you think the subtitle looks too cramped on the chart.
#' @param rel_heights (Mpanel specific) A numeric vector giving the relative proportions of each
#'   graph component (title, plots, footer (optional)).
#' @return ggplot2 object
#' @inheritParams labs_e61
#' @inheritParams cowplot::plot_grid
#' @inheritParams grDevices::png
#' @return Invisibly returns the file name.
#' @export

save_e61 <- function(filename,
                     ..., # specific for mpanel plots
                     plot = ggplot2::last_plot(),
                     chart_type = "MN",
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
                     title_adj = 1,
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

  # Check whether to save an mpanel or a single planel chart - these require different approaches
  plots <- c(list(...), plotlist)

  # check whether the plots are ggplot2 objects
  plots <- check_plots(plots)

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
      title_adj = title_adj,
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

#' Check plots are ggplot objects and return a list of only ggplot objects
#' @noRd
check_plots <- function(plots){

  temp_list <- list()

  for(i in seq_along(plots)){
    temp_plot <- plots[[i]]

    if(is.ggplot(temp_plot)) {
      temp_list[[length(temp_list) + 1]] <- temp_plot
    } else {
      warning("Some elements of the plotlist are not ggplot objects. Check you have not supplied the wrong object or used an incorrect argument.")
    }
  }

  return(temp_list)
}

#' Get the correct plot width, base text size and maximum height based on the chart type
#' @noRd
get_plot_width <- function(chart_type, max_height){

  # Set the maximum width based on the type of outputs
  if(chart_type == "MN"){

    max_width <- 18.59 # based on 215.9mm page width and 15mm margins either side

  } else if(chart_type == "RN"){

    max_width <- 13.985 # based on 338.7mm page width, 20mm margins, 15mm column sep and 2 columns (i.e. divide the remainder by 2)

  } else if(chart_type == "PPT"){

    max_height <- 13.25
    max_width <- 31.32

  } else if(is.null(chart_type)){

    max_width <- 20

  } else {
    stop("Invalid chart type. Please select from one of the following: 'MN' for micronotes, 'RN' for research notes, 'PPT' for powerpoint slides, or leave blank to use default maximum widths")
  }

  return(list("max_width" = max_width, "max_height" = max_height))
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

#' Counts the number of occurrences of a line break (\n)
#'
#' @param text The string to be parsed.
#' @return Integer counting the number of line breaks in the string.
#' @noRd
n_count <- function(text) {
  nchar(text) - nchar(gsub("\n", "", text, fixed = TRUE))
}
