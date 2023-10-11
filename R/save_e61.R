#' Saves ggplot graphs with sensible defaults
#'
#' @description Designed to save ggplot graphs made with the e61 theme with
#'   sensible defaults that ensure the text size is appropriately proportioned
#'   given default sizing.
#'
#'   Currently the only file formats supported are \code{.svg} (preferred),
#'   \code{.pdf} or \code{.eps}. PDF and SVG are modern vector graphics file
#'   formats which can be scaled up and down in size without blurring or
#'   becoming pixelated.
#'
#'   See \code{\link[ggplot2]{ggsave}} for details on custom function arguments.
#'
#' @param filename File name to create on disk. Providing the file format
#'   extension (e.g. .svg) is optional. The file extension must be lowercase. If
#'   you want to save multiple files with different formats, see the
#'   \code{format} argument for details.
#' @param plot Single-panel plot object to save. Defaults to the last plot
#'   displayed so usually you do not need to provide this explicitly.
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
#' @param base_size Numeric. Chart font size. Default is 10.
#' @param ... (multi-panel specific) Plot objects to put on the panel.
#' @param plotlist (multi-panel specific) List of plots to combine as an
#'   multi-panel and save. You can also enter the charts individually as
#'   arguments to the function.
#' @param height_adj (multi-panel specific) Rescales the height of the
#'   multi-panel. The function sets sensible defaults but this provides you with
#'   manual control if you need it.
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
#' @return Invisibly returns the file name.
#' @export

save_e61 <- function(filename,
                     ...,
                     plot = last_plot(),
                     format = c("svg", "pdf", "eps"),
                     chart_type = c("MN", "RN", "PPT"),
                     auto_scale = TRUE, # manual control over whether y-axis is scaled
                     width = NULL, # manual control over the width of the chart
                     height = NULL, # manual control over the height of the chart
                     max_height = NULL, # manual control over the maximum height of the chart
                     save_data = FALSE,
                     base_size = 10, # set the base size for the theme61 font size call
                     # multi-panel specific arguments
                     plotlist = NULL,
                     title = NULL,
                     subtitle = NULL,
                     footnotes = NULL,
                     sources = NULL,
                     title_spacing_adj = 1, # adjust the amount of space given to the title
                     subtitle_spacing_adj = 1, # adjust the amount of space given to the subtitle
                     height_adj = NULL, # adjust the vertical spacing of the mpanel charts
                     ncol = 2,
                     nrow = NULL,
                     align = c("v", "none", "h", "hv"),
                     axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
                     rel_heights = NULL,
                     # For development purposes only
                     test = !isTRUE(getOption("test_save"))
) {

  # Compile plots
  plots <- c(list(...), plotlist)

  # For single-panel graphs
  if (length(plots) == 0) plots <- list(plot)

  # Guard clauses and checks ------------------------------------------------

  # Check whether the plots are ggplot2 objects
  plots <- check_plots(plots)

  # Enforce chart type
  chart_type <- match.arg(chart_type)

  # Check if the save directory exists
  dir_provided <- grepl("^(.*)\\/.*\\..{3}$", filename)
  dir_name <- gsub("^(.*)\\/.*\\..{3}$", "\\1", filename)

  if (dir_provided && !dir.exists(dir_name))
    stop("The directory you are trying to save to does not exist.")

  # Enforce file format requirements if a file extension is provided
  if (grepl("\\..{3}$", filename) && !grepl("\\.(svg|pdf|eps)$", filename)) {
    stop("You must provide a file extension. Only PDF and SVG file formats are supported.")
  }

  # Determine which file formats to save
  if (grepl("\\..{3}$", filename)) {
    format <- gsub("^.*\\.(.{3})$", "\\1", filename)

    # Strip file extension from filename
    filename <- gsub("^(.*)\\..{3}$", "\\1", filename)
  } else {
    format <- match.arg(format, several.ok = TRUE)
  }

  # Check if the data frame can be written
  if (save_data && !is.data.frame(plot$data))
    stop("You have set save_data = TRUE, but the data frame could not be extracted from the ggplot. This may be caused by a plot with multiple data frames supplied (e.g. if each geom has its own data). In this case you will need to set save_data = FALSE and manually save the data used to produce the graph.")


  # Advisory messages -------------------------------------------------------

  adv_msg <- c()

  # Turn these off is the test option is TRUE
  print_msg <- !test

  ## Check if y-axis label text is good

  # Message if the y-axis label text is missing
  if (print_msg && (is.null(plot$labels$y) || nchar(plot$labels$y) == 0)) {
    adv_msg <- c(adv_msg, "Your y-axis label is missing. Please provide the units of the axis for the reader. Specify the 'y' argument in 'labs_e61()'.")
  }

  # Message if the y-axis label text is too long
  if (print_msg && isTRUE(nchar(plot$labels$y) > 5)) {
    adv_msg <- c(adv_msg, "Your y-axis label may be too long. Consider if the information needed to interpret the graph is already in the title and only specify the required units in the y-axis label e.g. %, ppt, $b.")
  }

  # Compile and print advisory messages
  print_adv <- function() {
    cli::cli_div(theme = list(".bad" = list(color = "#cc0000",
                                            before = paste0(cli::symbol$cross, " ")),
                              ".adv" = list(`background-color` = "#FBFF00")
    )
    )
    cli::cli_h1("--- Fix the following issues with your graph ----------------------------------------", class = "adv")
    cli::cli_ul()
    sapply(adv_msg, cli::cli_alert, class = "bad")
    cli::cli_end()
  }

  if (length(adv_msg) > 0 && test) print_adv()

  # Single/multi-specific functions --------------------------------

  # Check whether to save an mpanel or a single planel chart - these require
  # different approaches
  if (length(plots) > 1) {
    retval <- save_multi(
      filename,
      format = format,
      plotlist = plots,
      chart_type = chart_type,
      title = title,
      subtitle = subtitle,
      footnotes = footnotes,
      sources = sources,
      width = width, # control width of the chart
      height = height, # control height of the chart
      max_height = max_height, # control maximum height of the chart
      auto_scale = auto_scale,
      title_spacing_adj = title_spacing_adj, # adjust the amount of space given to the title
      subtitle_spacing_adj = subtitle_spacing_adj, # adjust the amount of space given to the subtitle
      height_adj = height_adj, # adjust the vertical spacing of the mpanel charts
      base_size = base_size,
      ncol = ncol,
      nrow = nrow,
      align = align,
      axis = axis,
      rel_heights = rel_heights
    )

  } else {

    retval <- save_single(
      filename = filename,
      plot = plots[[1]],
      chart_type = chart_type,
      auto_scale = auto_scale, # control whether y-axis is scaled
      width = width, # control width
      height = height, # control height
      max_height = max_height, # control max height
      format = format,
      base_size = base_size,
      save_data = save_data
    )
  }

  # Post-saving -------------------------------------------------------------

  # Save the data used to make the graph
  if (save_data) {
    data_name <- gsub("\\.(\\w{3})$", "\\.csv", filename)
    data.table::fwrite(plot$data, data_name)
  }

  # Opens the graph file if the option is set
  if (as.logical(getOption("open_e61_graph", FALSE))) {

    # Put filename back together
    filename <- paste0(filename, ".", format[[1]])

    file_to_open <- shQuote(here::here(filename))

    out <- try(system2("open", file_to_open))

    if (out != 0) warning("Graph file could not be opened.")
  }

  # Invisibly return the filepath
  invisible(retval)

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
