#' Saves ggplot graphs with sensible defaults
#'
#' Saves ggplot2 graphs made with using theme61. Using `save_e61()` is
#' required to ensure graphs are consistent with the e61 style and formatting.
#'
#' The supported file formats are SVG, PDF, EPS and PNG.
#'
#' Use PDF in all notes and SVG in PowerPoint presentations. PDFs and SVGs are
#' better as they are modern vector graphics file formats which can be scaled up
#' and down in size without blurring or becoming pixelated.
#'
#' PNG should only be used for Twitter posts for compatibility reasons.
#'
#' @param filename File name to create on disk. Providing the file format
#'   extension (e.g. .svg) is suggested when saving to a single file format. The
#'   file extension must be lowercase. If you want to save to multiple formats,
#'   do not include the extension, see the `format` argument for details.
#' @param plot (single-panel specific) Name of the plot object to save. Defaults
#'   to the last plot displayed so usually you do not need to provide this
#'   argument explicitly.
#' @param chart_type String. Type of chart. This is used to set sensible chart
#'   widths based on the width of text in each document. Options are "MN" (for
#'   micronotes), "RN" (research notes) or "PPT" (PowerPoints).
#' @param auto_scale Logical. Should the y-axis be scaled automatically. Default
#'   is TRUE.
#' @param dim An optional named list specifying the plot height and width.
#'   Defaults to NULL which means the graph dimensions will be set based on the
#'   chart type and function-calculated value.
#' @param max_height Numeric. The maximum height of your plot in cm. This is
#'   used to constrain the plot resizing algorithm in cases where you want to
#'   limit the height of your charts.
#' @param format An optional vector of file formats to save as. For example
#'   `c("svg", "pdf")` will save 2 files with the same name to the same
#'   location to SVG and PDF formats. If the file format is specified in
#'   `filename`, then this argument is ignored.
#' @param save_data Logical. Set to TRUE if you want to save a .csv with the
#'   same name as the graph that contains the data needed to recreate the graph
#'   (defaults to FALSE).
#' @param base_size Numeric. Chart font size. Default is 10.
#' @param res Numeric. For saving to PNG only. Increase the size of the saved
#'   PNG. E.g. `res = 2` doubles the size of the saved graph.
#' @param bg_colour Sets the graph background colour. Defaults to "white".
#'   Accepts a colour name, hex code or theme61 colour object name. For graphs
#'   used in research note boxes, set the colour to `e61_boxback`.
#' @param ... (multi-panel specific) Plot objects to put on the panel.
#' @param plotlist (multi-panel specific) List of plots to combine as an
#'   multi-panel and save. You can also enter the charts individually as
#'   arguments to the function.
#' @param height_adj (multi-panel specific) Rescales the height of the
#'   multi-panel. The function sets sensible defaults but this provides you with
#'   manual control if you need it.
#' @param spacing_adj (multi-panel specific) An optional named list specifying
#'   the adjustment to the title and subtitle. Rescales the size of the space
#'   give to the multi-panel title/subtitle. Use if you think the title looks
#'   too cramped on the chart.
#' @param rel_heights (multi-panel specific) A numeric vector giving the
#'   relative proportions of each graph component (title, plots, footer
#'   (optional)).
#' @param width,height `r lifecycle::badge("deprecated")` width and height are no longer supported; use `dim` instead.
#' @inheritParams labs_e61
#' @inheritParams cowplot::plot_grid
#' @return Invisibly returns the file name.
#' @export

save_e61 <- function(filename,
                     ...,
                     plot = last_plot(),
                     format = c("svg", "pdf", "eps", "png"),
                     chart_type = c("MN", "RN", "PPT"),
                     auto_scale = TRUE,
                     dim = list(height = NULL, width = NULL),
                     max_height = NULL,
                     save_data = FALSE,
                     base_size = 10,
                     res = 1,
                     # multi-panel specific arguments
                     bg_colour = "white",
                     plotlist = NULL,
                     title = NULL,
                     subtitle = NULL,
                     footnotes = NULL,
                     sources = NULL,
                     spacing_adj = list(title = 1, subtitle = 1),
                     height_adj = NULL,
                     ncol = 2,
                     nrow = NULL,
                     align = c("v", "none", "h", "hv"),
                     axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
                     rel_heights = NULL,
                     width = NULL,
                     height = NULL) {

  # Deprecation messages
  if (!missing("width"))
    lifecycle::deprecate_stop(when = "0.6.0",
                              what = "save_e61(width)",
                              details = c("!" = "It has been replaced with the `dim` argument which takes a named list like `list(width = 10, height = 10)`."))

  if (!missing("height"))
    lifecycle::deprecate_stop(when = "0.6.0",
                              what = "save_e61(height)",
                              details = c("!" = "It has been replaced with the `dim` argument which takes a named list like `list(width = 10, height = 10)`."))


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
  if (grepl("\\..{3}$", filename) && !grepl("\\.(svg|pdf|eps|png)$", filename)) {
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
  if (save_data && !is.data.frame(plots[[1]]$data))
    stop("You have set save_data = TRUE, but the data frame could not be extracted from the ggplot. This may be caused by a plot with multiple data frames supplied (e.g. if each geom has its own data). In this case you will need to set save_data = FALSE and manually save the data used to produce the graph.")

  # Check list args are valid
  if (!all(names(dim) %in% c("height", "width")))
    stop("You have specified invalid list elements in 'dim'.")

  if (!all(names(spacing_adj) %in% c("title", "subtitle")))
    stop("You have specified invalid list elements in 'spacing_adj'.")

  # Advisory messages -------------------------------------------------------

  ## Check if y-axis label text is good
  adv_msg <- c()

  y_miss <- c()
  y_long <- c()

  # Loop through the plots
  for(i in seq_along(plots)){

    # Skip checks if any of the following criteria are met
    skip_check <-
      if (
        # If a theme61 function is not being used
        length(plots[[i]]$scales$scales) > 1 &&
        !"scale_e61" %in% class(plots[[i]]$scales$scales[[2]])
      ) {
        TRUE
      } else if (
        # If rescaled dual axis scales are used
        length(plots[[i]]$scales$scales) > 1 &&
        "rescale_y" %in% class(plots[[i]]$scales$scales[[2]])
      ) {
        TRUE
      } else if (
        # If y_top is not being used
        isFALSE(attr(plots[[i]]$theme, "y_top"))
      ) {
        TRUE
      } else {
        FALSE
      }

    # Message if the y-axis label text is missing
    if (
      # Checks if y is missing or blank
      (is.null(plots[[i]]$labels$y) || nchar(plots[[i]]$labels$y) == 0) &&
      !skip_check
      ) {
      y_miss <- c(y_miss, i)
    }

    # Message if the y-axis label text is too long
    if (
      # Check label char length
      isTRUE(nchar(plots[[i]]$labels$y) > 5) &&
      !skip_check
      ) {
      y_long <- c(y_long, i)
    }
  }

  # Compile the messages
  if (length(y_miss) > 0) {
    y_miss <- paste0(
      "Plot ",
      paste(y_miss, collapse = ", "),
      " is missing a y-axis label. Provide the y-axis units for the reader by specifying the 'y' argument in 'labs_e61()'.")
  } else {
    y_miss <- NULL
  }

  if (length(y_long) > 0) {
    y_long <- paste0(
      "Plot ",
      paste(y_long, collapse = ", "),
      " y-axis labels may be too long. Consider if the information needed to interpret the graph is already in the title and only specify the required units in the y-axis label (for example you could use: %, ppt, $b, '000, n)."
    )
  } else {
    y_long <- NULL
  }

  adv_msg <- c(y_miss, y_long)

  # Compile and print advisory messages
  print_adv <- function() {
    cli::cli_div(theme = list(".bad" = list(color = "#cc0000",
                                            before = paste0(cli::symbol$cross, " ")),
                              ".adv" = list(`color` = "#cc0000")
    )
    )
    cli::cli_h1("--- Fix the following issues with your graph ----------------------------------------", class = "adv")
    cli::cli_ul()
    sapply(adv_msg, cli::cli_alert, class = "bad")
    cli::cli_end()
  }

  if (length(adv_msg) > 0 && is.null(getOption("no_advisory"))) print_adv()

  # # Require user acknowledgement if there are issues to address
  # # Turn these off in test env
  # if (length(adv_msg) > 0 && !is_testing() && is.null(getOption("no_advisory"))) {
  #
  #   # Require user acknowledgement
  #   prompt <- ""
  #   while (prompt == "") {
  #     prompt <- readline(prompt = "Type 'Y' to stop generating the graph or 'N' to continue.")
  #   }
  #
  #   if (prompt == "Y") {
  #     return(message("Stopping graph generation based on user request. To turn off this message for the remainder of the session, run `options(no_advisory = TRUE)`."))
  #   } else {
  #     message("To turn off this message for the remainder of the session, set the `no_advisory` option to TRUE.")
  #   }
  #
  # }

  # Make graph to save --------------------------------

  # Check whether to save an mpanel or a single planel chart - these require
  # different approaches
  if (length(plots) > 1) {
    save_input <- save_multi(
      filename = filename,
      format = format,
      plots = plots,
      chart_type = chart_type,
      title = title,
      subtitle = subtitle,
      footnotes = footnotes,
      sources = sources,
      width = dim$width, # control width of the chart
      height = dim$height, # control height of the chart
      max_height = max_height, # control maximum height of the chart
      auto_scale = auto_scale,
      title_spacing_adj = spacing_adj$title, # adjust the amount of space given to the title
      subtitle_spacing_adj = spacing_adj$subtitle, # adjust the amount of space given to the subtitle
      height_adj = height_adj, # adjust the vertical spacing of the mpanel charts
      base_size = base_size,
      ncol = ncol,
      nrow = nrow,
      align = align,
      axis = axis,
      rel_heights = rel_heights,
      bg_colour = bg_colour
    )
  } else {

    save_input <- save_single(
      filename = filename,
      plot = plots[[1]],
      chart_type = chart_type,
      auto_scale = auto_scale, # control whether y-axis is scaled
      width = dim$width, # control width
      height = dim$height, # control height
      max_height = max_height, # control max height
      format = format,
      base_size = base_size,
      bg_colour = bg_colour
    )
  }


  # Save --------------------------------------------------------------------

  save_graph(
    graph = save_input$graph,
    format = format,
    filename = filename,
    width = save_input$width,
    height = save_input$height,
    bg_colour = bg_colour,
    res = res
  )

  # Post-saving -------------------------------------------------------------

  # Save the data used to make the graph
  if (save_data) {

    for (i in seq_along(plots)) {
      data_name <- gsub("\\.(\\w{3})$", paste0(i, ".csv"), filename)
      data.table::fwrite(plots[[i]]$data, data_name)
    }
  }

  # Opens the graph file in the Viewer or browser

  # Put filename back together
  file_to_open <- paste0(filename, ".", format[[1]])

  if (isTRUE(getOption("open_e61_graph", FALSE))) {
    file_to_open <- shQuote(here::here(file_to_open))

    out <- try(system2("open", file_to_open))

    if (out != 0) warning("Graph file could not be opened")
  } else {

    # rstudioapi::viewer will only open temp files in the Viewer pane for some reason
    temp_file <- tempfile(fileext = paste0(".", format[[1]]))
    file.copy(filename, temp_file)

    out <- try(rstudioapi::viewer(temp_file))

    if (!is.null(out)) warning("Graph file could not be opened.")

  }

  # Invisibly returns the filename/s
  retval <- paste(filename, format, sep = ".")

  invisible(retval)

}

#' Set option to open graphs in the browser instead of the Viewer pane
#'
#' Previous versions of theme61 opened graphs in the browser instead of the
#' Viewer pane. You can bring back this functionality by running this function,
#' which sets a session-wide option.
#'
#' @return This function is used for its side effects.
#' @rdname open_graph_browser
#' @export
set_open_graph_browser <- function() {
  options(open_e61_graph = TRUE)

  invisible(TRUE)
}

#' @rdname open_graph_browser
#' @export
unset_open_graph <- function() {
  options(open_e61_graph = FALSE)

  invisible(FALSE)
}

#' Converts SVG to PNG
#'
#' Converts an SVG file to a PNG file
#'
#' @param file_in File path to the SVG image to convert.
#' @param file_out File path to the PNG image to save. Default saves a file with
#'   the same name and location (except for the file extension).
#' @param delete Logical. Delete the original SVG file? (defaults to FALSE)
#' @param res Numeric. Increase the dimensions of the saved PNG. E.g. `res
#'   = 2` doubles the dimensions of the saved graph.
#' @return Invisibly returns the file path to the PNG image
#' @keywords internal
#' @export
svg_to_png <- function(file_in, file_out = NULL, res = 1, delete = FALSE) {

  if (!grepl(".*\\.svg$", file_in))
    stop("file_in must be an svg file.")

  if (is.null(file_out)) {
    file_out <- gsub("(.*)\\.svg$", "\\1.png", file_in)
  } else if (!grepl(".*\\.png$", file_out)) {
    stop("file_out must be a png file.")
  }

  if (res != 1) {
    # This approach to rescaling starts by saving a rescaled SVG before
    # converting it to PNG. Hence the need for temp files.
    file_temp_svg <- "intermed.svg"
    file_temp_png <- "intermed.png"

    res <- res / 1.25 # For some reason any res > 1 scales 1:1.25...

    rsvg::rsvg_png(svg = file_in, file = file_temp_png)

    g_info <- magick::image_info(magick::image_read(file_temp_png))

    rsvg::rsvg_svg(svg = file_in,
                   file = file_temp_svg,
                   width = g_info$width * res,
                   height = g_info$height * res
                   )

    rsvg::rsvg_png(svg = file_temp_svg, file = file_out)

    unlink(file_temp_svg)
    unlink(file_temp_png)

  } else {

    rsvg::rsvg_png(svg = file_in, file = file_out)

  }

  if (delete) unlink(file_in)

  invisible(file_out)
}
