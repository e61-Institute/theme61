#' Save graphs with theme61 styles and defaults
#'
#' Saves ggplot2 graphs made with using theme61. Using `save_e61()` is required
#' to ensure graphs are consistent with the e61 style and formatting.
#'
#' Use PDF in all notes and SVG in PowerPoint presentations. PDFs and SVGs are
#' better as they are modern vector graphics file formats which can be scaled up
#' and down in size without blurring or becoming pixelated. PNG should only be
#' used when required for compatibility reasons.
#'
#' @param filename File name to create on disk. Providing the file format
#'   extension (e.g. .svg) is suggested when saving to a single file format. The
#'   file extension must be lowercase. If you want to save to multiple formats,
#'   do not include the extension, see the `format` argument for details.
#' @param plot (single-panel specific) Name of the plot object to save. Defaults
#'   to the last plot displayed so usually you do not need to provide this
#'   argument explicitly.
#' @param chart_type String, or vector of strings if saving multiple plots. Type
#'   of chart. This is used to set sensible chart widths based on the type of
#'   plot you are saving. Options are "normal" (default; for normal charts),
#'   "wide" (for time series graphs) or "square" (for scatter plots).
#' @param auto_scale Logical. Scale the y-axis automatically. Default is TRUE.
#' @param dim An optional named list specifying the plot height and width.
#'   Defaults to NULL which means the graph dimensions will be calculated
#'   automatically.
#' @param pad_width Numeric. Add horizontal whitespace to the sides of the
#'   graph. Defaults to no additional padding.
#' @param max_height Numeric. The maximum height of your plot in cm. This is
#'   used to constrain the plot resizing algorithm in cases where you want to
#'   limit the height of your charts. Defaults to NULL which does not restrict
#'   the height.
#' @param format A string vector of file formats to save as. Accepts "svg",
#'   "pdf", "eps", "png", "jpg". For example `c("svg", "pdf")` will save 2 files
#'   with the same name to the same location to SVG and PDF formats. If the file
#'   format is specified in `filename` or by the `set_format` option, then this
#'   argument is ignored.
#' @param save_data Logical. Set to TRUE if you want to save a .csv with the
#'   same name as the graph that contains the data needed to recreate the graph
#'   (defaults to FALSE).
#' @param print_info Logical. Set to TRUE if you want graph dimensions and other
#'   information printed to the console. Defaults to FALSE.
#' @param spell_check Logical. Check spelling of words in the title and caption.
#'   Defaults to TRUE. Set to FALSE to turn off.
#' @param preview Logical. Set to TRUE to show a preview of the graph in the
#'   Viewer pane but not save to disk. Defaults to FALSE.
#' @param base_size Numeric. Chart font size. Default is 10.
#' @param res Numeric. For saving to PNG only. Rescale the size of the saved
#'   PNG. E.g. `res = 2` doubles the size of the saved graph.
#' @param bg_colour Set the graph background colour. Accepts a colour name, hex
#'   code or theme61 colour object name. Defaults to "white". For graphs used in
#'   research note boxes, set the colour to `e61_boxback`.
#' @param ... (multi-panel specific) Plot objects to put on the panel.
#' @param plotlist (multi-panel specific) List of plots to combine as an
#'   multi-panel and save. You can also enter the charts individually as
#'   arguments to the function.
#' @param height_adj (multi-panel specific) Rescales the height of the
#'   multi-panel. The function sets sensible defaults but this provides you with
#'   manual control if you need it.
#' @param spacing_adj (multi-panel specific) A named list specifying the
#'   adjustment to the title and subtitle. Rescales the size of the space given
#'   to the multi-panel title/subtitle. Use if you think the title looks too
#'   cramped on the chart.
#' @param rel_heights (multi-panel specific) A numeric vector giving the
#'   relative proportions of each graph component (title, plots, footer).
#' @inheritParams labs_e61
#' @inheritParams cowplot::plot_grid
#' @return Invisibly returns the file name.
#' @export

save_e61 <- function(filename = NULL,
                     ...,
                     plot = last_plot(),
                     format = c("svg", "pdf", "eps", "png", "jpg"),
                     chart_type = NULL,
                     auto_scale = TRUE,
                     dim = list(height = NULL, width = NULL),
                     pad_width = 0,
                     max_height = NULL,
                     preview = FALSE,
                     save_data = FALSE,
                     print_info = FALSE,
                     spell_check = TRUE,
                     base_size = 10,
                     res = 1,
                     bg_colour = "white",
                     # multi-panel specific arguments
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
                     rel_heights = NULL
                     ) {

  # Compile plots
  plots <- c(list(...), plotlist)

  # For single-panel graphs
  if (length(plots) == 0) plots <- list(plot)

  # Guard clauses and checks ------------------------------------------------

  # Check whether the plots are ggplot2 objects
  plots <- check_plots(plots)

  # Enforce chart type
  if(is.null(chart_type)){
    chart_type <- "normal"

  } else if(length(chart_type) == 1){

    if(!chart_type %in% c("normal", "wide", "square"))
      stop("Invalid chart type. All chart types must be one of 'normal', 'wide' or 'square'.")

  } else if(length(chart_type) > 1){

    for(i in 1:length(chart_type)){
      if(!chart_type[i] %in% c("normal", "wide", "square"))
        stop("Invalid chart type. All chart types must be one of 'normal', 'wide' or 'square'.")
    }
  }

  # Check if filename has been provided when preview mode is FALSE
  if (!preview && is.null(filename)) stop("You must provide a file path to save the graph.")

  # Override save directory with temp file if preview mode is TRUE
  if (preview) {
    cli::cli_alert_info("Preview mode is activated, file will not be saved to disk.")
    filename <- tempfile(fileext = ".svg")
  }

  # Check if the save directory exists
  dir_provided <- grepl("^(.*)\\/.*\\..{3}$", filename)
  dir_name <- gsub("^(.*)\\/.*\\..{3}$", "\\1", filename)

  if (dir_provided && !dir.exists(dir_name))
    stop("The directory you are trying to save to does not exist.")

  # Enforce file format requirements if a file extension is provided
  if (grepl("\\..{3}$", filename) && !grepl("\\.(svg|pdf|eps|png|jpg)$", filename)) {
    stop("You must provide a valid file extension. The following file formats are supported: svg, pdf, eps, png, jpg.")
  }

  # Determine which file formats to save
  if (grepl("\\..{3}$", filename)) {
    format <- gsub("^.*\\.(.{3})$", "\\1", filename)

    # Strip file extension from filename
    filename <- gsub("^(.*)\\..{3}$", "\\1", filename)
  } else if (is.null(format) && !is.null(getOption("default_save_format"))) {
    format <- getOption("default_save_format")
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

  # Currently spell check is the only function provided in the advisory msgs
  bad_msg <- c()
  adv_msg <- c()

  spell_chk <- list()

  # Loop through the plots
  for(i in seq_along(plots)){

    # Spell checks
    fields <- c("title", "subtitle", "caption")

    spell_chk_i <- lapply(fields, function(field) {
      val <- plots[[i]]$label[[field]]
      if (!is.null(val)) {
        res <- check_spelling(val)
        if (length(res) > 0) return(res)
      }
      return(NULL)
    })

    # Assign names and remove NULLs (i.e. no typos)
    names(spell_chk_i) <- fields
    spell_chk_i <- Filter(Negate(is.null), spell_chk_i)

    # Format nicely
    spell_chk_i <- lapply(names(spell_chk_i), function(x) {

      paste0("There may be a typo in the ", x, ": ",
             paste(spell_chk_i[[x]], collapse = ", "))
    })

    spell_chk_i <- unlist(spell_chk_i)
    spell_chk <- c(spell_chk, spell_chk_i)

  }

  # Turn off spell check
  if (!spell_check) spell_chk <- NULL

  # Compile the messages
  bad_msg <- NULL
  adv_msg <- c(spell_chk)

  # Compile advisory messages
  print_adv <- function() {
    cli::cli_div(theme = list(".bad" = list(color = "#cc0000",
                                            before = paste0(cli::symbol$cross, " ")),
                              ".adv" = list(`color` = "#cc0000")
    )
    )
    cli::cli_h1("--- Your graph may have some issues to address ----------------------------------------", class = "adv")
    cli::cli_ul()
    sapply(bad_msg, cli::cli_alert, class = "bad")
    sapply(adv_msg, cli::cli_alert, class = "adv")
    cli::cli_end()
  }

  # Print advisory messages
  if (length(adv_msg) + length(bad_msg) > 0 && is.null(getOption("no_advisory"))) print_adv()

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
      pad_width = pad_width,
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
      pad_width = pad_width,
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

  # Print information on saving parameters
  if (print_info) {
    cli::cli_alert_info("Graph width = {round(save_input$width, 4)} and height = {round(save_input$height, 4)}.")
  }

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
  } else if (interactive()) {
    # Only run this in interactive mode
    # rstudioapi::viewer will only open temp files in the Viewer pane for some reason
    temp_file <- tempfile(fileext = paste0(".", format[[1]]))
    file.copy(file_to_open, temp_file)

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
unset_open_graph_browser <- function() {
  options(open_e61_graph = FALSE)

  invisible(FALSE)
}

#' Sets the default file save format if format is not specified
#'
#' This function sets the file save format if \code{format} is not specified in
#' \code{save_e61} and the file extension is not provided in \code{filename}.
#'
#' @inheritParams save_e61
#' @return This function is used for its side effects.
#' @rdname set_format
#' @export
set_format <- function(format) {
  options(default_save_format = format)

  invisible(TRUE)
}

#' Clears the default file save format from the session options
#'
#' This function clears the default file save format specified in
#' \code{set_format}.
#'
#' @return This function is used for its side effects.
#' @rdname set_format
#' @export
unset_format <- function() {
  options(default_save_format = NULL)

  invisible(FALSE)
}

#' Converts SVG to a bitmap file
#'
#' Converts an SVG file to a bitmap file, currently supports JPEG and PNG.
#'
#' @param file_in File path to the SVG image to convert.
#' @param file_out File path to the PNG or JPEG. image to save. Default saves a
#'   file with the same name and location (except for the file extension).
#' @param delete Logical. Delete the original SVG file? (defaults to FALSE).
#' @param res Numeric. Increase the dimensions of the saved PNG or JPEG. E.g.
#'   `res = 2` doubles the dimensions of the saved graph.
#' @return Invisibly returns the file path to the PNG image
#' @keywords internal
#' @export
svg_to_bitmap <- function(file_in, file_out = NULL, res = 1, delete = FALSE) {

  res <- res * 4 # res = 1 produces exceedingly small images now apparently

  if (!grepl(".*\\.svg$", file_in))
    stop("file_in must be an svg file.")

  # If file_out is null, then save to a PNG by default
  if (is.null(file_out)) {
    file_out <- gsub("(.*)\\.svg$", "\\1.png", file_in)
  } else if (!grepl(".*\\.png$", file_out) & !grepl(".*\\.jpg$", file_out)) {
    stop("file_out must be a png or jpg file.")
  }

  if(grepl(".*\\.png$", file_out)) fmt <- "png" else fmt <- "jpg"

  if (res != 1) {
    # This approach to rescaling starts by saving a rescaled SVG before
    # converting it to PNG. Hence the need for temp files.
    file_temp_svg <- "intermed.svg"
    file_temp_out <- paste0("intermed.", fmt)

    # For some reason this changed at some point and the scaling is fine now.
    # Keeping this here in case it reverts back in the future.
    # res <- res / 1.25 # For some reason any res > 1 scales 1:1.25...

    rsvg::rsvg_png(svg = file_in, file = file_temp_out)

    g_info <- magick::image_info(magick::image_read(file_temp_out))

    rsvg::rsvg_svg(svg = file_in,
                   file = file_temp_svg,
                   width = g_info$width * res,
                   height = g_info$height * res
                   )

    if(fmt == "png"){
      rsvg::rsvg_png(svg = file_temp_svg, file = file_out)

    } else if(fmt == "jpg"){
      image_temp <- magick::image_read_svg(file_temp_svg)

      magick::image_write(image = image_temp, path = file_out, format = "jpg")
    }

    unlink(file_temp_svg)
    unlink(file_temp_out)

  } else {

    if(fmt == "png"){
      rsvg::rsvg_png(svg = file_in, file = file_out)

    } else if(fmt == "jpg"){
      image_temp <- magick::image_read_svg(file_in)

      magick::image_write(image = image_temp, path = file_out, format = "jpg")
    }
  }

  if (delete) unlink(file_in)

  invisible(file_out)
}
