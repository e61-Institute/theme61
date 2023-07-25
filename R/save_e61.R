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
#'
#' @param filename File name to create on disk. Providing the file format
#'   extension (e.g. .svg) is optional. The file extension must be lowercase. If
#'   you want to save multiple files with different formats, see the
#'   \code{format} argument for details.
#' @param plot Plot object to save. Defaults to the last plot displayed so
#'   usually you do not need to provide this explicitly.
#' @param width Plot width in cm. Defaults to 8.5.
#' @param height Plot height in cm. The function will attempt to calculate an
#'   appropriate height based on the labels you have provided, but this is
#'   sensitive to small changes in the graph text so you should check if the
#'   automatic value is aesthetically appropriate (no excess whitespace).
#'   Otherwise, the function will default to a value of 9 but this is unlikely
#'   to be appropriate.
#' @param format An optional vector of file formats to save as. For example
#'   \code{c("svg", "pdf")} will save 2 files with the same name to the same
#'   location to SVG and PDF formats. If the file format is specified in
#'   \code{filename}, then this argument is ignored.
#' @param save_data Logical. Set to TRUE if you want to save a .csv with the
#'   same name as the graph that contains the data needed to recreate the graph
#'   (defaults to FALSE).
#' @param dim_msg Logical. Set to TRUE if you want to know what dimensions the
#'   graph was saved to (defaults to FALSE).
#' @param resize Numeric. Only used when PNG is the file format. Resize the
#'   graph width and height. You may also need to adjust the \code{pointsize}
#'   and \code{res} to ensure the text is readable.
#' @inheritParams grDevices::png
#' @return Invisibly returns the file name.
#' @export

save_e61 <-  function(filename,
                      plot = ggplot2::last_plot(),
                      width = NULL,
                      height = NULL,
                      format = c("svg", "pdf", "eps", "png"),
                      save_data = FALSE,
                      dim_msg = FALSE,
                      resize = NULL,
                      pointsize = 12,
                      res = 72
                      ) {


  # Guard clauses and failing checks ----------------------------------------

  # Enforce file format requirements if a file extension is provided (quietly
  # permits eps files too)
  if (grepl("\\..{3}$", filename) && !grepl("\\.(png|svg|pdf|eps)$", filename)) {
    stop("You must provide a file extension. Only PDF, SVG and PNG file formats are currently supported.")
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

  # Identify graph attributes -----------------------------------------------

  # Check if graph is horizontal
  is_flip <- isTRUE("CoordFlip" %in% class(ggplot2::ggplot_build(plot)$layout$coord))

  # Check if the graph is a multi-panel generated with mpanel_e61 by
  # exploiting the fact that those graphs have zero-length labels, while
  # labs_e61 forces the user to have at least a title (so length > 0 for
  # single panels)
  is_multi <- !is.null(attr(plot, "panel_rows"))

  # Advisory messages -------------------------------------------------------

  print_msg <- if (is_multi) {
    FALSE
  } else if (isTRUE(getOption("no_t61_style_msg"))) {
    FALSE
  } else {
    TRUE
  }

  # The following checks don't apply when multi-panel graphs are created

  # Message if theme function not used
  if (print_msg && is.null(attr(plot$theme, "t61"))) {

    cli::cli_text(cli::bg_br_yellow(cli::col_black(
      "Please remember to use 'theme_e61()' in your ggplot code to ensure the ",
      "e61 theme is applied.")))
  }

  # Message if package scale_x/y function not used
  if (print_msg && !"scale_e61" %in% class(ggplot2::layer_scales(plot)$y)) {

    cli::cli_text(cli::bg_br_yellow(cli::col_black(
      "Please remember to use 'scale_x/y_continuous_e61()' in your ggplot code ",
      "to ensure the graph axes render correctly.")))
  }

  # Message if colour/fill functions aren't used, message to appear only if a
  # colour/fill mappping exists
  if (print_msg && any(grepl("(colour|color|fill)", names(plot$mapping))) &&
      !"scale_col_e61" %in% unlist(sapply(plot$scales$scales, class))) {

    cli::cli_text(cli::bg_br_yellow(cli::col_black(
      "Please remember to use 'scale_colour/fill_e61()' in your ggplot code ",
      "to ensure the e61 colour palette is used.")))
  }

  # Message if the y-axis label text is missing
  if (print_msg && (is.null(plot$labels$y) || nchar(plot$labels$y) == 0)) {

    cli::cli_text(cli::bg_br_yellow(cli::col_black(
      "Your y-axis label is missing. Please provide the units of the axis for the reader.")))
  }

  # Message if the y-axis label text is too long
  if (print_msg && isTRUE(nchar(plot$labels$y) > 5)) {

    cli::cli_text(cli::bg_br_yellow(cli::col_black(
      "Your y-axis label is too long. Consider if the information needed to interpret the graph is already in the title, and only specify the units in the y-axis label.")))
  }

  # Height and width setting ------------------------------------------------

  # For multi-panels: Adjust the width to fit the extra panels and send out user
  # message to specify the height

  # Pull together mpanel attributes
  mp_dims <- list(
    cols = attr(plot, "panel_cols"),
    rows = attr(plot, "panel_rows"),
    head_dim = attr(plot, "panel_head"),
    foot_dim = attr(plot, "panel_foot")
  )

  if (is_multi && is.null(width)) {
    width <- 8.5 * mp_dims$cols
  }

  if (is_multi && is.null(height)) {
    height <- 7.5 + 8 * (mp_dims$rows - 1) + 0.5 * mp_dims$head_dim + 0.35 * mp_dims$foot_dim

    cli::cli_text(cli::col_green("Note: You are saving a multi-panel graph, save_e61() has automatically set the height to ", height, ", but this value may not be appropriate. Check how the saved graph file looks and adjust the height as required."))
  }

  # Calculate graph height based on the graph labels for normal orientation graphs
  if (is.null(height) && !is_flip && !is_multi) {

    h <- 6.5

    # Calculate the height adjustment needed for...

    # Titles
    t_adj <-
      if (!is.null(plot$labels$title)) 0.6 + n_count(plot$labels$title) * 0.3 else 0

    # Subtitles
    st_adj <-
      if (!is.null(plot$labels$subtitle)) 0.5 + n_count(plot$labels$subtitle) * 0.3 else 0

    # Captions
    cp_adj <-
      if (!is.null(plot$labels$caption)) 0.5 + n_count(plot$labels$caption) * 0.3 else 0

    # Adjustment for width of y-axis label
    y_adj <-
      if (!is.null(plot$labels$y)) (nchar(plot$labels$y) - 1) * -0.2 else 0

  height <- h + t_adj + st_adj + cp_adj + y_adj

  cli::cli_text(cli::col_green("Note: save_e61() has automatically set the height to ", height, ". Please open the saved graph file and check if this is actually appropriate for your graph. You may have to adjust the value if the y-axis is particularly wide."))

  }

  # Message for the user to specify their own height to dimension graphs
  # correctly. This message runs after the above so when the automatic height
  # setting is used it does not trigger.
  if (is.null(height)) {

    cli::cli_text(cli::col_green("Note: When you use ", sQuote("save_e61()"), " to save images with defaults, you should set the ", sQuote("height"), " argument manually to your own value to avoid excess/insufficient whitespace on the rendered image."))
    cli::cli_text(cli::col_green("Unfortunately the only way to check this is to open the rendered graphic and inspect it visually."))
  }

  # Fallback default dimensions if not otherwise specified
  if (is.null(width) && !is_flip) width <- 8.5
  if (is.null(height) && !is_flip) height <- 9

  # When coord_flip() is used to make a plot horizontal, the default dims are
  # too small
  if (is.null(width) && is_flip) width <- 17
  if (is.null(height) && is_flip) height <- 12

  # Resize elements if a png is to be output
  if (!is.null(resize)) {
    if (format != "png")
      stop("The 'resize' argument is not supported unless the file format is .png")
    if (!is.numeric(resize))
      stop("'resize' must be numeric.")

    # Rescale elements as required
    width <- width * resize
    height <- height * resize
  }


  # Save --------------------------------------------------------------------
  lapply(format, function(fmt) {
    file_i <- paste0(filename, ".", fmt)

    switch(
      fmt,
      svg = svglite::svglite(filename = file_i, width = cm_to_in(width), height = cm_to_in(height)),
      eps = cairo_ps(filename = file_i, width = cm_to_in(width), height = cm_to_in(height)),
      pdf = cairo_pdf(filename = file_i, width = cm_to_in(width), height = cm_to_in(height)),
      png = png(filename = file_i, width = width, height = height, units = "cm", pointsize = pointsize, res = res)
    )
    print(plot)
    dev.off()
  })

  # Post-saving messages and functions ---------------------
  if (dim_msg) cli::cli_text(cli::col_green("The graph height and width have been set to ", height, " and ", width, "."))

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

  # Invisibly returns the filename (or vector of filenames). Currently some of
  # the tests rely on the filename being returned so maybe don't change this
  # without a good reason.
  retval <- paste(filename, format, sep = ".")

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

#' Counts the number of occurrences of a line break (\n)
#'
#' @param text The string to be parsed.
#' @return Integer counting the number of line breaks in the string.
#' @noRd
n_count <- function(text) {
  nchar(text) - nchar(gsub("\n", "", text, fixed = TRUE))
}
