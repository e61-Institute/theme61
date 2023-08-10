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
#' @param autoheight Automatically sets the graph height. Only works with vector
#'   graphics formats. Defaults to TRUE.
#' @param start_h Numeric. Optionally, supply a starting height for the
#'   auto-height algorithm.
#' @param save_data Logical. Set to TRUE if you want to save a .csv with the
#'   same name as the graph that contains the data needed to recreate the graph
#'   (defaults to FALSE).
#' @param dim_msg Logical. Set to TRUE if you want to know what dimensions the
#'   graph was saved to (defaults to FALSE).
#' @param resize Numeric. Only used when PNG is the file format. Resize the
#'   graph width and height. You may also need to adjust the \code{pointsize}
#'   and \code{res} to ensure the text is readable.
#' @param test For development use only.
#' @inheritParams grDevices::png
#' @return Invisibly returns the file name.
#' @export

save_e61 <-  function(filename,
                      plot = ggplot2::last_plot(),
                      width = 8.5,
                      height = NULL, # maximum height of the chart
                      format = c("svg", "pdf", "eps", "png"),
                      autoheight = TRUE,
                      save_data = FALSE,
                      resize = NULL,
                      pointsize = 12,
                      res = 72,
                      test = !isTRUE(getOption("test_save"))
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


  user_w <- !is.null(width)

  # Collate graph attributes -----------------------------------------------

  # Build the plot object internally
  # Could have wider applications in the future...
  plot_build <- ggplot_build(plot)

  # Check if graph is horizontal
  is_flip <- isTRUE("CoordFlip" %in% class(ggplot2::ggplot_build(plot)$layout$coord))

  # Check if the graph was generated with mpanel_e61 by checking for attributes
  # added to mpanels
  is_multi <- !is.null(attr(plot, "panel_rows"))

  # Get facet dimensions if applicable
  if (is.null(plot$facet$params)) {
    is_facet <- TRUE

    facet_dims <- list()
    facet_dims$cols <- plot_build$layout$layout$COL
    facet_dims$rows <- plot_build$layout$layout$ROW

  } else {
    is_facet <- FALSE
  }


  # Advisory messages -------------------------------------------------------

  # Note that the following checks don't apply when multi-panel graphs are created
  print_msg <-
    if (is_multi || isTRUE(getOption("not_e61_style_msg"))) FALSE else TRUE

  adv_msg <- c()
  info_msg <- c()

  # Message if theme function not used
  if (print_msg && is.null(attr(plot$theme, "t61"))) {

    adv_msg <- c(adv_msg, "Add 'theme_e61()' to your ggplot code to ensure the e61 theme is applied.")
  }

  # Message if package scale_x/y function not used
  if (print_msg && !"scale_e61" %in% class(ggplot2::layer_scales(plot)$y) && "ScaleContinuous" %in% class(ggplot2::layer_scales(plot)$y)) {

    adv_msg <- c(adv_msg, "Add 'scale_y_continuous_e61()' to your ggplot code to ensure the graph axes render correctly.")
  }

  # Message if colour/fill functions aren't used, message to appear only if a
  # colour/fill mappping exists
  if (print_msg && any(grepl("(colour|color|fill)", names(plot$mapping))) &&
      !"scale_col_e61" %in% unlist(sapply(plot$scales$scales, class))) {

    adv_msg <- c(adv_msg, "Add 'scale_colour/fill_e61()' to your ggplot code to ensure the e61 colour palette is used.")
  }

  # Message if the y-axis label text is missing
  if (print_msg && (is.null(plot$labels$y) || nchar(plot$labels$y) == 0)) {
    adv_msg <- c(adv_msg, "Your y-axis label is missing. Please provide the units of the axis for the reader. Specify the 'y' argument in 'labs_e61()'.")
  }

  # Message if the y-axis label text is too long
  if (print_msg && isTRUE(nchar(plot$labels$y) > 5)) {
    adv_msg <- c(adv_msg, "Your y-axis label may be too long. Consider if the information needed to interpret the graph is already in the title and only specify the required units in the y-axis label e.g. %, ppt, $b.")
  }

  # Height and width setting ------------------------------------------------

  ## Fallback default dimensions ----

  # Fallback default dimensions if not otherwise specified
  if (!user_w && !is_flip) width <- 8.5
  if (!user_h && !is_flip) height <- 9

  # When coord_flip() is used to make a plot horizontal, the default dims are
  # too small
  if (!user_w && is_flip) width <- 17
  if (!user_h && is_flip) height <- 12

  ## Multi-panels ----

  # Adjust the width to fit the extra panels and send out user message to
  # specify the height

  # Pull together mpanel attributes
  mp_dims <- list(
    cols = attr(plot, "panel_cols"),
    rows = attr(plot, "panel_rows"),
    head_dim = attr(plot, "panel_head"),
    foot_dim = attr(plot, "panel_foot")
  )

  if (is_multi && !user_w) {
    width <- 8.5 * mp_dims$cols
  }

  ## Faceted regular graphs ----

  # Expand the width if the facet has multiple columns
  if (is_facet && !user_w) {
    width <- 8.5 * facet_dims$cols
    height <- 9 * facet_dims$rows
  }

  ## Regular graphs ----

  # Message for the user to specify their own height to dimension graphs
  # correctly. This message does not appear if autoheight functions are used.
  if (!user_h && !autoheight) {

    info_msg <- c(info_msg, paste0("When you use ", sQuote("save_e61()"), " to save images with defaults, you should set the ", sQuote("height"), " argument manually to your own value to avoid excess/insufficient whitespace on the rendered image. Unfortunately the only way to check this is to open the rendered graphic and inspect it visually."))
  }

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

  # Trim excess height from graph ----------------------------------------------

  if (!user_h && autoheight) {

    if (!is.null(start_h)) height <- start_h

    # Autoheight function
    retval <- auto_height_finder(plot = plot,
                                 mpanel = is_multi,
                                 width = width,
                                 height = height)

    height <- round(retval$height, 1)
    iter <- retval$iter

    info_msg <-
      c(info_msg,
        paste0("Autoheight has set the graph height and width to ",
               height,
               " cm and ",
               width,
               " cm (in ",
               iter,
               " iterations)."
               )
        )
  }

  # Save --------------------------------------------------------------------

  lapply(format, function(fmt) {
    file_i <- paste0(filename, ".", fmt)

    switch(
      fmt,
      svg = svglite::svglite(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      eps = cairo_ps(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      pdf = cairo_pdf(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      png = png(filename = file_i, width = width, height = height, units = "cm", pointsize = pointsize, res = res, bg = "transparent")
    )
    print(plot)
    dev.off()
  })

  # Post-saving messages and functions ---------------------
  if (dim_msg) {
    info_msg <- c(info_msg, paste0("The graph height and width have been set to ", height, " and ", width, "."))
  }

  # Compile the messages together
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

  print_info <- function() {
    cli::cli_div(theme = list(".info-head" = list(color = "#247700"),
                              ".just-info" = list(color = "#000000")
                              )
                 )
    cli::cli_h1("--- For information -----------------------", class = "info-head")
    cli::cli_ul()
    sapply(info_msg, cli::cli_alert_info, class = "just-info")
    cli::cli_end()
  }

  if (length(adv_msg) > 0 && test) print_adv()
  if (length(info_msg) > 0 && test) print_info()

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

#' Helper function used in save_e61() to automatically determine the graph
#' height
#'
#' @param plot ggplot2 object
#' @param mpanel Logical. Whether the graph was generated using
#'   \code{mpanel_e61()} or not.
#' @param width Graph width as supplied to \code{save_e61()}.
#' @param height Graph height as supplied to \code{save_e61()}.
#' @param incr The increment in cm to iterate the graph heights by.
#' @return The height of the graph.
#' @noRd
auto_height_finder <- function(plot, mpanel, width, height, incr = 0.2) {




}
