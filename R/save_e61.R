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
#' @inheritParams grDevices::png
#' @return Invisibly returns the file name.
#' @export


save_e61 <- function(filename,
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
                     test = !isTRUE(getOption("test_save"))
) {

  # Advisory messages -------------------------------------------------------

  # Note that the following checks don't apply when multi-panel graphs are created
  print_msg <- TRUE

  adv_msg <- c()
  info_msg <- c()

  # Message if theme function not used
  if (print_msg && is.null(attr(plot$theme, "t61"))) {

    adv_msg <- c(adv_msg, "Add 'theme_e61()' to your ggplot code to ensure the e61 theme is applied.")
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


  # Set maximum width based on output type ----------------------------------

  if(is.null(chart_type)) chart_type <- "MN"

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

  plot_build <- ggplot2::ggplot_build(plot)


  # Update y-axis limits ----------------------------------------------------

  # check whether the plot is an mpanel plot or a regular plot
  if(is.null(attr(plot, "plot_type"))){
    is_mpanel <- FALSE

  } else if(attr(plot, "plot_type") == "mpanel"){
    is_mpanel <- TRUE

  } else {
    is_mpanel <- FALSE
  }

  # update the chart scales if this is an auto_scaled chart
  if(!is_mpanel) {

    # check if the y-var is numeric
    y_var_name <- ggplot2::quo_name(plot$mapping$y)
    y_var_class <- plot$data[[y_var_name]] %>% class()

    if (y_var_name == "NULL") {
      layers <- plot$layers

      for (j in seq_along(layers)) {
        # don't get y-aesthetic for geom_text objects
        layer_type <- layers[[j]]$geom %>% class()

        if ("GeomText" %in% layer_type) next

        # otherwise get the y-variable name and type
        y_var_name <- ggplot2::quo_name(layers[[j]]$mapping$y)

        if (y_var_name == "NULL") next

        y_var_class <- plot$data[[y_var_name]] %>% class()

        # if we found one numeric class, break because that all we need
        if(y_var_class == "numeric") break
      }

    } else {
      y_var_class <- plot$data[[y_var_name]] %>% class()
    }

    # if the y-variable name is not in the dataset, throw an error and force the user to actually create it properly
    data_names <- names(plot$data)

    # TODO - fix this issue if possible
    if(!y_var_name %in% data_names) {
      stop("Unable to locate y-axis variable in dataset. Please make sure it is a valid variable name and in your dataset. Note that the auto scaler will not work with variables that are created within ggplot (e.g. you cannot do things like the following 'ggplot(data, aes(y = log(y_var))'. Please create all variables in your data set before using it to create a ggplot.")
    }

    # if the y-variable class is numeric, then update the chart scales
    if(y_var_class == "numeric"){

      # first check if we want to include a second y-axis or not (check by looking at whether it has a non-zero width grob)
      grobs <- ggplot2::ggplotGrob(plot)

      test_sec_axis <- get_grob_width(grobs, grob_name = "axis-r")

      # add a second axis if there is already one present
      sec_axis <- !(is.null(test_sec_axis) | test_sec_axis == 0)

      # then update
      suppressMessages({plot <- update_chart_scales(plot, auto_scale, sec_axis)})
    }
  }


  # Get the number of panel rows and columns ------------------------------

  # Check if the graph was generated with mpanel_e61 by checking for attributes added to mpanels
  if(!is.null(attr(plot, "panel_rows"))){

    n_panel_cols = attr(plot, "panel_cols")
    n_panel_rows = attr(plot, "panel_rows")

  # Get facet dimensions if applicable
  } else if (length(plot$facet$params) != 0) {

    n_panel_cols <- max(plot_build$layout$layout$COL)
    n_panel_rows <- max(plot_build$layout$layout$ROW)

  # The default is just 1 row and 1 column - i.e. no facets or mpanel plots have been used
  } else {
    n_panel_cols <- 1
    n_panel_rows <- 1
  }


  # Set width -------------------------------------------------------------

  # check whether the user has supplied a given width first (i.e. different to the default 8.5cm)
  if(is.null(width)) {

    # When coord_flip() is used to make a plot horizontal, the default dims are too small
    if (isTRUE("CoordFlip" %in% class(ggplot2::ggplot_build(plot)$layout$coord))) {

      width <- max_width

      plot <- plot + format_flipped_bar()

    # If it's only one panel, set the chart width to 2/3 of the max-width
    } else if(n_panel_cols == 1){

      width <- 2/3 * max_width

    # Else use the whole width
    } else {
      width <- max_width
    }
  }


  # Update labels -----------------------------------------------------------

  # update y-axis labels if the chart is not an mpanel chart
  if (!is_mpanel) {

    # if one of the y-variables is numeric, adjust the y-axis scale
    if (y_var_class == "numeric") {
      plot <- update_y_axis_labels(plot)
    }
  }

  # Update the size of the text used for titles, footnotes, axes etc.

  p <- ggplot2::ggplotGrob(plot)

  # allow charts to be the width of the panels
  right_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-r"), get_grob_width(p, grob_name = "axis-r")) + 0.25 # this seems to just overshoot hence the extra 0.25cm
  left_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-l"), get_grob_width(p, grob_name = "axis-l")) + 0.25 # this seems to just overshoot hence the extra 0.25cm

  known_wd <- right_axis_width + left_axis_width

  tot_panel_width <- width - known_wd

  plot <- update_labs(plot = plot, is_mpanel = is_mpanel, plot_width = tot_panel_width)


  # Height adjustments ----------------------------------------------------

  if(is.null(height)){

    # Step 1 - Get the amount of free height and width we have to play with (what is not already used up by the set elements)
    p <- ggplot2::ggplotGrob(plot)

    known_ht <- get_known_height(plot, is_mpanel)

    right_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-r"), get_grob_width(p, grob_name = "axis-r")) + 0.25 # this seems to just overshoot hence the extra 0.1cm
    left_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-l"), get_grob_width(p, grob_name = "axis-l")) + 0.25 # this seems to just overshoot hence the extra 0.1cm

    known_wd <- right_axis_width + left_axis_width

    # calculate the free width and height we have to play with
    free_ht <- if(!is.null(max_height)) {
      max_height - known_ht

    } else {
      100 - known_ht
    }

    free_wd <- width - known_wd

    # Step 2 - Find the number of panels (these have null rows and heights because they are flexible)
    null_rowhts <- as.numeric(p$heights[grid::unitType(p$heights) == "null"])
    null_colwds <- as.numeric(p$widths[grid::unitType(p$widths) == "null"])
    panel_asps <- (
      matrix(null_rowhts, ncol = 1)
      %*% matrix(1 / null_colwds, nrow = 1))

    # check that aspect ratios are consistent
    # stop("Panel aspect ratios must be consistent")

    # Step 3 - Divide the free width by the number of columns (panels) we have
    panel_width <- free_wd / n_panel_cols # width of each panel
    panel_height <- panel_width * max(panel_asps[1,]) # height of each panel (width * aspect ratio)

    # Step 4 - Work out the best height of the plot

    # Variable to adjust scale the total plot height as the estimated height grows - there seems to be a small amount of error in the above calculation
    size_adj <- 1.05

    # calculate height taking into account the various adjustments
    height <- known_ht * size_adj + panel_height * n_panel_rows
  }


  # Save ------------------------------------------------------------------

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

  # Post-saving messages and functions ------------------------------------

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
