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
                     base_size = 8, # set the base size for the theme61 font size call
                     height_adj = 1, # adjust the vertical spacing of the mpanel charts
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

  if(length(plots) > 1){

    save_mpanel_e61(
      filename,
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
      base_size = base_size, # set the base size for the theme61 font size call
      height_adj = height_adj, # adjust the vertical spacing of the mpanel charts
      ncol = ncol,
      nrow = nrow,
      align = align,
      axis = axis,
      rel_heights = rel_heights,
      pointsize = pointsize,
      res = res
    )

  } else {

    save_spanel_e61(
      filename = filename,
      plot = plot,
      chart_type = chart_type,
      auto_scale = auto_scale, # manual control over whether y-axis is scaled
      width = width, # manual control over the width of the chart
      height = height, # manual control over the height of the chart
      max_height = max_height, # manual control over the maximum height of the chart
      format = format,
      save_data = save_data,
      resize = resize,
      pointsize = pointsize,
      res = res,
      test = test
    )
  }
}

#' Save a single-panel chart with e61 formatting
#' @noRd
save_spanel_e61 <- function(
    filename,
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
    test = !isTRUE(getOption("test_save"))){

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
  if(auto_scale) plot <- update_scales(plot, auto_scale)


  # Get the number of panel rows and columns ------------------------------


  # Get facet dimensions if applicable
  if (length(plot$facet$params) != 0) {

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

      # If it's only one panel, set the chart width to 1/2 of the max-width
    } else if(n_panel_cols == 1){

      width <- 1/2 * max_width

      # Else use the whole width
    } else {
      width <- max_width
    }

  } else if(width == "col_width"){
    width <- max_width
  }


  # Update labels -----------------------------------------------------------

  # Update the size of the text used for titles, footnotes, axes etc.

  p <- ggplot2::ggplotGrob(plot)

  # allow charts to be the width of the panels
  right_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-r"), get_grob_width(p, grob_name = "axis-r")) + 0.25 # this seems to just overshoot hence the extra 0.25cm
  left_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-l"), get_grob_width(p, grob_name = "axis-l")) + 0.25 # this seems to just overshoot hence the extra 0.25cm

  known_wd <- right_axis_width + left_axis_width

  tot_panel_width <- width - known_wd

  plot <- update_labs(plot = plot, is_mpanel = is_mpanel, plot_width = tot_panel_width)


  # Update the y-axis labels ------------------------------------------------

  # update y-axis labels
  plot <- update_y_axis_labels(plot)


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


#' Aesthetically update the y-axis scales and labels
#' @noRd
update_scales <- function(plot, auto_scale, warn = F){

  # check if we have a numeric y-variable
  check_y_var <- check_for_y_var(plot)

  # if we don't have a numeric y-variable then check whether the plot contains ageom_density or geom_histogram (GeomBar without a y-variable)
  if (!check_y_var) {
    layers <- plot$layers

    for (j in seq_along(layers)){

      # don't get y-aesthetic for geom_text objects
      layer_type <- layers[[j]]$geom %>% class()

      # if there isn't one but it is a density plot or a bar chart then it is either a density chart or a histogram so go ahead
      if ("GeomDensity" %in% layer_type | "GeomBar" %in% layer_type) {

        check_y_var <- T

        break
      }
    }
  }

  # check if we want to include a second y-axis or not (check by looking at whether it has a non-zero width grob)
  grobs <- ggplot2::ggplotGrob(plot)

  test_sec_axis <- get_grob_width(grobs, grob_name = "axis-r")

  sec_axis <- !(is.null(test_sec_axis) | test_sec_axis == 0)

  # if the y-variable class is numeric, or the plot is a density or histogram, then update the chart scales
  if(check_y_var){

    suppressMessages({plot <- update_chart_scales(plot, auto_scale, sec_axis)})

  # if the y-var class is NULL, send a warning message about the auto updating of chart scales
  } else if(!check_y_var & warn == F){

    warning("Could not identify the class of the y variable. This prevents the y-axis scales from being automatically updated to aesthetic values. To address this issue check that you have not edited the variable within your ggplot call (e.g. aes(y = 100 * var)). Instead make any changes before passing the dataset to ggplot (e.g. data %>% mutate(new_var = 100 * var) %>% ggplot(...)).")
    warn <<- T
  }

  return(plot)
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

#' Check whether the dataset has a y-variable that can be used for scaling
#'@noRd

check_for_y_var <- function(plot){

  chart_data <- ggplot2::ggplot_build(plot)$data

  check <- F

  for(i in seq_along(chart_data)){

    # check the y-variable first
    if(!is.null(chart_data[[i]]$y) & is.numeric(chart_data[[i]]$y)){
      check <- T
      break
    }

    if(!is.null(chart_data[[i]]$ymax) | !is.null(chart_data[[i]]$ymin)){

      check <- T
      break
    }
  }

  return(check)
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
