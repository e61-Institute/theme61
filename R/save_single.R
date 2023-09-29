
#' Save a single-panel chart with e61 formatting
#' @noRd
save_single <- function(
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
    base_size = 10,
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

  max_width <- get_plot_width(chart_type, max_height)$max_width
  max_height <- get_plot_width(chart_type, max_height)$max_height

  plot <- plot + theme_e61(base_size = base_size * max_width / 18.59)

  plot_build <- ggplot2::ggplot_build(plot)


  # Update y-axis limits ----------------------------------------------------

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
  right_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-r"), get_grob_width(p, grob_name = "axis-r"))
  left_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-l"), get_grob_width(p, grob_name = "axis-l"))

  known_wd <- right_axis_width + left_axis_width

  tot_panel_width <- width - known_wd

  plot <- update_labs(plot, tot_panel_width)

  # update the mplot_labels
  plot <- update_mplot_label(plot, plot_width = tot_panel_width, chart_type, base_size)

  # update y-axis labels
  plot <- update_y_axis_labels(plot, base_size = base_size)


  # Height adjustments ----------------------------------------------------

  if(is.null(height)){

    # Step 1 - Get the amount of free height and width we have to play with (what is not already used up by the set elements)
    p <- ggplot2::ggplotGrob(plot)

    known_ht <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))

    right_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-r"), get_grob_width(p, grob_name = "axis-r"))
    left_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-l"), get_grob_width(p, grob_name = "axis-l"))

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

    # Step 3 - Divide the free width by the number of columns (panels) we have
    panel_width <- free_wd / n_panel_cols # width of each panel
    panel_height <- panel_width * max(panel_asps[1,]) # height of each panel (width * aspect ratio)

    # Step 4 - Work out the best height of the plot - if it can be achieved under the maximum height
    if(panel_height * n_panel_rows < free_ht){
      height <- known_ht + panel_height * n_panel_rows
    } else {
      height <- max_height
    }
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
