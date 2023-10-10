
#' Save a single-panel chart with e61 formatting
#' @noRd
save_single <- function(
    filename,
    plot = last_plot(),
    chart_type = NULL,
    auto_scale = TRUE, # manual control over whether y-axis is scaled
    width = NULL, # manual control over the width of the chart
    height = NULL, # manual control over the height of the chart
    max_height = 100, # manual control over the maximum height of the chart
    format = c("svg", "pdf", "eps", "png"),
    save_data = FALSE,
    base_size = 10,
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

  # Check if we have a spatial chart, if we do save without editing ---------

  is_spatial_chart <- FALSE

  for(i in seq_along(plot$layers)){

    layer_class <- class(plot$layers[[i]]$geom)

    if("GeomSf" %in% layer_class) {
      is_spatial_chart <- TRUE

      break
    }
  }

  # if it's a spatial plot, turn of autoscaling
  if(is_spatial_chart) auto_scale <- FALSE


  # Set maximum width based on output type ----------------------------------

  if(is.null(chart_type)) chart_type <- "MN"

  max_width <- get_plot_width(chart_type)
  max_height <- get_plot_width(chart_type)

  # update the base size variable
  base_size <- base_size * max_width / get_plot_width("MN")

  # update the base size without removing the legend
  if(is_spatial_chart){
    plot <- plot + theme_e61_spatial(base_size = base_size)

  } else {

    legend_position <- plot$theme$legend.position

    plot <- plot +
      theme_e61(
        keep_legend = T,
        base_size = base_size
      )

    if(!is.null(legend_position)){
      plot <- plot + theme(legend.position = legend_position)
    }
  }

  plot_build <- ggplot_build(plot)


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

  # set to a dummy value initially
  max_panel_width <- 1e10

  # check whether the user has supplied a given width first (i.e. different to the default 8.5cm)
  if(is.null(width)) {

    # When coord_flip() is used to make a plot horizontal, the default dims are too small
    if (isTRUE("CoordFlip" %in% class(ggplot_build(plot)$layout$coord))) {

      width <- max_width
      max_panel_width <- max_width / 2 # only allow the panel to be at most half the column consistent with other chart types

      plot <- plot + format_flip()

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
  p <- ggplotGrob(plot)

  # allow charts to be the width of the panels
  right_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-r"), get_grob_width(p, grob_name = "axis-r"))
  left_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-l"), get_grob_width(p, grob_name = "axis-l"))

  known_wd <- right_axis_width + left_axis_width

  tot_panel_width <- width - known_wd

  # check that the total panel width isn't over the maximum, again this is only an issue for the coord flipped charts
  if(tot_panel_width > max_panel_width) tot_panel_width <- max_panel_width

  # update the width after this check
  width <- tot_panel_width + known_wd

  plot <- update_labs(plot, tot_panel_width)

  # update the mplot_labels
  plot <- update_mplot_label(plot, chart_type, base_size)

  # update y-axis labels
  plot <- update_y_axis_labels(plot)


  # Height adjustments ----------------------------------------------------

  if(is.null(height)){

    # Step 1 - Get the amount of free height and width we have to play with (what is not already used up by the set elements)
    p <- ggplotGrob(plot)

    known_ht <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))

    # calculate the total free width and height we have to play with
    free_ht <- max_height - known_ht
    free_wd <- width - known_wd

    # Step 2 - Find the number of panels (these have null rows and heights because they are flexible)
    null_rowhts <- as.numeric(p$heights[grid::unitType(p$heights) == "null"])
    null_colwds <- as.numeric(p$widths[grid::unitType(p$widths) == "null"])

    panel_asps <- (
      matrix(null_rowhts, ncol = 1)
      %*% matrix(1 / null_colwds, nrow = 1))

    # Step 3 - Divide the free width by the number of columns (panels) we have
    panel_width <- free_wd / n_panel_cols # width of each individual panel

    # Check that the panel width does not exceed the max panel width
    # - this will only happen for flipped bar charts and is designed to let them
    # - take up more space if they have long y-axis titles, but not take up
    # - excessive space with the panel
    if(panel_width > max_panel_width) panel_width <- max_panel_width

    # panel height is just the panel width * the aspect ratio
    panel_height <- panel_width * max(panel_asps[1,]) # height of each panel (width * aspect ratio)

    # Step 4 - Work out the best height of the plot - if it can be achieved under the maximum height
    if(panel_height * n_panel_rows < free_ht){

      height <- known_ht + panel_height * n_panel_rows

    } else {
      height <- max_height
    }
  }

  # Save ------------------------------------------------------------------
  save_graph(graph = plot, format, filename, width, height)

  # Post-save ------------------------------------

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

  # Invisibly returns the filename (or vector of filenames). Currently some of
  # the tests rely on the filename being returned so maybe don't change this
  # without a good reason.
  retval <- paste(filename, format, sep = ".")

  invisible(retval)
}
