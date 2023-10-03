#' Save a multi-panel chart with e61 formatting
#' @noRd

save_multi <-
  function(filename,
           format = c("svg", "pdf", "eps", "png"),
           ...,
           plotlist = NULL,
           chart_type = "MN",
           title = NULL,
           subtitle = NULL,
           footnotes = NULL,
           sources = NULL,
           width = NULL, # manual control over the width of the chart
           height = NULL, # manual control over the height of the chart
           max_height = NULL, # manual control over the maximum height of the chart
           auto_scale = TRUE,
           title_spacing_adj = 1, # adjust the amount of space given to the title
           subtitle_spacing_adj = 1, # adjust the amount of space given to the subtitle
           base_size = 10, # set the base size for the theme61 font size call
           height_adj = NULL, # adjust the vertical spacing of the mpanel charts
           ncol = 2,
           nrow = NULL,
           align = c("v", "none", "h", "hv"),
           axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
           rel_heights = NULL,
           pointsize = 12,
           res = 300
           ) {


    # Combine and clean plot list ---------------------------------------------

    plots <- c(list(...), plotlist)

    plots <- check_plots(plots)


    # Guard clauses and failing checks ----------------------------------------

    save_guard(filename)

    # Determine which file formats to save
    if (grepl("\\..{3}$", filename)) {
      format <- gsub("^.*\\.(.{3})$", "\\1", filename)

      # Strip file extension from filename
      filename <- gsub("^(.*)\\..{3}$", "\\1", filename)
    } else {
      format <- match.arg(format, several.ok = TRUE)
    }

    # Set maximum width based on output type ----------------------------------

    if(is.null(chart_type)) chart_type <- "MN"

    max_width <- get_plot_dims(chart_type, max_height)$max_width
    max_height <- get_plot_dims(chart_type, max_height)$max_height

    # Update the base size based on the type of chart (MN, RN etc.) being produced
    base_size <- base_size * max_width / get_plot_dims("MN")$max_width


    # Set width -------------------------------------------------------------

    # check whether the user has supplied a given width first (i.e. different to the default 8.5cm)
    if(is.null(width)) {

      # If it's only one panel, set the chart width to 1/2 of the max-width
      if(ncol == 1){

        width <- 1/2 * max_width

        # Else use the whole width
      } else {
        width <- max_width
      }
    }


    # Format each plot in the plotlist and get dimensions ----------------------------------------

    # for each plot update the y-axis scales
    clean_plotlist <- list()

    # keep track of various aspects of the charts
    known_height <- 0
    max_panel_asps <- 0
    max_left_axis_width <- 0
    max_right_axis_width <- 0
    y_lab_max_size <- 0
    max_break_width <- 0

    warn <- F

    for(i in seq_along(plots)){

      temp_plot <- plots[[i]]

      # update the text sizes
      temp_plot <- temp_plot + theme_e61(base_size = base_size)

      # check whether to apply the autoscaler or not
      if(auto_scale) temp_plot <- update_scales(temp_plot, auto_scale, warn = F)

      # save the plot
      clean_plotlist[[i]] <- temp_plot


      # Calculate the known width of the chart ----

      p <- ggplotGrob(temp_plot)

      # get max panel aspect ratio - this is found by looking at the number of null rows and cols (the panels)
      null_rowhts <- as.numeric(p$heights[grid::unitType(p$heights) == "null"])
      null_colwds <- as.numeric(p$widths[grid::unitType(p$widths) == "null"])
      panel_asps <- (
        matrix(null_rowhts, ncol = 1)
        %*% matrix(1 / null_colwds, nrow = 1))

      max_panel_asps <- pmax(max_panel_asps, panel_asps[1,1])

      # keep track of the maximum y-axis title size in cm
      y_font_size <- get_font_size(temp_plot, elem = "axis.text.y", parent = "axis.text")

      y_lab_size <- get_text_width(temp_plot$labels$y, font_size = y_font_size)
      y_lab_max_size <- pmax(y_lab_size, y_lab_max_size, na.rm = T)

      # keep track of the maximum break size
      break_width <- get_y_break_width(temp_plot)
      max_break_width <- pmax(break_width, max_break_width, na.rm = T)

      # keep track of the max right axis and left axis widths as all charts are set to have the same dimensions
      right_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-r"), get_grob_width(p, grob_name = "axis-r"))
      max_right_axis_width <- pmax(max_right_axis_width, right_axis_width)

      left_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-l"), get_grob_width(p, grob_name = "axis-l"))
      max_left_axis_width <- pmax(max_left_axis_width, left_axis_width)

      # OLD - keep for testing
      # widths <- grid::convertWidth(p$widths, "cm", valueOnly = TRUE)
      # temp_width <- sum(widths)
      # if(i <= ncol) known_width <- known_width + temp_width

      if(is.null(max_left_axis_width) || length(max_left_axis_width) == 0)
        max_left_axis_width <- 0

      if(is.null(max_right_axis_width) || length(max_right_axis_width) == 0)
        max_right_axis_width <- 0


      # Calculate the known height of the chart ---------------------------------

      # take the known height as the maximum of all the chart heights
      temp_height <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))

      known_height <- pmax(known_height, temp_height)
    }

    # update the max panel asps ratio
    if(max_panel_asps == 0) max_panel_asps <- 0.75

    # update the known width of the plot - max left and right axis widths multiplied by the number of columns
    known_width <- (max_left_axis_width + max_right_axis_width) * ncol

    # calculate the width of each panel
    free_wd <- width - known_width

    # Divide the free width by the number of columns (panels) we have
    panel_width <- free_wd / ncol # width of each panel
    panel_height <- panel_width * max_panel_asps # height of each panel (width * aspect ratio)


    # Update the labels -------------------------------------------------------

    if(auto_scale){

      for(i in seq_along(clean_plotlist)){

        temp_plot <- clean_plotlist[[i]]

        # update y-axis labels
        suppressMessages({
          temp_plot <-
            update_y_axis_labels(
              temp_plot,
              max_y_lab = y_lab_max_size,
              max_break_width = max_break_width,
              base_size = base_size
            )
        })

        # update labels - for each set the limit as width - knowwidth (axis labels etc.) divided by the number of columns we have
        temp_plot <- update_labs(temp_plot, panel_width)

        # update any mplot label sizes
        temp_plot <- update_mplot_label(temp_plot, plot_width = panel_width, chart_type, base_size)

        # save the plot
        clean_plotlist[[i]] <- temp_plot
      }
    }


    # Gather the plots ----------------------------------------------------

    plots <- clean_plotlist

    if (is.null(nrow)) {
      nrow <- ceiling(length(plots) / ncol)
    }

    # Put together the panels
    panels <- cowplot::plot_grid(
      plotlist = plots,
      align = align,
      axis = axis,
      ncol = ncol,
      nrow = nrow
    )

    # These all need to be lists
    panels <- list(panels)
    lab_head <- list()
    lab_foot <- list()

    # Prepare titles, subtitles etc. --------------------------------------

    # define text sizes
    title_text_size <- base_size * 1.15
    subtitle_text_size <- base_size
    footer_text_size <- base_size * 0.8

    # title
    if(!is.null(title)){

      if(auto_scale){

        title <-
          rescale_text(
            text = title,
            text_type = "title",
            font_size = title_text_size,
            # plot width is total width
            plot_width = width - (max_left_axis_width + max_right_axis_width + 2 * points_to_mm(10) / 10)
          )
      }

      lab_head$title <-
        cowplot::ggdraw() +
        cowplot::draw_label(
          title,
          fontface = "bold",
          x = 0.5,
          hjust = 0.5,
          vjust = 0.5,
          size = title_text_size
        )
    }

    # subtitle
    if(!is.null(subtitle)){

      if(auto_scale){
        subtitle <-
          rescale_text(
            text = subtitle,
            text_type = "subtitle",
            font_size = subtitle_text_size,
            # plot width is total width - outer axis width (we don't want to overlap those)
            plot_width = width - (max_left_axis_width + max_right_axis_width + 2 * points_to_mm(10) / 10)
          )
      }

      lab_head$subtitle <-
        cowplot::ggdraw() +
        cowplot::draw_label(
          subtitle,
          fontface = "plain",
          x = 0.5,
          hjust = 0.5,
          vjust = 0.5,
          size = subtitle_text_size
        )
    }

    # footnotes and sources
    caption <- caption_wrap(
        footnotes = footnotes,
        sources = sources,
        max_char = 120,
        caption_wrap = F
      )

    if (!is.null(caption)) {

      if(auto_scale){
        caption <-
          rescale_text(
            text = caption,
            text_type = "caption",
            font_size = footer_text_size,
            # plot width including the left axis
            plot_width = width - (max_right_axis_width + points_to_mm(10) / 10)
          )
      }

      lab_foot$footer <-
        cowplot::ggdraw() +
        cowplot::draw_label(
          caption,
          x = 0,
          hjust = 0,
          vjust = 0.5,
          size = footer_text_size
        ) +
        theme(plot.margin = margin(t = 5, r = 0, b = 3, l = 5))
    }


    # Height adjustments ----------------------------------------------------

    # Work out the best height for the plot
    if(is.null(height)){

      # Calculate height taking into account the various adjustments
      height <- (known_height + panel_height) * nrow
    }

    # Space for title if required - size of text, plus a line of buffer (0.3cm), times the spacing adjustment
    if(is.null(title)){
      t_h <- 0

    } else if(!is.null(subtitle)){
      t_h <- (get_text_height(text = title, font_size = title_text_size) + 0.3) * title_spacing_adj

    # if there is no subtitle, remove the extra 0.3 padding
    } else {
      t_h <- (get_text_height(text = title, font_size = title_text_size)) * title_spacing_adj
    }

    # Space for subtitle if required - size of text, plus half a line of buffer (0.14cm), times the spacing adjustment
    if(!is.null(subtitle)){
      s_h <- (get_text_height(text = subtitle, font_size = subtitle_text_size) + 0.14) * subtitle_spacing_adj
    } else {
      s_h <- 0
    }

    # Adjust the footer height depending on how much text there is
    if(!is.null(caption)){
      f_h <- get_text_height(text = caption, font_size = footer_text_size) + 0.3
    } else {
      f_h <- 0
    }

    # calculate the total height and panel height
    p_h <- height
    tot_height <- (p_h + sum(t_h + s_h + f_h))

    if (t_h == 0) t_h <- NULL
    if (s_h == 0) s_h <- NULL
    if (f_h == 0) f_h <- NULL

    # Use automatically generated relative heights if the user does not specify their own
    if (is.null(rel_heights)) rel_heights <- c(t_h, s_h, p_h, f_h)

    gg <- cowplot::plot_grid(
      plotlist = c(lab_head, panels, lab_foot),
      ncol = 1,
      rel_heights = rel_heights
    )


    # Save the chart --------------------------------------------------------

    save_graph(graph = gg, format, filename, width, height = tot_height, pointsize, res)


    # Post-save functions -----------------------------------------------------

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
