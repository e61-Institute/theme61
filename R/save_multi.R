#' Save a multi-panel chart with e61 formatting
#' @noRd
save_multi <-
  function(filename,
           format,
           plots,
           chart_type,
           title,
           subtitle,
           footnotes,
           sources,
           width, # manual control over the width of the chart
           height, # manual control over the height of the chart
           max_height, # manual control over the maximum height of the chart
           auto_scale,
           title_spacing_adj, # adjust the amount of space given to the title
           subtitle_spacing_adj, # adjust the amount of space given to the subtitle
           base_size, # set the base size for the theme61 font size call
           height_adj, # adjust the vertical spacing of the mpanel charts
           ncol,
           nrow,
           align,
           axis,
           rel_heights,
           bg_colour
           ) {

    # Set maximum width based on output type ----------------------------------

    if(is.null(chart_type)) chart_type <- "MN"

    max_width <- get_plot_width(chart_type)
    max_height <- get_plot_width(chart_type)

    # Update the base size based on the type of chart (MN, RN etc.) being produced
    base_size <- base_size * max_width / get_plot_width("MN")


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

    for(i in seq_along(plots)){

      temp_plot <- plots[[i]]

      # set the background colour
      temp_plot <- temp_plot + theme(rect = element_rect(fill = bg_colour))

      # check whether to apply the autoscaler or not
      if(auto_scale) {

        # update the scales to aesthetic values
        temp_plot <- update_scales(temp_plot, auto_scale)

        # update the text and margin sizes
        legend_title <- temp_plot$theme$legend.title
        legend_position <- temp_plot$theme$legend.position

        temp_plot <- temp_plot + theme(text = element_text(size = base_size))

        temp_plot <- temp_plot + update_margins(base_size = base_size, legend_title = legend_title)

        if(!is.null(legend_position)){
          temp_plot <- temp_plot + theme(legend.position = legend_position)
        }
      }

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

        # update y-axis labels - if the y-axis labels are set to the top
        suppressMessages({
          if(isFALSE(attr(temp_plot$theme, "no_y_top")))
            temp_plot <- update_y_axis_labels(temp_plot, max_break_width, y_lab_max_size)
        })

        # update labels - for each set the limit as width - knowwidth (axis labels etc.) divided by the number of columns we have
        temp_plot <- update_labs(temp_plot, panel_width)

        # update any plot label sizes
        temp_plot <- update_plot_label(temp_plot, chart_type, base_size)

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

    # Return objects needed to save the graph ----
    retval <- list(graph = gg,
                   width = width,
                   height = tot_height)

    return(retval)

}
