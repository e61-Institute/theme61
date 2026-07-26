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
           auto_scale,
           title_spacing_adj, # adjust the amount of space given to the title
           subtitle_spacing_adj, # adjust the amount of space given to the subtitle
           base_size, # set the base size for the theme61 font size call
           pad_width,
           pad_height,
           height_adj, # adjust the vertical spacing of the mpanel charts
           ncol,
           nrow,
           align,
           axis,
           rel_heights,
           bg_colour
  ) {

    # Set width -------------------------------------------------------------

    default_width <- 18.59

    # check whether the user has supplied a given width first (i.e. different to the default 8.5cm)
    if(is.null(width)) {

      # If it's only one panel, set the chart width to 1/2 of the max-width
      if(ncol == 1){
        width <- 1/2 * default_width

      } else if(ncol == 2) {
        width <- default_width

      # Else use the default width times 1.5
      } else {
        width <- 1.5 * default_width
      }
    }

    # Update the pad_width units from mm to cm - mm easier to enter for the user by cm easier to work with
    pad_width <- pad_width / 10
    pad_height <- pad_height / 10

    # Format each plot in the plotlist and get dimensions ----------------------------------------

    # for each plot update the y-axis scales
    clean_plotlist <- list()

    # keep track of various aspects of the charts
    known_height <- 0
    max_panel_asps <- 0
    max_left_axis_width <- 0
    max_right_axis_width <- 0

    # track the effective text size used per panel, in case a panel has
    # already customised its own text size away from the theme_e61() default
    panel_base_sizes <- rep(base_size, length(plots))

    for(i in seq_along(plots)){

      temp_plot <- plots[[i]]

      # Update the aspect ratio
      if(length(chart_type) > 1) {
        chart_type_temp <- chart_type[i]

      } else {
        chart_type_temp <- chart_type
      }

      temp_plot <- resolve_aspect_ratio(temp_plot, chart_type_temp)

      # set the background colour
      temp_plot <- temp_plot + theme(rect = element_rect(fill = bg_colour))

      # check whether to apply the autoscaler or not
      if(auto_scale) {

        # update the scales to aesthetic values
        temp_plot <- update_scales(temp_plot, auto_scale)

        # update the text and margin sizes
        legend_title <- temp_plot@theme$legend.title
        legendPosition <- temp_plot@theme$legend.position

        resolved_size <- resolve_text_size(temp_plot, base_size)
        temp_plot <- resolved_size$plot
        panel_base_sizes[i] <- resolved_size$base_size

        temp_plot <- temp_plot + update_margins(current_theme = temp_plot@theme,
                                                base_size = panel_base_sizes[i],
                                                legend_title = legend_title)

        if(!is.null(legendPosition)){
          temp_plot <- temp_plot + theme(legend.position = legendPosition)
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

      # keep track of the max right axis and left axis widths as all charts are set to have the same dimensions
      right_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-r"), get_grob_width(p, grob_name = "axis-r"))
      max_right_axis_width <- pmax(max_right_axis_width, right_axis_width)

      left_axis_width <- pmax(get_grob_width(p, grob_name = "ylab-l"), get_grob_width(p, grob_name = "axis-l"))
      max_left_axis_width <- pmax(max_left_axis_width, left_axis_width)

      if(is.null(max_left_axis_width) || length(max_left_axis_width) == 0)
        max_left_axis_width <- 0

      if(is.null(max_right_axis_width) || length(max_right_axis_width) == 0)
        max_right_axis_width <- 0

      # Calculate the known height of the chart

      # take the known height as the maximum of all the chart heights
      t_ht <- get_grob_height(p, grob_name = "title")
      st_ht <- get_grob_height(p, grob_name = "subtitle")
      cap_ht <- get_grob_height(p, grob_name = "caption")

      xlt_ht <- get_grob_height(p, grob_name = "xlab-t")
      xlb_ht <- get_grob_height(p, grob_name = "xlab-b")

      axb_ht <- get_grob_height(p, grob_name = "axis-b")
      axt_ht <- get_grob_height(p, grob_name = "axis-t")

      gbxt_ht <- get_grob_height(p, grob_name = "guide-box-top")
      gbxb_ht <- get_grob_height(p, grob_name = "guide-box-bottom")

      temp_height <- sum(t_ht, st_ht, cap_ht, xlt_ht, xlb_ht, axb_ht, axt_ht, gbxt_ht, gbxb_ht)

      known_height <- pmax(known_height, temp_height)
    }

    # update the max panel asps ratio
    if(max_panel_asps == 0) max_panel_asps <- 0.75

    # update the known width of the plot - max left and right axis widths multiplied by the number of columns
    known_width <- (max_left_axis_width + max_right_axis_width) * ncol

    # calculate the width of each panel - remove the known width of axes and the padding of each chart
    free_wd <- width - known_width - ncol * pad_width

    # Divide the free width by the number of columns (panels) we have
    panel_width <- free_wd / ncol # width of each panel
    panel_height <- panel_width * max_panel_asps # height of the tallest panel (width * aspect ratio)


    # Update the labels -------------------------------------------------------

    if(auto_scale){

      # Keep track of the plot heights as we go
      known_height <- 0

      for(i in seq_along(clean_plotlist)){

        temp_plot <- clean_plotlist[[i]]

        # update labels - for each set the limit as width divided by the number of columns we have
        temp_plot <- update_labs(temp_plot, panel_width + known_width / ncol)

        # update any plot label sizes
        temp_plot <- update_plot_label(temp_plot, chart_type, panel_base_sizes[i])

        # save the plot
        clean_plotlist[[i]] <- temp_plot

        # Calculate the known height of the chart
        p <- ggplotGrob(temp_plot)

        temp_height <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))

        known_height <- pmax(known_height, temp_height)
      }
    }


    # Gather the plots ----------------------------------------------------

    plots <- clean_plotlist

    if (is.null(nrow)) {
      nrow <- ceiling(length(plots) / ncol)
    }

    # Identify how much padding to put between charts
    chart_width_pad <- points_to_mm(5.5) + pad_width * 10 # Convert width padding back to mm for now
    chart_height_pad <- points_to_mm(5.5) + pad_height * 10

    # Create the main chart
    multi_plot <- patchwork::wrap_plots(
        plots,
        ncol = ncol,
        nrow = nrow
      ) &
      theme(plot.margin = margin(t = chart_height_pad, b = chart_height_pad, r = chart_width_pad, l = chart_width_pad, unit = "mm"))

    # Update width to take into account margins - these are applied to every plot
    # in the same way so we need to scale for the number of columns and rows
    tot_width_pad <- ncol * 2 * chart_width_pad / 10
    tot_height_pad <- nrow * 2 * chart_height_pad / 10

    # Get the interior width. Previously this also subtracted patchwork's own
    # default annotation margin (2 * 5.5pt) on top of the panel margins
    # already folded into tot_width_pad, because patchwork sizes the title/
    # subtitle/caption row with a throwaway ggplot using ITS default
    # plot.margin, and places those grobs outside of it. We now zero out that
    # margin's l/r explicitly on the annotation theme (see below), so it no
    # longer needs to be subtracted here.
    tot_width <- width + tot_width_pad
    internal_width <- tot_width - 4 * pad_width


    # Prepare titles, subtitles etc. --------------------------------------

    # define text sizes based on theme61 settings
    theme61_settings <- theme_e61()

    # Access text size from specific elements
    title_text_size <- 14
    subtitle_text_size <- 12
    footer_text_size <- 8

    # Define spacing between title and subtitle, subtitle and charts, and charts and footnotes
    # note these are all in points (5.5. is the standard margin in ggplot)
    title_subtitle_spacing <- 5.5
    subtitle_charts_spacing <- 11
    caption_spacing <- 16.5

    # title
    if(!is.null(title)){

      if(auto_scale){

        title <-
          rescale_text_multi(
            text = title,
            text_type = "title",
            font_size = title_text_size,
            # plot width is just the internal width - we don't want titles and captions overlapping the outside axes
            plot_width = internal_width
          )
      }

      multi_plot <- multi_plot +
        patchwork::plot_annotation(
          title = title,
          theme = theme(
            plot.title = element_text(
              size = title_text_size,
              face = "bold",
              hjust = 0,
              vjust = 0.5,
              margin = margin(t = 5.5, b = title_subtitle_spacing, l = 0, r = 0)
            ),
            # patchwork sizes the title/subtitle/caption row using a throwaway
            # ggplot with its OWN default plot.margin, which becomes two extra
            # columns bracketing the whole composition that the title/caption
            # grobs are never placed into. Zero out the l/r margin (keeping
            # t/b, which the height calculations below rely on) so that
            # doesn't reserve horizontal space beyond what internal_width
            # already accounts for.
            plot.margin = margin(t = 5.5, r = 0, b = 5.5, l = 0)
          )
        )
    }

    # subtitle
    if(!is.null(subtitle)){

      if(auto_scale){
        subtitle <-
          rescale_text_multi(
            text = subtitle,
            text_type = "subtitle",
            font_size = subtitle_text_size,
            # plot width is just the internal width - we don't want titles and captions overlapping the outside axes
            plot_width = internal_width
          )
      }

      multi_plot <- multi_plot +
        patchwork::plot_annotation(
          subtitle = subtitle,
          theme = theme(
            plot.subtitle = element_text(
              size = subtitle_text_size,
              face = "plain",
              hjust = 0,
              vjust = 0.5,
              margin = margin(t = 0, b = subtitle_charts_spacing, l = 0, r = 0)
            ),
            plot.margin = margin(t = 5.5, r = 0, b = 5.5, l = 0)
          )
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
          rescale_text_multi(
            text = caption,
            text_type = "caption",
            font_size = footer_text_size,
            # plot width is just the internal width - we don't want titles and captions overlapping the outside axes
            plot_width = internal_width
          )
      }

      multi_plot <- multi_plot +
        patchwork::plot_annotation(
          caption = caption,
          theme = theme(
            plot.caption = element_text(
              size = footer_text_size,
              face = "plain",
              hjust = 0,
              vjust = 0.5,
              margin = margin(b = 5.5, t = caption_spacing, l = 0, r = 0)
            ),
            plot.margin = margin(t = 5.5, r = 0, b = 5.5, l = 0)
          )
        )
    }


    # Height adjustments ----------------------------------------------------

    # Work out the best height for the plot
    if(is.null(height)){

      # Calculate height taking into account the various adjustments
      height <- (known_height + panel_height) * nrow
    }

    # Space for title if required - size of text, plus a buffer based on the margin added above
    if(!is.null(title)){
      t_h <- get_text_height(text = title, font_size = title_text_size) + points_to_mm(title_subtitle_spacing) / 10 + points_to_mm(5.5) / 10
    } else {
      t_h <- 0
    }

    # Space for subtitle if required - size of text, plus a buffer based on the margin added above
    if(!is.null(subtitle)){
      s_h <- get_text_height(text = subtitle, font_size = subtitle_text_size) + points_to_mm(subtitle_charts_spacing) / 10
    } else {
      s_h <- 0
    }

    # Adjust the footer height depending on how much text there is
    if(!is.null(caption)){
      f_h <- get_text_height(text = caption, font_size = footer_text_size) + points_to_mm(caption_spacing) / 10 + points_to_mm(5.5) / 10
    } else {
      f_h <- 0
    }

    # calculate the total height and panel height
    p_h <- height
    tot_height <- (p_h + sum(t_h + s_h + f_h))

    # Return objects needed to save the graph ----
    multi_plot <- multi_plot &
      theme(plot.background = element_rect(fill = "transparent",
                                           colour = "transparent"),
            legend.background = element_rect(fill = "transparent",
                                             colour = "transparent")
      )

    # Update height to also take into account margins
    tot_height <- tot_height + tot_height_pad

    # Save values to return
    retval <- list(graph = multi_plot,
                   width = tot_width,
                   height = tot_height)

    return(retval)
  }
