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
           outer_width, # manual control over the outer left/right figure margin
           outer_height, # manual control over the outer top/bottom figure margin
           height_adj, # adjust the vertical spacing of the mpanel charts
           ncol,
           nrow,
           align,
           axis,
           rel_heights,
           bg_colour,
           print_label_positions = FALSE
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

    # Identify how much padding to put between charts - applied to each panel
    # below so that title/subtitle/caption wrapping widths (measured before
    # the panels are combined) are calculated against the same margin that
    # ends up in the final render, rather than theme_e61()'s default margin
    chart_width_pad <- points_to_mm(5.5) + pad_width * 10 # Convert width padding back to mm for now
    chart_height_pad <- points_to_mm(5.5) + pad_height * 10

    # outer_width/outer_height override the margin at the outer edge of the
    # whole figure (as opposed to pad_width/pad_height, which only add space
    # *between* panels). NULL (the default) keeps the built-in margin, which
    # is 0 - tested against every sample-graphs/label_wrapping stress case
    # (long titles/subtitles/footnotes, all panel layouts) with no clipping.
    # Kept in both mm (for the per-panel plot.margin, which is set in mm) and
    # points (for the title/subtitle/caption plot_annotation margins below,
    # which - like the rest of ggplot2 - are in points) so neither needs a
    # unit conversion at the point of use.
    outer_height_mm <- if (is.null(outer_height)) 1 else outer_height
    outer_height_pt <- if (is.null(outer_height)) mm_to_points(1) else mm_to_points(outer_height)
    outer_width_mm <- if (is.null(outer_width)) 1 else outer_width

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

      # apply the same inter-panel margin used in the final render now, so
      # that title/subtitle/caption wrap widths are measured against it
      # instead of theme_e61()'s (much smaller) default plot.margin
      temp_plot <- temp_plot +
        theme(plot.margin = margin(t = chart_height_pad, b = chart_height_pad,
                                   r = chart_width_pad, l = chart_width_pad, unit = "mm"))

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

      p <- t61_ggplotGrob_quiet_na(temp_plot)

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

    base_margin_h <- outer_height_mm
    base_margin_w <- outer_width_mm

    # calc nrow so the per-panel margin - and therefore known_height, measured
    # from each panel's own rendered grob below - reflects the margin the panel
    # will actually be given, rather than theme_e61()'s own default plot.margin
    if (is.null(nrow)) {
      nrow <- ceiling(length(plots) / ncol)
    }

    # Update the labels -------------------------------------------------------

    if(auto_scale){

      # Keep track of the plot heights as we go
      known_height <- 0

      for(i in seq_along(clean_plotlist)){

        temp_plot <- clean_plotlist[[i]]

        # update labels - the wrap limit is this panel's share of width and
        # axes. Deliberately excludes chart_width_pad: that margin isn't
        # available to this panel's own text (it's outside the panel's
        # rendered cell). 0.9 buffer matches internal_width below - text
        # width is measured on a throwaway device that doesn't necessarily
        # use the exact font metrics of the final render, so wrapping against
        # the full available width leaves no room for that mismatch and can
        # let a panel's own title/subtitle bleed into the next panel.
        temp_plot <- update_labs(temp_plot, 0.9 * (panel_width + known_width / ncol))

        # update any plot label sizes
        temp_plot <- update_plot_label(temp_plot, chart_type, panel_base_sizes[i])

        # ensure pad_width/pad_height only measure space between panels: the
        # outer edges of the whole grid (left of column 1, right of the last
        # column, above row 1, below the last row)
        row_i <- ceiling(i / ncol)
        col_i <- ((i - 1) %% ncol) + 1

        top_i <- if (row_i == 1) base_margin_h else chart_height_pad
        bottom_i <- if (row_i == nrow) base_margin_h else chart_height_pad
        left_i <- if (col_i == 1) base_margin_w else chart_width_pad
        right_i <- if (col_i == ncol) base_margin_w else chart_width_pad

        temp_plot <- temp_plot +
          theme(plot.margin = margin(t = top_i, b = bottom_i, r = right_i, l = left_i, unit = "mm"))

        # save the plot
        clean_plotlist[[i]] <- temp_plot

        # Calculate the known height of the chart
        p <- t61_ggplotGrob_quiet_na(temp_plot)

        temp_height <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))

        known_height <- pmax(known_height, temp_height)
      }
    }

    # Auto-position eligible plot_label() text on each panel now that its
    # final size within the grid is known (every panel shares the same
    # panel_width/panel_height and aggregated axis/title overhead). Uses
    # the smaller, uniform margin the combined chart actually renders with
    # (not theme_e61()'s larger per-panel default), since positioning
    # against the wrong margin could place a label on real content once
    # the real panel renders.
    #
    # panel_total_width/height reconstruct each panel's total footprint
    # (panel + axis + margin) from the same pieces tot_width/ncol and
    # (p_h)/nrow use further below -- those aren't available yet here.
    panel_total_width  <- panel_width + max_left_axis_width + max_right_axis_width + 2 * chart_width_pad / 10
    panel_total_height <- panel_height + known_height + 2 * chart_height_pad / 10

    for (i in seq_along(clean_plotlist)) {
      clean_plotlist[[i]] <- clean_plotlist[[i]] +
        theme(plot.margin = margin(t = chart_height_pad, b = chart_height_pad,
                                   r = chart_width_pad, l = chart_width_pad, unit = "mm"))
      clean_plotlist[[i]] <- t61_apply_autolabel(
        clean_plotlist[[i]], width_cm = panel_total_width, height_cm = panel_total_height,
        print_positions = print_label_positions
      )
    }


    # Gather the plots ----------------------------------------------------

    plots <- clean_plotlist

    # Only needed when auto_scale is FALSE: the loop above already applied
    # this exact margin to every panel when auto_scale is TRUE, so re-running
    # it here would just repeat the same theme merge for no visual effect.
    if (!auto_scale) {
      for (i in seq_along(plots)) {
        row_i <- ceiling(i / ncol)
        col_i <- ((i - 1) %% ncol) + 1

        top_i <- if (row_i == 1) base_margin_h else chart_height_pad
        bottom_i <- if (row_i == nrow) base_margin_h else chart_height_pad
        left_i <- if (col_i == 1) base_margin_w else chart_width_pad
        right_i <- if (col_i == ncol) base_margin_w else chart_width_pad

        plots[[i]] <- plots[[i]] +
          theme(plot.margin = margin(t = top_i, b = bottom_i, r = right_i, l = left_i, unit = "mm"))
      }
    }

    # Create the main chart
    multi_plot <- patchwork::wrap_plots(
        plots,
        ncol = ncol,
        nrow = nrow
      )

    # Update width/height to take into account margins actually applied
    # above: chart_width_pad/chart_height_pad only on the interior edges
    # between panels ((ncol - 1) column gaps, (nrow - 1) row gaps), plus the
    # baseline margin on the outer edges of the grid.
    tot_width_pad <- (2 * base_margin_w + (ncol - 1) * 2 * chart_width_pad) / 10
    tot_height_pad <- (2 * base_margin_h + (nrow - 1) * 2 * chart_height_pad) / 10

    # Interior width available to the title/subtitle/caption text: total
    # width minus the (baseline-only) outer margins, since those aren't
    # usable content space either.
    tot_width <- width + tot_width_pad
    # 0.9 buffer keeps title/subtitle/caption text clear of the panel axes
    internal_width <- 0.9 * (tot_width - 2 * base_margin_w / 10)


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
              family = theme61_settings$text$family,
              hjust = 0,
              vjust = 0.5,
              margin = margin(t = 5.5, b = title_subtitle_spacing, l = 0, r = 0)
            ),
            # patchwork sizes this row with a throwaway ggplot using its own
            # default plot.margin, adding columns outside where the text is
            # placed. Zero out l/r (keeping t/b, used by the height
            # calculations below) so the text can use the full internal_width.
            plot.margin = margin(t = outer_height_pt, r = 0, b = outer_height_pt, l = 0)
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
              family = theme61_settings$text$family,
              hjust = 0,
              vjust = 0.5,
              margin = margin(t = 0, b = subtitle_charts_spacing, l = 0, r = 0)
            ),
            plot.margin = margin(t = outer_height_pt, r = 0, b = outer_height_pt, l = 0)
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
              family = theme61_settings$text$family,
              hjust = 0,
              vjust = 0.5,
              margin = margin(b = 5.5, t = caption_spacing, l = 0, r = 0)
            ),
            plot.margin = margin(t = outer_height_pt, r = 0, b = outer_height_pt, l = 0)
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
      t_h <- get_text_height(text = title, font_size = title_text_size) + points_to_mm(title_subtitle_spacing) / 10 + outer_height_mm / 10
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
      f_h <- get_text_height(text = caption, font_size = footer_text_size) + points_to_mm(caption_spacing) / 10 + outer_height_mm / 10
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
