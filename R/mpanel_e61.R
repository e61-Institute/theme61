#' Save a multi-panel chart with e61 formatting
#' @noRd

save_mpanel_e61 <-
  function(filename,
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
           title_adj = 1,
           title_spacing_adj = 1, # adjust the amount of space given to the title
           subtitle_spacing_adj = 1, # adjust the amount of space given to the subtitle
           base_size = 8, # set the base size for the theme61 font size call
           height_adj = 1, # adjust the vertical spacing of the mpanel charts
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


    # Set width -------------------------------------------------------------

    # check whether the user has supplied a given width first (i.e. different to the default 8.5cm)
    if(is.null(width)) {

      # If it's only one panel, set the chart width to 2/3 of the max-width
      if(ncol == 1){

        width <- 2/3 * max_width

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
    known_width <- 0
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
      if(auto_scale){

        # check if the y-var is numeric
        y_var_name <- ggplot2::quo_name(temp_plot$mapping$y)
        y_var_class <- temp_plot$data[[y_var_name]] %>% class()

        if(y_var_name == "NULL"){

          layers <- temp_plot$layers

          for(j in seq_along(layers)){

            # don't get y-aesthetic for geom_text objects
            layer_type <- layers[[j]]$geom %>% class()

            if("GeomText" %in% layer_type) next

            # otherwise get the y-variable name and type
            y_var_name <- ggplot2::quo_name(layers[[j]]$mapping$y)

            if(y_var_name == "NULL") next

            y_var_class <- temp_plot$data[[y_var_name]] %>% class()

            # if we found one numeric class, break because that all we need
            if(y_var_class == "numeric") break
          }

        } else {
          y_var_class <- temp_plot$data[[y_var_name]] %>% class()
        }

        # if one of the y-variables is numeric, adjust the y-axis scale
        if(y_var_class == "numeric"){

          # first check if we want to include a second y-axis or not (check by looking at whether it has a non-zero width grob)
          grobs <- ggplot2::ggplotGrob(temp_plot)

          test_sec_axis <- get_grob_width(grobs, grob_name = "axis-r")

          # add a second axis if there is already one present
          sec_axis <- !(is.null(test_sec_axis) | test_sec_axis == 0)

          # then update the chart scales
          suppressMessages({temp_plot <- update_chart_scales(temp_plot, auto_scale, sec_axis)})

          # if the y-var class is NULL, send a warning message about the auto updating of chart scales
        } else if(y_var_class == "NULL" & warn == F){

          warning("Could not identify the class of the y variable. This prevents the y-axis scales from being automatically updated to aesthetic values. To address this issue check that you have not edited the variable within your ggplot call (e.g. aes(y = 100 * var)). Instead make any changes before passing the dataset to ggplot (e.g. data %>% mutate(new_var = 100 * var) %>% ggplot(...)).")
          warn <- T
        }

        # update the titles and subtitles of the plots
        suppressMessages({temp_plot <- update_labs(plot = temp_plot, is_mpanel = F, plot_width = (0.95 * width) / ncol)})
      }

      # save the plot
      clean_plotlist[[i]] <- temp_plot


      # Calculate the known width of the chart ----

      p <- ggplot2::ggplotGrob(temp_plot)

      widths <- grid::convertWidth(p$widths, "cm", valueOnly = TRUE)
      temp_width <- sum(widths)

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

      # add the total known width for the first row
      if(i <= ncol) known_width <- known_width + temp_width

      if(is.null(max_left_axis_width) | length(max_left_axis_width) == 0) max_left_axis_width <- 0
      if(is.null(max_right_axis_width) | length(max_right_axis_width) == 0) max_right_axis_width <- 0


      # Calculate the known height of the chart ---------------------------------

      # if we're in the first column, add the known height
      if(i %% ncol == 1 | ncol == 1){
        temp_height <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))

        known_height <- known_height + temp_height
      }
    }

    if(max_panel_asps == 0) max_panel_asps <- 0.75


    # Update the y-axis scales ------------------------------------------------

    if(auto_scale){

      for(i in seq_along(clean_plotlist)){

        temp_plot <- clean_plotlist[[i]]

        suppressMessages({
          temp_plot <-
            update_y_axis_labels(temp_plot,
                                 max_y_lab = y_lab_max_size,
                                 max_break_width = max_break_width)
        })

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
    title_text_size <- base_size * 1.25 * title_adj
    subtitle_text_size <- base_size * 1.125 * title_adj
    footer_text_size <- base_size - 1

    # title
    if(!is.null(title)){

      if(auto_scale){

        title <-
          rescale_text(
            text = title,
            text_type = "title",
            font_size = title_text_size,
            # plot width is total width
            plot_width = width * 0.95
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
            plot_width = width - (max_left_axis_width + max_right_axis_width)
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
    caption <- label_wrap(
        footnotes = footnotes,
        sources = sources,
        footnote_max_char = 120,
        footnote_wrap = F
      )$caption

    if (!is.null(caption)) {

      if(auto_scale){
        caption <-
          rescale_text(
            text = caption,
            text_type = "caption",
            font_size = footer_text_size,
            # plot width including the left axis
            plot_width = width - max_right_axis_width
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
        ggplot2::theme(plot.margin = margin(t = 5, r = 0, b = 3, l = 5))
    }


    # Height adjustments ----------------------------------------------------

    # Work out the best height for the plot
    if(is.null(height)){

      # calculate the free width and height we have to play with
      if(!is.null(max_height)) {
        free_ht <- max_height - known_height

      } else {
        free_ht <- 100 - known_height
      }

      free_wd <- width - known_width

      # Divide the free width by the number of columns (panels) we have
      panel_width <- free_wd / ncol # width of each panel
      panel_height <- panel_width * max_panel_asps # height of each panel (width * aspect ratio)

      # Calculate height taking into account the various adjustments
      height <- (known_height + panel_height) * nrow
    }

    # TODO - fix this crude height adjustment
    if(is.null(height_adj)){
      if(nrow == 1) {
        height_adj <- 1.15

      } else if(nrow == 2) {
        height_adj <- 0.90

      } else if(nrow >= 3) {
        height_adj <- 0.75
      }
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
    tot_height <- (p_h + sum(t_h + s_h + f_h)) * height_adj

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

    # Save the mpanel --------------------------------------------------------

    lapply(format, function(fmt) {

      file_i <- paste0(filename, ".", fmt)

      switch(
        fmt,
        svg = svglite::svglite(filename = file_i, width = cm_to_in(width), height = cm_to_in(tot_height), bg = "transparent"),
        eps = cairo_ps(filename = file_i, width = cm_to_in(width), height = cm_to_in(tot_height), bg = "transparent"),
        pdf = cairo_pdf(filename = file_i, width = cm_to_in(width), height = cm_to_in(tot_height), bg = "transparent"),
        png = png(filename = file_i, width = width, height = tot_height, units = "cm", pointsize = pointsize, res = res, bg = "transparent")
      )

      print(gg)
      dev.off()
    })
}

#' Create a multi-panel graph with e61 formatting
#'
#' @description Wrapper around \link[cowplot]{plot_grid} and \code{labs_e61} to
#'   create multi-panel graphs with appropriate title and footer formatting.
#'
#'   This function is designed for creating 2x2 panel graphs, although it should
#'   work for any arrangement of panels (e.g. 2x1, 3x2, etc.). Your mileage may
#'   vary.
#'
#'   When saving multi-panel graphs make sure you use \code{save_mpanel_e61}.
#'
#' @details \strong{Read this if your titles and stuff are getting cut off.}
#'
#'   The function tries to be smart when setting the values for
#'   \code{rel_heights}, but it can be incorrect due to differences in the
#'   amount of text in each component.
#'
#'   The way \code{mpanel_e61} works is that the title, subtitle, graph panels
#'   and footers are all separate components that are then put together after
#'   they have been independently generated. The purpose of \code{rel_heights}
#'   is to specify what proportion of the final plot each of these components
#'   require. As a result, if the values are too small for a given component,
#'   part of it will get visually cut off.
#'
#'   The function tries to increase the amount of space given to a component if,
#'   for example, your title has two lines, or you write a really long footnote
#'   that spans many lines of text. But this is an inexact science. In all
#'   likelihood, you will need to specify your own values, which why this
#'   paragraph of text tries to explain the underlying function so you aren't
#'   just blindly inputting numbers.
#'
#'   The default values for \code{rel_heights} for a 1-line title, 1-line
#'   subtitle, 1-line footnote and 1-line sources list is \code{c(0.05, 0.05, 1,
#'   0.1)}. If you only have a title and footnotes/sources then only 3 values
#'   are needed: \code{c(0.05, 1, 0.1)}.
#'
#'   These values are all relative ratios, meaning, in the second example, the
#'   title gets \eqn{\frac{0.05}{1.15}}, or around 4 per cent of the total graph
#'   height. This means that if your graph panels are really tall (e.g. you make
#'   a 3x2 multi-panel), you will need to reduce the share of the space
#'   allocated to the titles or you will have extra whitespace (I think? Haven't
#'   actually tested this).
#'
#' @param ... Plot objects to put on the panel.
#' @inheritParams labs_e61
#' @inheritParams cowplot::plot_grid
#' @param title_adj Rescales the size of the title text to be slightly larger
#'   than the titles of the subplots (default is 1.1). 2 doubles the font size.
#' @param rel_heights A numeric vector giving the relative proportions of each
#'   graph component (title, plots, footer (optional)). See the Details for more
#'   detail.
#' @param auto_scale Logical. Should the y-axis of the charts be scaled manually. Default is TRUE.
#' @param show_height Logical. Prints a message showing the \code{rel_heights}
#'   used when producing the graph. Mostly used for testing. Defaults to FALSE.
#'
#' @return ggplot2 object
#' @export
#' @examples
#'  gg <- ggplot2::ggplot() +
#'    labs_e61(title = "Figure", y = "%") +
#'    scale_y_continuous_e61(limits = c(0, 10, 2.5)) +
#'    theme_e61()
#'
#'  mpanel_e61(gg, gg, gg, gg,
#'    title = "Multi-panel graph title",
#'    subtitle = "Graph subtitle",
#'    footnotes = c("Footnote 1", "Footnote 2"),
#'    sources = c("Source 1", "Source 2"))
mpanel_e61 <-
  function(...,
           plotlist = NULL,
           title = NULL,
           subtitle = NULL,
           footnotes = NULL,
           sources = NULL,
           title_max_char = 100,
           subtitle_max_char = 120,
           footnote_max_char = 140,
           title_wrap = TRUE,
           subtitle_wrap = TRUE,
           footnote_wrap = TRUE,
           auto_scale = TRUE,
           title_adj = 1.1,
           ncol = 2,
           nrow = NULL,
           rel_heights = NULL,
           align = c("v", "none", "h", "hv"),
           axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
           show_height = FALSE
  ) {

    # for each plot update the y-axis scales
    plots <- c(list(...), plotlist)

    clean_plotlist <- list()

    tot_height <- 0
    tot_width <- 0

    for(i in seq_along(plots)){

      temp_plot <- plots[[i]]

      # check if the y-var is numeric
      y_var_name <- ggplot2::quo_name(temp_plot$mapping$y)
      y_var_class <- temp_plot$data[[y_var_name]] %>% class()

      if(y_var_name == "NULL"){

        layers <- temp_plot$layers

        for(j in seq_along(layers)){

          # don't get y-aesthetic for geom_text objects
          layer_type <- layers[[j]]$geom %>% class()

          if("GeomText" %in% layer_type) next

          # otherwise get the y-variable name and type
          y_var_name <- ggplot2::quo_name(layers[[j]]$mapping$y)

          if(y_var_name == "NULL") next

          y_var_class <- temp_plot$data[[y_var_name]] %>% class()

          # if we found one numeric class, break because that all we need
          if(y_var_class == "numeric") break
        }

      } else {
        y_var_class <- temp_plot$data[[y_var_name]] %>% class()
      }

      # if one of the y-variables is numeric, adjust the y-axis scale
      if(y_var_class == "numeric"){
        temp_plot <- update_y_axis_labels(temp_plot)
      }

      # save the plot
      clean_plotlist[[i]] <- temp_plot

      # save the known width and height of the chart
      p <- ggplot2::ggplotGrob(temp_plot)

      known_width <- sum(grid::convertWidth(p$widths, "cm", valueOnly = TRUE), na.rm = T)
      known_height <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE), na.rm = T)

      tot_width <- tot_width + known_width
      tot_height <- tot_height + known_height
    }

    # Prep header/footer text
    lab_text <- label_wrap(
      title = title,
      subtitle = subtitle,
      footnotes = footnotes,
      sources = sources,
      title_max_char = title_max_char,
      subtitle_max_char = subtitle_max_char,
      footnote_max_char = footnote_max_char,
      title_wrap = title_wrap,
      subtitle_wrap = subtitle_wrap,
      footnote_wrap = footnote_wrap
    )

    # Gather the plots
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

    lab_head$title <-
      cowplot::ggdraw() +
      cowplot::draw_label(
        lab_text$title,
        fontface = "bold",
        x = 0.5,
        hjust = 0.5,
        size = 11.5 * title_adj
      )

    if (!is.null(lab_text$subtitle)) {
      lab_head$subtitle <-
        cowplot::ggdraw() +
        cowplot::draw_label(
          lab_text$subtitle,
          fontface = "plain",
          x = 0.5,
          hjust = 0.5,
          size = 10 * title_adj
        )
    }

    if (!is.null(lab_text$caption)) {
      lab_foot$footer <-
        cowplot::ggdraw() +
        cowplot::draw_label(lab_text$caption,
                            x = 0,
                            hjust = 0,
                            size = 9) +
        ggplot2::theme(plot.margin = margin(0, 0, 0, 3))
    }

    # Space for title if required
    t_h <- if (!is.null(lab_text$title)) 0.07 else 0.001

    # Space for subtitle if required
    s_h <- if (!is.null(lab_text$subtitle)) 0.07 else NULL

    # Adjust the footer height depending on how much text there is
    f_h <-
      if (!is.null(lab_text$caption)) 0.06 * (n_count(lab_text$caption) + 1) else NULL

    # Calculate plot height
    p_h <- sum(t_h, s_h, 0.9, f_h) / 1.1
    p_h <- p_h * (1 + (nrow - 1) * 0.4) # Adjustment factor for >1 row plots

    # Use automatically generated relative heights if the user does not specify their own
    if (is.null(rel_heights)) rel_heights <- c(t_h, s_h, p_h, f_h)

    gg <- cowplot::plot_grid(
      plotlist = c(lab_head, panels, lab_foot),
      ncol = 1,
      rel_heights = rel_heights
    )

    # Add some extra info on the multi-panel attributes
    attr(gg, "panel_rows") <- nrow
    attr(gg, "panel_cols") <- ncol
    attr(gg, "panel_head") <- if (is.numeric(t_h) && is.numeric(s_h)) (t_h + s_h) / 0.07 else 0
    attr(gg, "panel_foot") <- if (is.numeric(f_h)) f_h / 0.06 else 0
    attr(gg, "plot_type") <- "mpanel"
    attr(gg, "known_width") <- tot_width
    attr(gg, "known_height") <- tot_height

    # Print the rel_heights used
    if (show_height) message(paste(rel_heights, collapse = ", "))

    return(gg)
  }
