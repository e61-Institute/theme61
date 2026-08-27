#' Save a single-panel chart with e61 formatting
#' @noRd
save_single <- function(
    filename,
    plot,
    chart_type,
    auto_scale, # manual control over whether y-axis is scaled
    width, # manual control over the width of the chart
    height, # manual control over the height of the chart
    max_height, # manual control over the maximum height of the chart
    format,
    base_size,
    pad_width,
    pad_height,
    bg_colour,
    print_label_positions = FALSE,
    fast_labels = FALSE
) {

  # Several plot + theme(...) merges below (e.g. update_margins(),
  # resolve_aspect_ratio()) can silently open the session's default device on
  # Windows -- guard the whole function rather than each call site.
  t61_with_device({

  # Plot is assumed pre-classified and prepared by save_e61()
  is_spatial_chart <- inherits(plot, "e61_map")

  # Discrete y-axis (e.g. ridgeline)
  discrete_y <- has_discrete_y_scale(plot)

  # Set maximum width based on output type
  if (is.null(chart_type)) chart_type <- "normal"
  max_width <- 18.59

  # update the base size without removing the legend
  legendTitle <- plot@theme$legend.title
  legendPosition <- plot@theme$legend.position

  # Maps already have their axis chrome/gridlines corrected by
  # finalise_e61_plot() (see classify-plots.R), so there's nothing left to
  # do here - the text sizing/margin logic below is for non-map plots only.
  if (!is_spatial_chart) {

    resolved_size <- resolve_text_size(plot, base_size)
    plot <- resolved_size$plot
    base_size <- resolved_size$base_size

    plot <- plot + update_margins(current_theme = plot@theme,
                                  base_size = base_size,
                                  legend_title = legendTitle)

    if(!is.null(legendPosition)){
      plot <- plot + theme(legend.position = legendPosition)
    }
  }

  # Update plot background --------------------------------------------------

  plot <- plot + theme(rect = element_rect(fill = bg_colour))

  # Update the aspect ratio -------------------------------------------------

  # Only keep one chart if a list has been supplied
  if(length(chart_type) > 1) {

    chart_type <- chart_type[1]
    warning(paste0("Multiple chart types supplied, using first in list, which is: ", chart_type, "."))
  }

  # override chart type if the graph is a map
  if (is_spatial_chart) chart_type <- "custom"

  plot <- resolve_aspect_ratio(plot, chart_type)

  # Update y-axis limits ----------------------------------------------------

  # update the chart scales if this is an auto_scaled chart
  if(auto_scale && !discrete_y) plot <- update_scales(plot, auto_scale)

  # Get the number of panel rows and columns ------------------------------

  # Get facet dimensions if applicable
  if (length(plot@facet$params) != 0) {

    # Only build when actually needed - most theme61 charts are single-panel
    plot_build <- ggplot_build(plot)

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

    # When coord_flip() is used to make a plot horizontal, the default dims
    # are too small. (The flipped-coord theme changes themselves are already
    # applied by finalise_e61_plot() before save_single() is ever called.)
    if (isTRUE("CoordFlip" %in% class(plot@coordinates))) {

      width <- max_width
      max_panel_width <- max_width / 2 # only allow the panel to be at most half the column consistent with other chart types

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
  p <- t61_ggplotGrob_quiet_na(plot)

  # allow charts to be the width of the panels
  right_axis_width <- get_grob_width(p, grob_name = "axis-r")
  left_axis_width <- get_grob_width(p, grob_name = "axis-l")

  # Plot margin also eats into the panel's free width but isn't an axis grob -
  # omitting it overstates free_wd, which (since the panel is aspect-locked)
  # makes grid shrink the panel below budget and centre the leftover as
  # unaccounted blank space.
  known_wd <- right_axis_width + left_axis_width + get_margin_dim(p, "width")

  tot_panel_width <- width - known_wd

  # check that the total panel width isn't over the maximum, again this is only an issue for the coord flipped charts
  if(tot_panel_width > max_panel_width) tot_panel_width <- max_panel_width

  # update the width after this check
  width <- tot_panel_width + known_wd

  plot <- update_labs(plot, width)

  if(!is_spatial_chart){

    # update the plot_labels
    plot <- update_plot_label(plot, chart_type, base_size)

  }


  # Height adjustments ----------------------------------------------------

  if(is.null(height)){

    # Step 1 - Get the amount of free height and width we have to play with (what is not already used up by the set elements)
    p <- t61_ggplotGrob_quiet_na(plot)

    known_ht <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))

    # Re-measure known_wd from this same, post-update_labs()/update_plot_label()
    # grob. The known_wd computed further up is measured before those calls
    # mutate the plot; pairing that stale width budget with an up-to-date
    # known_ht here would feed the aspect-ratio lock inconsistent numbers,
    # which it silently resolves by squeezing/inflating margins instead of
    # erroring.
    known_wd <- get_grob_width(p, grob_name = "axis-r") +
      get_grob_width(p, grob_name = "axis-l") +
      get_margin_dim(p, "width")

    # calculate the total free width and height we have to play with
    if(is.null(max_height)) max_height <- 100

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

  # Add width padding - note it comes in mm and we will want to convert to cm
  width <- width + pad_width / 10
  height <- height + pad_height / 10

  # Auto-position eligible plot_label() text now that the final chart size
  # is known (see autolabel-apply.R). No-ops when there are no eligible
  # labels.
  plot <- t61_apply_autolabel(plot, width_cm = width, height_cm = height,
                              print_positions = print_label_positions, fast = fast_labels)

  # Return objects needed to save the graph ----
  retval <- list(graph = plot,
                 width = width,
                 height = height)

  return(retval)

  })

}
