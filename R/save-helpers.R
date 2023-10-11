# Helper functions that are used across save_single, save_multi or save_e61

#' Helper function to actually perform the saving functionality
#' @noRd
save_graph <- function(graph, format, filename, width, height) {
  lapply(format, function(fmt) {

    file_i <- paste0(filename, ".", fmt)

    # Create a temp name for png
    if (fmt == "png") file_temp <- tempfile(fileext = ".svg")

    switch(
      fmt,
      svg = svglite::svglite(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      eps = cairo_ps(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      pdf = cairo_pdf(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      # When saving PNG we save the SVG first then convert it to PNG
      png = svglite::svglite(filename = file_temp, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent")
    )

    print(graph)
    dev.off()

    # Save a PNG if required
    if (fmt == "png") {
      svg_to_png(file_temp, paste0(filename, ".png"), delete = TRUE)
    }

  })

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

      stop("Some elements of the plotlist are not ggplot objects. Check you have not supplied the wrong object or used an incorrect argument.")
    }
  }

  return(temp_list)
}

#' Get the correct plot width based on the chart type
#' @noRd
get_plot_width <- function(chart_type){

  # Set the maximum width based on the type of outputs
  if(chart_type == "MN"){

    max_width <- 18.59 # based on 215.9mm page width and 15mm margins either side

  } else if(chart_type == "RN"){

    max_width <- 13.985 # based on 338.7mm page width, 20mm margins, 15mm column sep and 2 columns (i.e. divide the remainder by 2)

  } else if(chart_type == "PPT"){

    max_width <- 31.32

  } else if(is.null(chart_type)){

    max_width <- 20

  } else {
    stop("Invalid chart type. Please select from one of the following: 'MN' for micronotes, 'RN' for research notes, 'PPT' for powerpoint slides, or leave blank to use default maximum widths")
  }

  return(max_width)
}

#' Get the base size of the plot
#' @noRd
get_base_size <- function(chart_type, plot_base_size = 10){

  # update the base size if the chart is not for a micronote
  if(chart_type == "RN"){

    plot_base_size <- plot_base_size * get_plot_width("RN") / get_plot_width("MN")

  } else if(chart_type == "PPT"){

    plot_base_size <- plot_base_size * get_plot_width("PPT") / get_plot_width("MN")

  } else {
    plot_base_size <- plot_base_size * 20 / get_plot_width("MN")
  }

  return(plot_base_size)
}
