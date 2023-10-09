# Helper functions that are used across save_single, save_multi or save_e61


#' Enforce file format requirements if a file extension is provided
#' @noRd
save_guard <- function(filename) {
  if (grepl("\\..{3}$", filename) && !grepl("\\.(png|svg|pdf|eps)$", filename)) {
    stop("You must provide a file extension. Only PDF, SVG and PNG file formats are currently supported.")
  }
}

#' Helper function to actually perform the saving functionality
#' @noRd
save_graph <- function(graph, format, filename, width, height, pointsize, res) {
  lapply(format, function(fmt) {

    file_i <- paste0(filename, ".", fmt)

    switch(
      fmt,
      svg = svglite::svglite(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      eps = cairo_ps(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      pdf = cairo_pdf(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = "transparent"),
      png = png(filename = file_i, width = width, height = height, units = "cm", pointsize = pointsize, res = res, bg = "transparent")
    )

    print(graph)
    dev.off()
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
      warning("Some elements of the plotlist are not ggplot objects. Check you have not supplied the wrong object or used an incorrect argument.")
    }
  }

  return(temp_list)
}

#' Get the correct plot width, base text size and maximum height based on the chart type
#' @noRd
get_plot_dims <- function(chart_type, max_height = 20){

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

  if(is.null(max_height)) max_height <- 100

  return(list("max_width" = max_width, "max_height" = max_height))
}

#' Get the base size of the plot
#' @noRd
get_base_size <- function(chart_type, plot_base_size = 10){

  # update the base size if the chart is not for a micronote
  if(chart_type == "RN"){

    plot_base_size <- plot_base_size * get_plot_dims("RN")$max_width / get_plot_dims("MN")$max_width

  } else if(chart_type == "PPT"){

    plot_base_size <- plot_base_size * get_plot_dims("PPT")$max_width / get_plot_dims("MN")$max_width

  } else {
    plot_base_size <- plot_base_size * 20 / get_plot_dims("MN")$max_width
  }

  return(plot_base_size)
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
