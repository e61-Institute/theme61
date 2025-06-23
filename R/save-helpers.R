# Helper functions that are used across save_single, save_multi or save_e61

#' Helper function to actually perform the saving functionality
#' @noRd
save_graph <- function(graph, format, filename, width, height, bg_colour, res) {
  lapply(format, function(fmt) {

    file_i <- paste0(filename, ".", fmt)

    # Create a temp name for png/jpg
    if (fmt == "png" | fmt == "jpg") file_temp <- tempfile(fileext = ".svg")

    # add very slight width buffer
    width <- width + 0.1

    switch(
      fmt,
      svg = svglite::svglite(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = bg_colour),
      eps = cairo_ps(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = bg_colour),
      pdf = cairo_pdf(filename = file_i, width = cm_to_in(width), height = cm_to_in(height), bg = bg_colour),
      # When saving PNG or JPEG we save the SVG first then convert it to PNG or JPEG
      png = svglite::svglite(filename = file_temp, width = cm_to_in(width), height = cm_to_in(height), bg = bg_colour),
      jpg = svglite::svglite(filename = file_temp, width = cm_to_in(width), height = cm_to_in(height), bg = bg_colour)
    )

    print(graph)
    dev.off()

    # Save a png/jpg if required
    if (fmt == "png") {
      svg_to_bitmap(file_temp, paste0(filename, ".png"), delete = TRUE, res = res)

    } else if(fmt == "jpg") {
      svg_to_bitmap(file_temp, paste0(filename, ".jpg"), delete = TRUE, res = res)
    }
  })
}

#' Check plots are ggplot objects and return a list of only ggplot objects
#' @noRd
check_plots <- function(plots){

  temp_list <- list()

  for(i in seq_along(plots)){
    temp_plot <- plots[[i]]

    if(is_ggplot(temp_plot)) {
      temp_list[[length(temp_list) + 1]] <- temp_plot
    } else {

      stop(paste0(temp_plot, " is not a valid save_e61() argument. Check that you have not supplied the wrong object to save_e61() or used an incorrect or outdated argument (use ?save_e61 to view valid arguments)."))
    }
  }

  return(temp_list)
}


#' Replication of testthat::is_testing() so we can turn off some functionality
#' in the test env.
#' @noRd
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' Function to check if a plot has a discrete y-scale
#' @noRd
has_discrete_y_scale <- function(plot) {
  # Check if the plot is a ggplot object
  if (!inherits(plot, "ggplot")) {
    return(FALSE)
  }

  # Check the y aesthetic mapping
  y_mapping <- plot$mapping$y
  if (!is.null(y_mapping)) {
    # Get the data and check if y variable is discrete
    plot_data <- plot$data
    if (!is.null(plot_data) && !is.null(y_mapping)) {
      y_var <- rlang::eval_tidy(y_mapping, plot_data)
      if (is.factor(y_var) || is.character(y_var)) {
        return(TRUE)
      }
    }
  }

  # Alternative check: look for geom_density_ridges
  layers <- plot$layers
  for (layer in layers) {
    if (!is.null(layer$geom)) {
      geom_class <- class(layer$geom)[1]
      if (grepl("(ridgeline|density_ridges)", geom_class, ignore.case = TRUE)) {
        return(TRUE)
      }
    }
  }

  # Check if scale_y_discrete has been explicitly added
  if (!is.null(plot$scales)) {
    y_scale <- plot$scales$get_scales("y")
    if (!is.null(y_scale) && inherits(y_scale, "ScaleDiscrete")) {
      return(TRUE)
    }
  }

  return(FALSE)

#' Helper function that spell checks any string vector that is supplied
#' @noRd
check_spelling <- function(vector) {
  if (!is.null(vector) && !is.character(vector)) {
    stop("The vector supplied to check_spelling must be a character vector.")
  }

  # Check spelling of each element in the vector
  retval <- hunspell::hunspell(vector, dict = hunspell::dictionary("en_AU"))
  retval <- unlist(retval)

  # Boolean to test whether there were any errors picked up
  length_chk <- length(retval)

  if (length_chk > 0) return(retval) else return(invisible(NULL))

}
