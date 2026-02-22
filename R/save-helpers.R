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

    closed <- FALSE
    on.exit({
      if (!closed) try(grDevices::dev.off(), silent = TRUE)
    }, add = TRUE)

    graph_i <- maybe_add_default_scales(graph)
    class(graph_i) <- setdiff(class(graph_i), "e61_plot")

    print(graph_i)

    grDevices::dev.off()
    closed <- TRUE

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
  y_mapping <- plot@mapping$y
  if (!is.null(y_mapping)) {
    # Get the data and check if y variable is discrete
    plot_data <- plot@data
    if (!is.null(plot_data) && !is.null(y_mapping)) {
      y_var <- rlang::eval_tidy(y_mapping, plot_data)
      if (is.factor(y_var) || is.character(y_var)) {
        return(TRUE)
      }
    }
  }

  # Alternative check: look for geom_density_ridges
  layers <- plot@layers
  for (layer in layers) {
    if (!is.null(layer$geom)) {
      geom_class <- class(layer$geom)[1]
      if (grepl("(ridgeline|density_ridges)", geom_class, ignore.case = TRUE)) {
        return(TRUE)
      }
    }
  }

  # Check if scale_y_discrete has been explicitly added
  if (!is.null(plot@scales)) {
    y_scale <- plot@scales$get_scales("y")
    if (!is.null(y_scale) && inherits(y_scale, "ScaleDiscrete")) {
      return(TRUE)
    }
  }

  return(FALSE)
}

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

#' Helper function that runs the spell checker through each plot
#'
#' Returns the mispelled words
#' @noRd
check_plot_spelling <- function(plot) {

  # Spell checks
  fields <- c("title", "subtitle", "caption")

  spell_chk_i <- lapply(fields, function(field) {
    val <- plot@labels[[field]]
    if (!is.null(val)) {
      # replace html line breaks with a space and remove other elements before
      # spell checking
      val <- gsub("<br>", " ", val)
      val <- gsub("<[^>]+>", "", val)

      res <- check_spelling(val)
      if (length(res) > 0) return(res)
    }
    return(NULL)
  })

  # Assign names and remove NULLs (i.e. no typos)
  names(spell_chk_i) <- fields
  spell_chk_i <- Filter(Negate(is.null), spell_chk_i)

  # Format nicely
  spell_chk_i <- lapply(names(spell_chk_i), function(x) {

    paste0("There may be a typo in the ", x, ": ",
           paste(spell_chk_i[[x]], collapse = ", "))
  })

  spell_chk <- unlist(spell_chk_i)

  return(spell_chk)

  }

#' Converts SVG to a bitmap file
#'
#' Converts an SVG file to a bitmap file, currently supports JPEG and PNG.
#'
#' @param file_in File path to the SVG image to convert.
#' @param file_out File path to the PNG or JPEG. image to save. Default saves a
#'   file with the same name and location (except for the file extension).
#' @param delete Logical. Delete the original SVG file? (defaults to FALSE).
#' @param res Numeric. Increase the dimensions of the saved PNG or JPEG. E.g.
#'   `res = 2` doubles the dimensions of the saved graph.
#' @return Invisibly returns the file path to the PNG image
#' @keywords internal
#' @export
svg_to_bitmap <- function(file_in, file_out = NULL, res = 1, delete = FALSE) {

  res <- res * 4 # res = 1 produces exceedingly small images now apparently

  if (!grepl(".*\\.svg$", file_in))
    stop("file_in must be an svg file.")

  # If file_out is null, then save to a PNG by default
  if (is.null(file_out)) {
    file_out <- gsub("(.*)\\.svg$", "\\1.png", file_in)
  } else if (!grepl(".*\\.png$", file_out) & !grepl(".*\\.jpg$", file_out)) {
    stop("file_out must be a png or jpg file.")
  }

  if(grepl(".*\\.png$", file_out)) fmt <- "png" else fmt <- "jpg"

  if (res != 1) {
    # This approach to rescaling starts by saving a rescaled SVG before
    # converting it to PNG. Hence the need for temp files.
    file_temp_svg <- "intermed.svg"
    file_temp_out <- paste0("intermed.", fmt)

    # For some reason this changed at some point and the scaling is fine now.
    # Keeping this here in case it reverts back in the future.
    # res <- res / 1.25 # For some reason any res > 1 scales 1:1.25...

    rsvg::rsvg_png(svg = file_in, file = file_temp_out)

    g_info <- magick::image_info(magick::image_read(file_temp_out))

    rsvg::rsvg_svg(svg = file_in,
                   file = file_temp_svg,
                   width = g_info$width * res,
                   height = g_info$height * res
    )

    if(fmt == "png"){
      rsvg::rsvg_png(svg = file_temp_svg, file = file_out)

    } else if(fmt == "jpg"){
      image_temp <- magick::image_read_svg(file_temp_svg)

      magick::image_write(image = image_temp, path = file_out, format = "jpg")
    }

    unlink(file_temp_svg)
    unlink(file_temp_out)

  } else {

    if(fmt == "png"){
      rsvg::rsvg_png(svg = file_in, file = file_out)

    } else if(fmt == "jpg"){
      image_temp <- magick::image_read_svg(file_in)

      magick::image_write(image = image_temp, path = file_out, format = "jpg")
    }
  }

  if (delete) unlink(file_in)

  invisible(file_out)
}
