# Helper functions that are used across save_single, save_multi or save_e61

#' Muffle only the "removed N rows containing missing values" warning geom
#' layers raise when drawn, from evaluating `expr`; any other warning still
#' propagates normally. save_single()/save_multi() build a gtable (via
#' ggplotGrob()/ggplot_gtable(), directly or through update_labs()/
#' update_scales()) purely to measure layout -- axis widths, aspect ratio,
#' title/axis heights -- while the plot still has auto-positioned
#' plot_label() text sitting at NA x/y (real positions are only resolved
#' later, by t61_apply_autolabel()), so building the gtable to read off its
#' dimensions always trips this warning even though nothing is wrong.
#' Matched on message text since ggplot2 gives it no distinct condition
#' class, so a real data/scale issue still surfaces, both here and
#' (unsuppressed) on the final real render.
#' @noRd
t61_quiet_na_removal <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("missing values or values outside the scale range", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' @noRd
t61_ggplotGrob_quiet_na <- function(plot) t61_quiet_na_removal(ggplot2::ggplotGrob(plot))

#' @noRd
t61_ggplot_gtable_quiet_na <- function(build) t61_quiet_na_removal(ggplot2::ggplot_gtable(build))

#' Helper function to actually perform the saving functionality
#' @noRd
save_graph <- function(graph, format, filename, width, height, bg_colour, res) {
  lapply(format, function(fmt) {

    file_i <- paste0(filename, ".", fmt)

    # png/jpg/eps/pdf are all produced by rendering an SVG first and then converting it with rsvg
    needs_temp_svg <- fmt %in% c("png", "jpg", "eps", "pdf")
    file_name_i <- if (needs_temp_svg) tempfile(fileext = ".svg") else file_i

    # add very slight width buffer
    width <- width + 0.1

    svglite::svglite(filename = file_name_i, width = cm_to_in(width), height = cm_to_in(height), bg = bg_colour)

    closed <- FALSE
    on.exit({
      if (!closed) try(grDevices::dev.off(), silent = TRUE)
    }, add = TRUE)

    graph_i <- maybe_add_default_scales(graph)
    class(graph_i) <- setdiff(class(graph_i), "e61_ggplot")
    print(graph_i)

    grDevices::dev.off()
    closed <- TRUE

    # Convert the rendered SVG into the requested format
    if (fmt == "png") {
      svg_to_bitmap(file_name_i, paste0(filename, ".png"), delete = TRUE, res = res)

    } else if (fmt == "jpg") {
      svg_to_bitmap(file_name_i, paste0(filename, ".jpg"), delete = TRUE, res = res)

    } else if (fmt == "pdf") {
      rsvg::rsvg_pdf(svg = file_name_i, file = file_i)
      unlink(file_name_i)

    } else if (fmt == "eps") {
      rsvg::rsvg_eps(svg = file_name_i, file = file_i)
      unlink(file_name_i)
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

#' Create a temp SVG file to preview a graph in the Viewer pane, regardless
#' of which format(s) were saved to disk
#' @noRd
make_preview_svg <- function(graph, format, filename, width, height, bg_colour, res) {

  preview_svg <- tempfile(fileext = ".svg")

  if ("svg" %in% format) {
    file.copy(paste0(filename, ".svg"), preview_svg, overwrite = TRUE)
  } else {
    save_graph(
      graph = graph,
      format = "svg",
      filename = tools::file_path_sans_ext(preview_svg),
      width = width,
      height = height,
      bg_colour = bg_colour,
      res = res
    )
  }

  invisible(preview_svg)
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

#' Work out the aspect ratio for a chart_type, but respect an aspect ratio
#' the user has already customised away from theme_e61()'s default of 0.75.
#' @noRd
resolve_aspect_ratio <- function(plot, chart_type) {
  current <- plot@theme$aspect.ratio

  customised <- !is.null(current) && !isTRUE(all.equal(current, 0.75))

  if (customised) return(plot)

  target <- switch(chart_type,
                   normal = 0.75,
                   square = 1,
                   wide = 0.5,
                   NULL)

  if (is.null(target)) return(plot)

  plot + theme(aspect.ratio = target)
}

#' Work out the text size to apply, but respect a size the user has already
#' customised away from theme_e61()'s default (the theme61.base_size option).
#' Returns the plot plus the effective size to use for any size-dependent
#' formatting done afterwards (e.g. update_margins()), so spacing stays
#' proportional to whichever size actually ends up on the plot.
#' @noRd
resolve_text_size <- function(plot, base_size) {
  current <- plot@theme$text$size
  default_size <- getOption("theme61.base_size", default = 10)

  customised <- !is.null(current) && !isTRUE(all.equal(current, default_size))

  if (customised) return(list(plot = plot, base_size = current))

  list(plot = plot + theme(text = element_text(size = base_size)), base_size = base_size)
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
