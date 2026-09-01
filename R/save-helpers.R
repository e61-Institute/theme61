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

#' With no device open, ggplotGrob()/ggplot_gtable() can silently open the
#' session's default device to measure text -- left open, that can corrupt
#' later renders. Opens a throwaway device first only if none is open.
#'
#' Also muffles the "font family 'pt-sans' not found in PostScript font
#' database" warning: grid's font-metric fallback doesn't know about
#' sysfonts-registered families on some devices, but showtext still renders
#' pt-sans correctly wherever it's actually drawn.
#' @noRd
t61_with_device <- function(expr) {
  if (grDevices::dev.cur() == 1) {
    svg_file <- tempfile(fileext = ".svg")
    svglite::svglite(svg_file)
    on.exit({
      grDevices::dev.off()
      unlink(svg_file)
    }, add = TRUE)
  }
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("not found in PostScript font database", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' @noRd
t61_ggplotGrob_quiet_na <- function(plot) t61_with_device(t61_quiet_na_removal(ggplot2::ggplotGrob(plot)))

#' @noRd
t61_ggplot_gtable_quiet_na <- function(build) t61_with_device(t61_quiet_na_removal(ggplot2::ggplot_gtable(build)))

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

    # Explicit, not dispatch: the e61_plot class is stripped below so this
    # print() never fires ggplot_build.e61_plot(), and save_single()/
    # save_multi() never bake default x/colour/fill scales into `graph`
    # itself (only into a build-only copy) - so this is the only place they
    # reach the actual rendered output.
    graph_i <- maybe_add_default_scales(graph)
    class(graph_i) <- setdiff(class(graph_i), "e61_plot")

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

  is_valid <- vapply(plots, is_ggplot, logical(1))

  if (!all(is_valid)) {
    bad_plot <- plots[[which(!is_valid)[1]]]
    stop(paste0(bad_plot, " is not a valid save_e61() argument. Check that you have not supplied the wrong object to save_e61() or used an incorrect or outdated argument (use ?save_e61 to view valid arguments)."))
  }

  plots
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
    # y_mapping may reference a layer-only column or an unevaluable
    # expression - fall back to "not discrete" instead of erroring.
    plot_data <- plot@data
    if (!is.null(plot_data) && !is.null(y_mapping)) {
      y_var <- tryCatch(rlang::eval_tidy(y_mapping, plot_data), error = function(e) NULL)
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

  # Check spelling of each element in the vector, treating words in
  # custom_dictionary.txt (e.g. "e61") as correctly spelled
  dict <- hunspell::dictionary("en_AU", add_words = t61_custom_dictionary())
  retval <- hunspell::hunspell(vector, dict = dict)
  retval <- unlist(retval)

  # Boolean to test whether there were any errors picked up
  length_chk <- length(retval)

  if (length_chk > 0) return(retval) else return(invisible(NULL))

}

#' Fetch (and cache for the session) the custom dictionary of words that
#' save_e61()'s spell-checker should never flag as typos. Cached in `t61_env`
#' so repeated calls - e.g. across several plots in a multi-panel save -
#' don't re-read the file from disk every time.
#' @noRd
t61_custom_dictionary <- function() {
  if (!is.null(t61_env$custom_dictionary)) {
    return(t61_env$custom_dictionary)
  }

  path <- system.file("extdata", "custom_dictionary.txt", package = "theme61")

  words <- if (nzchar(path)) readLines(path, warn = FALSE) else character(0)
  words <- trimws(words)
  words <- words[nzchar(words) & !startsWith(words, "#")]

  t61_env$custom_dictionary <- words

  words
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

  res <- res * 4 # res = 1 alone produces images too small to be usable

  if (!grepl(".*\\.svg$", file_in))
    stop("file_in must be an svg file.")

  # If file_out is null, then save to a PNG by default
  if (is.null(file_out)) {
    file_out <- gsub("(.*)\\.svg$", "\\1.png", file_in)
  } else if (!grepl(".*\\.png$", file_out) & !grepl(".*\\.jpg$", file_out)) {
    stop("file_out must be a png or jpg file.")
  }

  if(grepl(".*\\.png$", file_out)) fmt <- "png" else fmt <- "jpg"

  # Rescale by re-rendering the SVG at the target size, via temp files.
  file_temp_svg <- tempfile(fileext = ".svg")
  file_temp_out <- tempfile(fileext = paste0(".", fmt))
  on.exit(unlink(c(file_temp_svg, file_temp_out)), add = TRUE)

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

  if (delete) unlink(file_in)

  invisible(file_out)
}
