#' Save graphs with theme61 styles and defaults
#'
#' Saves ggplot2 graphs made using theme61. Using `save_e61()` is required
#' to ensure graphs are consistent with the e61 style and formatting.
#'
#' Use PDF in all notes and SVG in PowerPoint presentations. PDFs and SVGs are
#' better as they are modern vector graphics file formats which can be scaled up
#' and down in size without blurring or becoming pixelated. PNG should only be
#' used when required for compatibility reasons.
#'
#' @details `build_up = TRUE` saves a sequence of files (`filename_1`,
#'   `filename_2`, ..., `filename_N`) that each reveal one more
#'   category/series than the last, for stepping a chart across several
#'   PowerPoint slides. Categories not yet revealed are blanked (zeroed, or
#'   set to missing) rather than removed from the data, so the axes, scales
#'   and dimensions are identical across every step. Supported chart types:
#'   * bar/column charts (`geom_col()`/`geom_bar()`): reveals the x-axis
#'   categories left to right;
#'   * stacked area/ribbon charts (`geom_area()`/`geom_ribbon()`): reveals the
#'   stacked groups bottom to top;
#'   * grouped line/point charts (`geom_line()`/`geom_path()`/`geom_point()`/
#'   `geom_pointbar()`): reveals one colour/fill group (e.g. one line) at a
#'   time;
#'   * a single, ungrouped line or area series: reveals progressively along
#'   the x-axis instead (see `build_up_n`).
#'
#'   [plot_label()] labels are also synced to the reveal sequence, if the
#'   label's `colour` matches the rendered colour of a category/group (the
#'   usual way to label a line/bar instead of using a legend) - a label whose
#'   colour doesn't match any category (e.g. a source note) is left alone.
#'
#'   `build_up` is not supported for multi-panel graphs, faceted graphs,
#'   `preview = TRUE` or `return_plot_obj = TRUE`.
#'
#' @param filename File name to create on disk. Providing the file format
#'   extension (e.g. .svg) is suggested when saving to a single file format. The
#'   file extension must be lowercase. If you want to save to multiple formats,
#'   do not include the extension, see the `format` argument for details.
#' @param plot (single-panel specific) Name of the plot object to save. Defaults
#'   to the last plot displayed so usually you do not need to provide this
#'   argument explicitly.
#' @param plotlist (multi-panel specific) List of plots to combine as an
#'   multi-panel and save. You can also enter the charts individually as
#'   arguments to the function.
#' @param labs (multi-panel specific) A named list specifying the shared
#'   `title`, `subtitle`, `footnotes` and `sources` to place around the
#'   multi-panel figure. Defaults to `NULL` for each.
#' @param layout (multi-panel specific) A named list specifying the panel grid:
#'   `ncol`, `nrow`, `align` and `axis`. See `patchwork::plot_layout()` for what
#'   `align` and `axis` do. Defaults to `list(ncol = 2, nrow = NULL, align =
#'   "v", axis = "none")`.
#' @param spacing (multi-panel specific) A named list controlling whitespace and
#'   relative sizing:
#'   * `pad_width`, `pad_height`: Numeric (mm). Adds horizontal/vertical
#'   whitespace to the sides of all graphs. If saving multiple charts this will
#'   add the same spacing to all charts. Defaults to no additional padding.
#'   * `outer_width`: Numeric (mm). Overrides the margin between the
#'   left/right edges of the figure and the outermost panels. Defaults to NULL,
#'   which uses the built-in margin (0mm). Set higher to add whitespace around
#'   the outer edge of the figure; unlike `pad_width`, this does not affect the
#'   gap between panels.
#'   * `outer_height`: Numeric (mm). Overrides the margin between the
#'   top/bottom edges of the figure (i.e. above the title and below the
#'   footnotes/sources) and the panels. Defaults to NULL, which uses the
#'   built-in margin (0mm). Set higher to add whitespace around the outer edge
#'   of the figure; unlike `pad_height`, this does not affect the gap between
#'   panel rows.
#'   * `height_adj`: Rescales the height of the multi-panel. The function
#'   sets sensible defaults but this provides you with manual control if you
#'   need it.
#'   * `rel_heights`: A numeric vector giving the relative proportions of
#'   each graph component (title, plots, footer).
#'   * `title`, `subtitle`: Rescales the size of the space given to the
#'   multi-panel title/subtitle. Use if you think the title looks too cramped on
#'   the chart. Both default to 1.
#' @param chart_type String, or vector of strings if saving multiple plots. Type
#'   of chart. This is used to set sensible chart widths based on the type of
#'   plot you are saving. Options are:
#'   * "normal": default, for normal charts;
#'   * "wide": for time series graphs;
#'   * "square": for scatter plots;
#'   * "custom": for saving a custom aspect ratio specified in the
#'   `aspect_ratio` argument in [theme_e61()].
#' @param auto_scale Logical. Scale the y-axis automatically. Default is TRUE.
#' @param dim An optional named list specifying the plot height and width.
#'   Defaults to NULL which means the graph dimensions will be calculated
#'   automatically.
#' @param max_height Numeric. The maximum height of your plot in cm. This is
#'   used to constrain the plot resizing algorithm in cases where you want to
#'   limit the height of your charts. Defaults to NULL which does not restrict
#'   the height.
#' @param format A string vector of file formats to save as. Accepts "svg",
#'   "pdf", "eps", "png", "jpg". For example `c("svg", "pdf")` will save 2 files
#'   with the same name to the same location to SVG and PDF formats. If the file
#'   format is specified in `filename` or by the `set_format` option, then this
#'   argument is ignored.
#' @param save_data Logical. Set to TRUE if you want to save a .csv with the
#'   same name as the graph that contains the data needed to recreate the graph
#'   (defaults to FALSE).
#' @param print_info Logical. Set to TRUE if you want graph dimensions and other
#'   information printed to the console. Defaults to FALSE.
#' @param print_label_positions (single-panel specific) Logical. Set to TRUE to
#'   print the final `label`/`x`/`y` of any auto-positioned `plot_label()` text
#'   to the console as copy-pasteable arguments, so you can pin the chosen
#'   positions (or hand-tweak just one or two) instead of leaving them to
#'   auto-position again next time. Defaults to FALSE.
#' @param fast_labels (single-panel specific) Logical. Set to TRUE to skip the
#'   auto-positioning search for any `plot_label()` without an explicit `x`/`y`
#'   and use a cheap, render-free approximate position instead (near the
#'   label's own series, not collision-checked against other content). Much
#'   faster, at the cost of placement quality -- intended for quick previews
#'   while iterating, not the version you'd actually publish. Explicit `x`/`y`
#'   positions are unaffected either way. Defaults to FALSE.
#' @param spell_check Logical. Check spelling of words in the title and caption.
#'   Defaults to TRUE. Set to FALSE to turn off, or set the
#'   `theme61.enable_spellcheck` option to FALSE to skip it session-wide (see
#'   [set_t61_options]). Words listed in `inst/extdata/custom_dictionary.txt`
#'   are skipped - add words to that file if they should not be flagged.
#' @param preview Logical. Set to TRUE to show a preview of the graph in the
#'   Viewer pane but not save to disk. Defaults to FALSE.
#' @param base_size Numeric. Chart font size. Default is 10.
#' @param res Numeric. For saving to PNG only. Rescale the size of the saved
#'   PNG. E.g. `res = 2` doubles the size of the saved graph.
#' @param bg_colour Set the graph background colour. Accepts a colour name, hex
#'   code or theme61 colour object name. Defaults to "white". For graphs used in
#'   research note boxes, set the colour to `e61_boxback`.
#' @param build_up (single-panel specific) Logical. Save a sequence of files
#'   that each reveal one more category/series than the last, with a `_1`,
#'   `_2`, ..., `_N` suffix added to `filename`. See Details. Defaults to
#'   FALSE.
#' @param build_up_n (single-panel specific) Numeric. Only used by `build_up`
#'   for a single, ungrouped line or area series, where there's no existing
#'   category to step through and the x-axis instead needs to be divided into
#'   steps. Defaults to the number of unique x-values, capped at 10.
#' @param return_plot_obj (multi-panel specific) Logical. If TRUE, skips saving
#'   entirely and returns the composed multi-panel plot object instead (e.g. to
#'   print it in the Plots pane, or use it in a Shiny app). Only supported for
#'   multi-panel graphs - for a single plot, just print the ggplot object
#'   directly. Defaults to FALSE. Note that the returned object's layout (text
#'   sizes, panel spacing) is computed for a fixed target size (`dim`, or the
#'   same defaults `save_e61` would otherwise use) - it won't reflow if you
#'   resize the device afterwards.
#' @param ... (multi-panel specific) Plot objects to put on the panel.
#' @param title,subtitle,footnotes,sources `r lifecycle::badge("deprecated")`
#'   Use `labs` instead.
#' @param ncol,nrow,align,axis `r lifecycle::badge("deprecated")` Use `layout`
#'   instead.
#' @param
#' pad_width,pad_height,outer_width,outer_height,height_adj,rel_heights,spacing_adj
#' `r lifecycle::badge("deprecated")` Use `spacing` instead.
#' @return Invisibly returns the file name.
#' @export

save_e61 <- function(filename = NULL,
                     ...,
                     plot = last_plot(),
                     plotlist = NULL,
                     labs = list(title = NULL, subtitle = NULL, footnotes = NULL, sources = NULL),
                     layout = list(ncol = 2, nrow = NULL, align = "v", axis = "none"),
                     spacing = list(pad_width = 0, pad_height = 0, outer_width = NULL,
                                    outer_height = NULL, height_adj = NULL, rel_heights = NULL,
                                    title = 1, subtitle = 1),
                     dim = list(height = NULL, width = NULL),
                     format = c("svg", "pdf", "eps", "png", "jpg"),
                     chart_type = NULL,
                     auto_scale = TRUE,
                     max_height = NULL,
                     save_data = FALSE,
                     print_info = FALSE,
                     print_label_positions = FALSE,
                     fast_labels = FALSE,
                     spell_check = TRUE,
                     preview = FALSE,
                     base_size = 10,
                     res = 1,
                     bg_colour = "white",
                     build_up = FALSE,
                     build_up_n = NULL,
                     return_plot_obj = FALSE,
                     # Deprecated - use `labs` instead
                     title = lifecycle::deprecated(),
                     subtitle = lifecycle::deprecated(),
                     footnotes = lifecycle::deprecated(),
                     sources = lifecycle::deprecated(),
                     # Deprecated - use `layout` instead
                     ncol = lifecycle::deprecated(),
                     nrow = lifecycle::deprecated(),
                     align = lifecycle::deprecated(),
                     axis = lifecycle::deprecated(),
                     # Deprecated - use `spacing` instead
                     pad_width = lifecycle::deprecated(),
                     pad_height = lifecycle::deprecated(),
                     outer_width = lifecycle::deprecated(),
                     outer_height = lifecycle::deprecated(),
                     height_adj = lifecycle::deprecated(),
                     rel_heights = lifecycle::deprecated(),
                     spacing_adj = lifecycle::deprecated()
                     ) {

  # `filename` is the first formal, ahead of `...` -- so a multi-panel call
  # that passes its plots positionally without naming filename (the natural
  # shape for preview = TRUE, which has no path to give -- e.g.
  # save_e61(p1, p2, preview = TRUE)) silently matches the first plot to
  # `filename` instead of `...`, dropping it from the graph. Reclaim it as
  # the first plot instead: `filename` staying NULL then falls through to
  # the ordinary "no path supplied" error below when one is actually needed
  # (i.e. preview = FALSE).
  if (!is.null(filename) && ggplot2::is_ggplot(filename)) {
    plots <- c(list(filename), list(...), plotlist)
    filename <- NULL
  } else {
    # Compile plots
    plots <- c(list(...), plotlist)
  }

  # Fold deprecated top-level arguments into their replacement list args ----
  # (mirrors the pattern used for plot_label()'s deprecated facet_name/facet_value)

  .save_e61_deprecate_into <- function(list_arg, list_name, element, value, arg_name) {
    if (lifecycle::is_present(value)) {
      lifecycle::deprecate_warn(
        when = "0.8.0",
        what = paste0("save_e61(", arg_name, " = )"),
        with = paste0("save_e61(", list_name, " = )"),
        details = paste0(
          "Set `", element, "` inside the `", list_name, "` list instead: ",
          list_name, " = list(", element, " = ...)"
        )
      )
      list_arg[[element]] <- value
    }
    list_arg
  }

  labs   <- .save_e61_deprecate_into(labs, "labs", "title", title, "title")
  labs   <- .save_e61_deprecate_into(labs, "labs", "subtitle", subtitle, "subtitle")
  labs   <- .save_e61_deprecate_into(labs, "labs", "footnotes", footnotes, "footnotes")
  labs   <- .save_e61_deprecate_into(labs, "labs", "sources", sources, "sources")

  layout <- .save_e61_deprecate_into(layout, "layout", "ncol", ncol, "ncol")
  layout <- .save_e61_deprecate_into(layout, "layout", "nrow", nrow, "nrow")
  layout <- .save_e61_deprecate_into(layout, "layout", "align", align, "align")
  layout <- .save_e61_deprecate_into(layout, "layout", "axis", axis, "axis")

  spacing <- .save_e61_deprecate_into(spacing, "spacing", "pad_width", pad_width, "pad_width")
  spacing <- .save_e61_deprecate_into(spacing, "spacing", "pad_height", pad_height, "pad_height")
  spacing <- .save_e61_deprecate_into(spacing, "spacing", "outer_width", outer_width, "outer_width")
  spacing <- .save_e61_deprecate_into(spacing, "spacing", "outer_height", outer_height, "outer_height")
  spacing <- .save_e61_deprecate_into(spacing, "spacing", "height_adj", height_adj, "height_adj")
  spacing <- .save_e61_deprecate_into(spacing, "spacing", "rel_heights", rel_heights, "rel_heights")

  if (lifecycle::is_present(spacing_adj)) {
    lifecycle::deprecate_warn(
      when = "0.8.0",
      what = "save_e61(spacing_adj = )",
      with = "save_e61(spacing = )",
      details = "Set `title`/`subtitle` inside the `spacing` list instead: spacing = list(title = ..., subtitle = ...)"
    )
    if (!is.null(spacing_adj$title)) spacing$title <- spacing_adj$title
    if (!is.null(spacing_adj$subtitle)) spacing$subtitle <- spacing_adj$subtitle
  }

  # Fill in defaults for any list elements the caller didn't supply
  spacing$pad_width  <- spacing$pad_width  %||% 0
  spacing$pad_height <- spacing$pad_height %||% 0
  spacing$title      <- spacing$title      %||% 1
  spacing$subtitle   <- spacing$subtitle   %||% 1
  layout$ncol  <- layout$ncol  %||% 2
  layout$align <- layout$align %||% "v"
  layout$axis  <- layout$axis  %||% "none"

  # Coerce plot classes and prep --------------------------------------------

  # For single-panel graphs
  if (length(plots) == 0) plots <- list(plot)

  # Ensure plots are e61 plots
  plots <- as_e61_plot(plots)

  # Classify each plot as map/non-map (and correct map-only axis chrome).
  # This has to happen for every panel (not just single-panel saves) since
  # save_multi() reads the theme (e.g. legend position/title) straight off
  # each plot.
  plots <- lapply(plots, finalise_e61_plot)

  if (length(plots) == 1) {
    is_map <- inherits(plots[[1]], "e61_map")

    if (is_map) {
      auto_scale <- FALSE
      chart_type <- "custom"
    } else if (is.null(chart_type)) {
      chart_type <- "normal"
    }

    plots[[1]] <- plots[[1]] + ggplot2::theme(rect = ggplot2::element_rect(fill = bg_colour))
  }

  # Check whether the plots are ggplot2 objects
  plots <- check_plots(plots)

  # Guard clauses -----------------------------------------------------------
  if (return_plot_obj && length(plots) <= 1) {
    cli::cli_abort("return_plot_obj is only supported for multi-panel graphs (2 or more plots). For a single plot, just print the ggplot object directly.")
  }

  if (build_up) {
    if (length(plots) > 1)
      cli::cli_abort("build_up is only supported for single-panel graphs.")

    if (return_plot_obj)
      cli::cli_abort("build_up cannot be combined with return_plot_obj.")

    if (preview)
      cli::cli_abort("build_up cannot be combined with preview = TRUE.")

    if (length(plots[[1]]@facet$params) != 0)
      cli::cli_abort("build_up is not supported for faceted graphs.")
  }

  # Enforce chart type
  if(is.null(chart_type)){
    chart_type <- "normal"

  } else if(length(chart_type) == 1){

    if(!chart_type %in% c("normal", "wide", "square", "custom"))
      cli::cli_abort("Invalid chart type. Chart types must be 'normal', 'wide', 'square', or 'custom'.")

  } else if(length(chart_type) > 1){

    if(!all(chart_type %in% c("normal", "wide", "square")))
      cli::cli_abort("Invalid chart type. All chart types must be one of 'normal', 'wide' or 'square'.")
  }

  # Check if filename has been provided when preview/return_plot_obj mode is FALSE
  if (!preview && !return_plot_obj && is.null(filename)) cli::cli_abort("You must provide a file path to save the graph.")

  # Override save directory with temp file if preview mode is TRUE
  if (preview && !return_plot_obj) {
    cli::cli_alert_info("Preview mode is activated, file will not be saved to disk.")
    filename <- tempfile(fileext = ".svg")
  }

  # Check if the save directory exists (not applicable if we're just
  # returning the plot object - nothing gets written to disk)
  if (!return_plot_obj) {
    dir_provided <- grepl("^(.*)\\/.*\\..{3}$", filename)
    dir_name <- gsub("^(.*)\\/.*\\..{3}$", "\\1", filename)

    if (dir_provided && !dir.exists(dir_name))
      cli::cli_abort("The directory you are trying to save to does not exist.")
  }

  # Skip file format resolution entirely if we're just returning the plot
  # object - nothing gets written to disk, so filename/format are unused
  # (and filename may be NULL, which the checks below can't handle).
  if (!return_plot_obj) {

    # Enforce file format requirements if a file extension is provided
    if (grepl("\\..{3}$", filename) && !grepl("\\.(svg|pdf|eps|png|jpg)$", filename)) {
      cli::cli_abort("You must provide a valid file extension. The following file formats are supported: svg, pdf, eps, png, jpg.")
    }

    # Determine which file formats to save
    if (grepl("\\..{3}$", filename)) {
      format <- gsub("^.*\\.(.{3})$", "\\1", filename)

      # Strip file extension from filename
      filename <- gsub("^(.*)\\..{3}$", "\\1", filename)
    } else if (missing(format) && !is.null(getOption("theme61.default_save_format"))) {
      # missing(), not is.null() - format always has a default value, so
      # is.null(format) is never true and this branch was unreachable.
      format <- getOption("theme61.default_save_format")
    } else {
      format <- match.arg(format, several.ok = TRUE)
    }
  }

  # Check if the data frame(s) can be written - every panel needs its own
  # extractable data frame, not just the first, since multi-panel graphs are
  # often built from a different data frame per panel.
  if (save_data && !all(vapply(plots, function(p) is.data.frame(p@data), logical(1))))
    cli::cli_abort("You have set save_data = TRUE, but the data frame could not be extracted from one or more of the ggplots. This may be caused by a plot with multiple data frames supplied (e.g. if each geom has its own data). In this case you will need to set save_data = FALSE and manually save the data used to produce the graph.")

  # Check list args are valid
  if (!all(names(dim) %in% c("height", "width")))
    cli::cli_abort("You have specified invalid list elements in 'dim'.")

  if (!all(names(labs) %in% c("title", "subtitle", "footnotes", "sources")))
    cli::cli_abort("You have specified invalid list elements in 'labs'.")

  if (!all(names(layout) %in% c("ncol", "nrow", "align", "axis")))
    cli::cli_abort("You have specified invalid list elements in 'layout'.")

  if (!all(names(spacing) %in% c("pad_width", "pad_height", "outer_width", "outer_height",
                                  "height_adj", "rel_heights", "title", "subtitle")))
    cli::cli_abort("You have specified invalid list elements in 'spacing'.")

  # Spell checker -------------------------------------------------------

  if (spell_check && isTRUE(getOption("theme61.enable_spellcheck", TRUE))) {
    # Loop through the plots. unlist() (not c()) so that plots with no typos
    # (check_plot_spelling() returns NULL) are dropped rather than counted as
    # an empty message.
    spell_chk <- unlist(lapply(plots, check_plot_spelling))

    # Compile the messages
    adv_msg <- c(spell_chk)

    # Compile advisory messages
    print_adv <- function() {
      cli::cli_div(theme = list(".adv" = list(`color` = "#cc0000")))
      sapply(adv_msg, cli::cli_alert, class = "adv")
      cli::cli_end()
    }

    # Print advisory messages
    if (length(adv_msg) > 0) print_adv()

  }

  # Make graph to save --------------------------------

  # Check whether to save an mpanel or a single planel chart - these require
  # different approaches
  if (length(plots) > 1) {
    save_input <- save_multi(
      filename = filename,
      format = format,
      plots = plots,
      chart_type = chart_type,
      title = labs$title,
      subtitle = labs$subtitle,
      footnotes = labs$footnotes,
      sources = labs$sources,
      width = dim$width, # control width of the chart
      height = dim$height, # control height of the chart
      auto_scale = auto_scale,
      title_spacing_adj = spacing$title, # adjust the amount of space given to the title
      subtitle_spacing_adj = spacing$subtitle, # adjust the amount of space given to the subtitle
      height_adj = spacing$height_adj, # adjust the vertical spacing of the mpanel charts
      base_size = base_size,
      print_label_positions = print_label_positions,
      pad_width = spacing$pad_width,
      pad_height = spacing$pad_height,
      outer_width = spacing$outer_width,
      outer_height = spacing$outer_height,
      ncol = layout$ncol,
      nrow = layout$nrow,
      align = layout$align,
      axis = layout$axis,
      rel_heights = spacing$rel_heights,
      bg_colour = bg_colour
    )

    # Short-circuit: return the composed plot object instead of saving it
    if (return_plot_obj) return(save_input$graph)

  } else {

    save_input <- save_single(
      filename = filename,
      plot = plots[[1]],
      chart_type = chart_type,
      auto_scale = auto_scale, # control whether y-axis is scaled
      width = dim$width, # control width
      height = dim$height, # control height
      max_height = max_height, # control max height
      format = format,
      base_size = base_size,
      print_label_positions = print_label_positions,
      fast_labels = fast_labels,
      pad_width = spacing$pad_width,
      pad_height = spacing$pad_height,
      bg_colour = bg_colour
    )
  }


  # Save --------------------------------------------------------------------

  if (build_up) {

    # The expensive layout/scaling work above (save_single()) was done once,
    # using the complete data, so every step below shares identical
    # dimensions and axis limits - all that's left is a cheap re-render per
    # step with some rows blanked out.
    build_up_result <- resolve_build_up(save_input$graph, build_up_n)
    step_filenames <- paste0(filename, "_", seq_along(build_up_result$steps))

    for (k in seq_along(build_up_result$steps)) {

      step_plot <- save_input$graph

      for (j in seq_along(build_up_result$targets)) {
        step_plot@layers[[build_up_result$targets[j]]]$data <- build_up_result$steps[[k]][[j]]
      }

      save_graph(
        graph = step_plot,
        format = format,
        filename = step_filenames[k],
        width = save_input$width,
        height = save_input$height,
        bg_colour = bg_colour,
        res = res
      )
    }

    cli::cli_alert_info("build_up saved {length(step_filenames)} chart(s): {paste(basename(step_filenames), collapse = ', ')}")

  } else {

    save_graph(
      graph = save_input$graph,
      format = format,
      filename = filename,
      width = save_input$width,
      height = save_input$height,
      bg_colour = bg_colour,
      res = res
    )
  }

  # Post-saving -------------------------------------------------------------

  # Print information on saving parameters
  if (print_info) {
    cli::cli_alert_info("Graph width = {round(save_input$width, 4)} and height = {round(save_input$height, 4)}.")
  }

  # Save the data used to make the graph
  if (save_data) {

    if (build_up) {

      for (k in seq_along(build_up_result$steps)) {
        # A build_up chart only ever has one target data frame worth writing
        # out per step (the reference layer's) if there are several, they're
        # all derived from the same reveal sequence, so the first is enough.
        data.table::fwrite(build_up_result$steps[[k]][[1]], paste0(step_filenames[k], ".csv"))
      }

    } else {

      for (i in seq_along(plots)) {
        # Give each plot's data file the same name as the graph. When there are
        # multiple plots (multi-panel), append the panel number to keep the
        # file names unique, since each panel may be built from a different
        # data frame.
        data_name <- if (length(plots) > 1) {
          paste0(filename, " ", i, ".csv")
        } else {
          paste0(filename, ".csv")
        }

        data.table::fwrite(plots[[i]]@data, data_name)
      }
    }
  }

  # Opens the graph file in the Viewer, and also in the browser if requested
  # Not attempted for build_up - there's a sequence of files, not one - the
  # info message above already tells the user what was saved.
  if (!build_up) {

    # Put filename back together
    file_to_open <- paste0(filename, ".", format[[1]])

    if (isTRUE(getOption("theme61.open_in_browser", FALSE))) {
      file_to_open_browser <- shQuote(here::here(file_to_open))

      out <- try(utils::browseURL(here::here(file_to_open)))

      if (inherits(out, "try-error")) cli::cli_warn("Graph file could not be opened")

    }

    if (interactive() &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      # Only run this in interactive mode
      # rstudioapi::viewer will only open temp files in the Viewer pane for some reason
      # Always preview an SVG, even if the saved format(s) are not SVG
      preview_svg <- make_preview_svg(
        graph = save_input$graph,
        format = format,
        filename = filename,
        width = save_input$width,
        height = save_input$height,
        bg_colour = bg_colour,
        res = res
      )

      out <- try(rstudioapi::viewer(preview_svg))

      if (!is.null(out)) cli::cli_warn("Graph file could not be opened.")

    }
  }

  # Invisibly returns the filename/s
  retval <- if (build_up) {
    as.vector(outer(step_filenames, format, paste, sep = "."))
  } else {
    paste(filename, format, sep = ".")
  }

  invisible(retval)
}
