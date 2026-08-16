#' Print method for theme61 plots
#'
#' - Always draws a plot in the Plots pane
#' - Also renders a preview in the Viewer (opt-out via option)
#' - Prefers Viewer focus by default (best-effort)
#' - All of the above is skipped in `theme61.iterate_mode`
#'
#' @keywords internal
#' @export
print.e61_plot <- function(x, ...) {

  # theme61.iterate_mode: skip the Viewer preview and all automatic
  # theme61 styling (theme, scales, facet spacing, etc.) for fast
  # iteration. Dropping the class means the plot builds and prints with
  # plain ggplot2 defaults; any theme61 functions the user called
  # explicitly (e.g. scale_colour_e61()) are already part of the plot
  # object and still apply.
  if (isTRUE(getOption("theme61.iterate_mode", FALSE))) {
    class(x) <- setdiff(class(x), c("e61_map", "e61_plot"))
    return(print(x))
  }

  # opt-out (default ON)
  if (isFALSE(getOption("theme61.preview_on_print", TRUE))) {
    return(NextMethod())
  }

  in_rstudio <- interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()

  # Alter defaults for facets with free y scales
  if (!is.null(x@facet$params$free$y)) {
    free_y <- x@facet$params$free$y
  } else {
    free_y <- FALSE
  }

  # Detect whether user supplied a y scale (pre-build)
  ys <- x@scales$get_scales("y")
  user_has_y <- !is.null(ys)

  # Detect whether user supplied custom limits
  user_limits <- FALSE
  if (user_has_y) {
    lim <- ys$limits
    user_limits <- !is.null(lim) && !inherits(lim, "waiver")
  }

  # Decide whether to autoscale in preview
  auto_scale_preview <- TRUE
  if (free_y) {
    if (!user_has_y) {
      auto_scale_preview <- FALSE
    } else if (!inherits(ys, "scale_e61")) {
      auto_scale_preview <- FALSE
    } else if (user_limits) {
      auto_scale_preview <- FALSE
    }
  }

  # Viewer preview (render in background). Auto-positioned plot_label()
  # text without an explicit x/y uses the cheap, render-free fast
  # placement here (see t61_place_label_fast()) rather than the full
  # search -- this runs on every print(), so it needs to stay fast for
  # quick iteration. save_e61() itself (without preview = TRUE) always
  # resolves the real, optimised position regardless.
  if (in_rstudio) {
    suppressWarnings(
      suppressMessages(save_e61(plot = x, preview = TRUE, format = "svg", auto_scale = auto_scale_preview, fast_labels = TRUE))
      )
  }

  # Plots pane render (must include theme61 defaults)
  x_plot <- finalise_e61_plot(x)
  x_plot <- maybe_add_default_scales(x_plot)
  class(x_plot) <- setdiff(class(x_plot), c("e61_map", "e61_plot"))
  print(x_plot)

  # Prefer Viewer focus by default (best-effort). Users who don't want any
  # of this can use theme61.iterate_mode instead.
  if (in_rstudio) {
    activate_viewer_after_plot()
  }

  # Print copy-pasteable auto-positioned label code (see autolabel-apply.R),
  # if any plot_label() layer asked for it via print_position = TRUE. Uses
  # save_single() directly, which resolves the chart's final size and
  # applies auto-positioning but never writes a file, so this works without
  # the user having to call save_e61() first.
  if (t61_has_print_position_labels(x)) {
    try(
      suppressWarnings(
        save_single(filename = NULL, plot = x, chart_type = NULL, auto_scale = auto_scale_preview,
                    width = NULL, height = NULL, max_height = NULL, format = "svg",
                    base_size = 10, pad_width = 0, pad_height = 0, bg_colour = "white",
                    print_label_positions = TRUE)
      ),
      silent = TRUE
    )
  }

  invisible(x)
}

#' @noRd
activate_viewer_after_plot <- function() {
  if (!interactive() ||
      !requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    return(invisible(FALSE))
  }

  # Immediate attempt
  try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE)

  # After top-level task returns
  id <- NULL
  id <- addTaskCallback(function(...) {
    try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE)
    removeTaskCallback(id)
    TRUE
  })

  # Retry a few times to win focus races
  if (requireNamespace("later", quietly = TRUE)) {
    later::later(function() try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE), 0.05)
    later::later(function() try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE), 0.20)
    later::later(function() try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE), 0.50)
  }

  invisible(TRUE)
}
