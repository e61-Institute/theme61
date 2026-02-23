#' Print method for theme61 plots
#'
#' - Always draws a plot in the Plots pane
#' - Also renders a preview in the Viewer (opt-out via option)
#' - Prefers Plots focus by default
#'
#' @keywords internal
#' @export
print.e61_ggplot <- function(x, ...) {

  # opt-out (default ON)
  if (isFALSE(getOption("theme61.preview_on_print", TRUE))) {
    return(NextMethod())
  }

  in_rstudio <- interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()

  # Alter defaults for facets with free y scales

  free_y <- x@facet$params$free$y

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

  # opt-in (default OFF): focus Viewer after printing
  focus_viewer <- isTRUE(getOption("theme61.focus_viewer_on_print", FALSE))

  # Viewer preview (render in background)
  if (in_rstudio) {
    suppressWarnings(
      suppressMessages(save_e61(plot = x, preview = TRUE, format = "svg"))
      )
  }

  # Plots pane render (must include theme61 defaults)
  x_plot <- maybe_add_default_scales(x)
  class(x_plot) <- setdiff(class(x_plot), "e61_ggplot")
  print(x_plot)

  # Optional Viewer focus (off by default)
  if (in_rstudio && focus_viewer) {
    activate_viewer_after_plot()
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
