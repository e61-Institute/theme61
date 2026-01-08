#' Print method for theme61 plots
#'
#' - Always draws a plot in the Plots pane
#' - Also renders a preview in the Viewer (opt-out via option)
#' - Prefers Viewer focus by default (best-effort)
#'
#' @export
print.e61_ggplot <- function(x, ...) {

  # opt-out (default ON)
  if (isFALSE(getOption("theme61.preview_on_print", TRUE))) {
    return(NextMethod())
  }

  in_rstudio <- interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()

  # Viewer preview
  if (in_rstudio) {
    suppressWarnings(
      suppressMessages(
        save_e61(
          plot        = x,
          preview     = TRUE,
          format      = "svg",
          spell_check = FALSE,
          save_data   = FALSE,
          print_info  = FALSE
        )
      )
    )
  }

  # Plots pane render (must include theme61 defaults)
  x_plot <- maybe_add_default_scales(x)
  class(x_plot) <- setdiff(class(x_plot), "e61_ggplot")
  print(x_plot)

  # Prefer Viewer focus by default (best-effort)
  if (in_rstudio) {
    # one-shot “after plotting settles” activator
    id <- NULL
    id <- addTaskCallback(function(...) {
      try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE)
      removeTaskCallback(id)
      TRUE
    })
  }

  activate_viewer_after_plot()

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
