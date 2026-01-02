#' Print method for theme61 plots to automatically render plots in Viewer pane
#' @export
print.e61_plot <- function(x, ...) {

  if (isFALSE(getOption("theme61.preview_on_print", TRUE))) {
    return(NextMethod())
  }

  in_rstudio <- interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()

  # Preview first
  if (in_rstudio) {
    suppressWarnings(suppressMessages(
      save_e61(plot = x, preview = TRUE)
    ))
      }

  # Plot last so Plots pane keeps it
  x_plot <- x
  class(x_plot) <- setdiff(class(x_plot), "e61_plot")
  print(x_plot)

  # Now force focus to Viewer (after RStudio finishes plot focus changes)
  if (in_rstudio) {
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

  # Run on next top-level task, then re-try shortly after.
  id <- NULL
  id <- addTaskCallback(function(...) {
    try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE)
    removeTaskCallback(id)
    TRUE
  })

  if (requireNamespace("later", quietly = TRUE)) {
    later::later(function() try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE), 0.05)
    later::later(function() try(rstudioapi::executeCommand("activateViewer", quiet = TRUE), silent = TRUE), 0.20)
  }

  invisible(TRUE)
}
