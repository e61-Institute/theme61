#' Print method for theme61 plots to automatically render plots in Viewer pane
#' @export
print.e61_ggplot <- function(x, ...) {

  if (isFALSE(getOption("theme61.preview_on_print", TRUE))) {
    return(NextMethod())
  }

  in_rstudio <- interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()

  # Preview first
  if (in_rstudio) {
    suppressWarnings(
      suppressMessages(
        theme61::save_e61(
          filename     = NULL,
          plot         = x,
          preview      = TRUE,
          format       = "svg",
          spell_check  = FALSE,
          save_data    = FALSE,
          print_info   = FALSE
        )
      )
    )
  }

  # Plot last so Plots pane keeps it
  x_plot <- x
  class(x_plot) <- setdiff(class(x_plot), "e61_ggplot")
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
