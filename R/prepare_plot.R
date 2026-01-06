#' Generic to prepare plots for saving
#' @export
prepare_plot <- function(x, ...) {
  UseMethod("prepare_plot")
}

#' @export
prepare_plot.e61_plot <- function(x, ...) {
  x
}

#' @export
prepare_plot.e61_map <- function(x, ...) {
  x
}

#' Method that fails for non-plots
#' @export
prepare_plot.default <- function(x, ...) {
  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be prepared into a e61 plot."
  )
}
