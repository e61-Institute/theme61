# as_e61_plot ----

#' Generic to coerce plots to e61_plot class
#' @export
as_e61_plot <- function(x, ...) {
  UseMethod("as_e61_plot")
}

#' Method for plain ggplot
#' @export
as_e61_plot.ggplot <- function(x, ...) {
  if (!inherits(x, "e61_plot")) {
    class(x) <- c("e61_plot", class(x))
  }
  x
}

#' Method for existing e61_plot class objects
#' @export
as_e61_plot.e61_plot <- function(x, ...) {
  x
}

#' Method that works for lists of plots
#' @export
as_e61_plot.list <- function(x, ...) {
  lapply(x, as_e61_plot)
}

#' Method that fails for non-plots
#' @export
as_e61_plot.default <- function(x, ...) {
  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be converted to an e61 plot"
  )
}

#' Generic to coerce plots to e61_map class
#' @export
as_e61_map <- function(x, ...) {
  UseMethod("as_e61_map")
}

# as_e61_map ----

#' Method for plain ggplot
#' @export
as_e61_map.ggplot <- function(x, ...) {
  if (!inherits(x, "e61_map")) {
    class(x) <- c("e61_map", class(x))
  }
  x
}

#' Method for existing e61_map class objects
#' @export
as_e61_map.e61_map <- function(x, ...) {
  x
}

#' Method that works for lists of plots
#' @export
as_e61_map.list <- function(x, ...) {
  lapply(x, as_e61_map)
}

#' Method that fails for non-plots
#' @export
as_e61_map.default <- function(x, ...) {
  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be converted to an e61 map"
  )
}
