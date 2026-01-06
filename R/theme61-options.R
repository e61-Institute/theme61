# This is where all the set/unset option functions live

#' Set option to open graphs in the browser instead of the Viewer pane
#'
#' Previous versions of theme61 opened graphs in the browser instead of the
#' Viewer pane. You can bring back this functionality by running this function,
#' which sets a session-wide option.
#'
#' @return This function is used for its side effects.
#' @rdname open_graph_browser
#' @export
set_open_graph_browser <- function() {
  options(open_e61_graph = TRUE)

  invisible(TRUE)
}

#' @rdname open_graph_browser
#' @export
unset_open_graph_browser <- function() {
  options(open_e61_graph = FALSE)

  invisible(FALSE)
}

#' Sets the default file save format if format is not specified
#'
#' This function sets the file save format if \code{format} is not specified in
#' \code{save_e61} and the file extension is not provided in \code{filename}.
#'
#' @inheritParams save_e61
#' @return This function is used for its side effects.
#' @rdname set_format
#' @export
set_format <- function(format) {
  options(default_save_format = format)

  invisible(TRUE)
}

#' Clears the default file save format from the session options
#'
#' This function clears the default file save format specified in
#' \code{set_format}.
#'
#' @return This function is used for its side effects.
#' @rdname set_format
#' @export
unset_format <- function() {
  options(default_save_format = NULL)

  invisible(FALSE)
}
