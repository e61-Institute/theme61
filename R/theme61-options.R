#' Set various options in the theme61 package
#'
#' To see the list of available options, just run the function in the console
#' with no arguments i.e. \code{set_t61_options()}.
#'
#' @param opt A named list of options to set. See Details for available options.
#'
#' @details The following options are available to set:
#'  \itemize{
#'    \item \code{theme61.open_in_browser}: If TRUE, graphs will also open in the browser in addition to the Viewer pane. This is FALSE by default.
#'    \item \code{theme61.default_save_format}: The default file save format if format is not specified in [save_e61] and the file extension is not provided in \code{filename}. This is "svg" by default.
#'    \item \code{theme61.preview_on_print}: If TRUE (default), graphs will be automatically previewed in the Viewer pane when printed to the console.
#'    \item \code{theme61.base_size}: The base font size for graphs. This is 10 by default.
#'    \item \code{theme61.auto_theme}: If TRUE (default), \code{theme_e61()} is automatically applied whenever you call \code{ggplot()}. Set to FALSE to turn this off and apply your own theme instead.
#'    \item \code{theme61.iterate_mode}: If TRUE, all of theme61's automatic styling and Viewer pane preview rendering is skipped, so graphs print to the Plots pane with plain ggplot2 defaults as fast as possible. This is FALSE by default. Any theme61 functions you call explicitly (e.g. \code{scale_colour_e61()}, \code{theme_e61()}) still apply as normal, since they become part of the plot object regardless of this option.
#'  }
#' @return This function is used for its side effects.
#'
#' @examples
#' \dontrun{
#' # Set the default save format to "png"
#' set_t61_options(list(theme61.default_save_format = "png"))
#' }
#'
#' @export
set_t61_options <- function(opt = NULL) {
  if (is.null(opt)) {
    t61_opts <- names(options())[data.table::like(names(options()), "^theme61\\..*")] |> as.list()

    t61_set_opts <- sapply(t61_opts, options)

    # Print options as a pretty list displayed as 'option = value'
    cli::cli_bullets(c("i" = "Current theme61 options:"))
    for (opt in names(t61_set_opts)) {
      cli::cli_bullets(c(" " = paste0(opt, " = ", t61_set_opts[[opt]])))
    }

    return(invisible(t61_set_opts))
  } else {
    if (!is.list(opt) || is.null(names(opt))) {
      stop("opt must be a named list of options to set.")
    }

    # Make sure options supplied are valid theme61 options
    valid_opts <- names(options())[data.table::like(names(options()), "^theme61\\..*")]
    invalid_opts <- setdiff(names(opt), valid_opts)
    if (length(invalid_opts) > 0) {
      stop(paste0("Invalid options supplied: ", paste(invalid_opts, collapse = ",
")), ". Valid options are: ", paste(valid_opts, collapse = ", "))
    }

    options(opt)
  }

  invisible(NULL)
}

#' Set option to also open graphs in the browser
#'
#' Previous versions of theme61 opened graphs in the browser instead of the
#' Viewer pane. You can bring back this functionality by running this function,
#' which sets a session-wide option. Graphs will still appear in the Viewer
#' pane as normal.
#'
#' @return This function is used for its side effects.
#' @rdname open_graph_browser
#' @export
set_open_graph_browser <- function() {
  options(theme61.open_in_browser = TRUE)

  invisible(TRUE)
}

#' @rdname open_graph_browser
#' @export
unset_open_graph_browser <- function() {
  options(theme61.open_in_browser = FALSE)

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
  options(theme61.default_save_format = format)

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
  options(theme61.default_save_format = NULL)

  invisible(FALSE)
}
