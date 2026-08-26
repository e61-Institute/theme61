#' Set various options in the theme61 package
#'
#' To see the list of available options, just run the function in the console
#' with no arguments i.e. \code{set_t61_options()}.
#'
#' @param opt A named list of options to set. See Details for available options.
#'
#' @details The following options are available to set:
#'  \itemize{
#'    \item \code{theme61.auto_label}: If TRUE (default), \code{plot_label()} text without an explicit `x`/`y` gets automatically positioned by \code{save_e61()} (see \code{?plot_label}). Set to FALSE to turn automatic positioning off entirely and restore the previous behaviour, where \code{x}/\code{y} are always required (\code{plot_label()} errors immediately if you omit them, regardless of \code{auto_position}) -- no auto-positioning work is attempted, so there's no performance cost from the feature at all.
#'    \item \code{theme61.auto_theme}: If TRUE (default), \code{theme_e61()} is automatically applied whenever you call \code{ggplot()}. Set to FALSE to turn this off and apply your own theme instead.
#'    \item \code{theme61.autolabel_fast_msg}: If TRUE (default), the first auto-positioned \code{plot_label()} text shown in the Viewer pane preview (or any \code{save_e61(fast_labels = TRUE)} call) shows a one-off reminder that the preview uses a quick placement heuristic, not the real collision-avoiding search -- labels may overlap there even when \code{save_e61()} would place them cleanly. Turns itself off after showing once; set back to TRUE to see it again.
#'    \item \code{theme61.base_size}: The base font size for graphs. This is 10 by default.
#'    \item \code{theme61.default_save_format}: The default file save format if format is not specified in [save_e61] and the file extension is not provided in \code{filename}. Unset by default, in which case [save_e61]'s own default (all supported formats: svg, pdf, eps, png, jpg) is used. Set via \code{set_format()} (or this function) to restrict the default(s); clear with \code{unset_format()} to go back to saving every format.
#'    \item \code{theme61.disable_spellcheck}: If TRUE, [save_e61]'s spell-checker is skipped entirely, regardless of its \code{spell_check} argument. This is FALSE by default.
#'    \item \code{theme61.iterate_mode}: If TRUE, all of theme61's automatic styling and Viewer pane preview rendering is skipped, so graphs print to the Plots pane with plain ggplot2 defaults as fast as possible. This is FALSE by default. Masked functions (\code{ggsave()}, \code{labs()}, \code{facet_wrap()}, \code{facet_grid()}) also stop redirecting to their theme61 equivalents and pass straight through to the underlying ggplot2 function instead. Any theme61 functions you call explicitly (e.g. \code{scale_colour_e61()}, \code{theme_e61()}, \code{labs_e61()}) still apply as normal, since they become part of the plot object regardless of this option.
#'    \item \code{theme61.open_in_browser}: If TRUE, graphs will also open in the browser in addition to the Viewer pane. This is FALSE by default.
#'    \item \code{theme61.preview_on_print}: If TRUE (default), graphs will be automatically previewed in the Viewer pane when printed to the console.
#'    \item \code{theme61.sec_axis_msg}: If TRUE (default), [sec_rescale_inv] shows a one-off reminder that rescaled secondary axis changes need the graph code run twice to take effect. Turns itself off after showing once; set back to TRUE to see it again.
#'  }
#'
#' @section Environment variables: A few behaviours run once, when theme61 is
#'   loaded (e.g. by \code{library(theme61)}), before any \code{options()} call
#'   in your script would take effect. These can't be controlled by
#'   \code{set_t61_options()} - instead, set the corresponding environment
#'   variable to \code{"1"} \emph{before} theme61 is loaded, e.g. in your
#'   \code{.Renviron} file or CI configuration:
#' \itemize{
#'   \item \code{THEME61_DISABLE_FONT_DOWNLOAD}: Skips downloading/registering the PT Sans font. Useful on CI or airgapped machines with no internet access. Enabled (font download happens) by default.
#'   \item \code{THEME61_DISABLE_GEOM_DEFAULTS}: Skips overwriting ggplot2's session-wide geom colour/fill defaults (e.g. \code{geom_point()}'s default colour). Enabled (defaults are overwritten) by default.
#'   \item \code{THEME61_DISABLE_VERSION_CHECK}: Skips the startup check against GitHub for a newer theme61 release. Enabled (the check runs) by default.
#' }
#'
#' @examples
#' \dontrun{
#' # Set the default save format to "png"
#' set_t61_options(list(theme61.default_save_format = "png"))
#'
#' # Environment variables must be set before library(theme61) is called,
#' # e.g. at the top of your script or in .Renviron:
#' Sys.setenv(THEME61_DISABLE_VERSION_CHECK = "1")
#' library(theme61)
#' }
#'
#' @export
set_t61_options <- function(opt = NULL) {
  if (is.null(opt)) {
    t61_opts <- names(options())[data.table::like(names(options()), "^theme61\\..*")] |> as.list()

    t61_set_opts <- sapply(t61_opts, options)

    # Print options as a pretty list displayed as 'option = value'
    bullets <- paste0(names(t61_set_opts), " = ", t61_set_opts)
    cli::cli_bullets(c(
      "i" = "Current theme61 options:",
      stats::setNames(bullets, rep(" ", length(bullets)))
    ))

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
