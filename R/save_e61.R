#' Saves ggplot graphs with sensible defaults
#'
#' @description Designed to save ggplot graphs made with the e61 theme with
#'   sensible defaults that ensure the text size is appropriately proportioned
#'   given default sizing.
#'
#'   See \code{\link[ggplot2]{ggsave}} for details on custom function arguments.
#'
#' @details Note that you will need to change the \code{height} argument to
#'   ensure that the graph displays without excess white space above and below
#'   the panel (if the value is too high), or weirdly shrinking the graph (if
#'   the value is too low).
#'
#' @details Currently the only file formats supported are \code{.svg}
#'   (preferred) and \code{.png}. SVG is a modern vector graphics file format
#'   which means it can be scaled up and down in size without blurring or
#'   becoming pixelated. Use the PNG file format when vector graphics are not
#'   supported.
#'
#' @param resize Rescales the graph and text. Useful when you need a very large
#'   or small graph and cannot use a vector graphics format. This only works
#'   when saving to the PNG file format. A value of 2 doubles the graph
#'   dimensions.
#' @inheritDotParams ggplot2::ggsave
#' @export

save_e61 <-
  function(filename,
           plot = ggplot2::last_plot(),
           resize = NULL,
           width = 8.5,
           height = 9,
           units = "cm",
           scale = 1,
           dpi = 100,
           ...) {

    # Add a message for the user to specify their own height to dimension graphs correctly
    if(getOption("save_e61.message", TRUE)) {

      message(paste(
        "When you use", sQuote("save_e61()"), "to save images with sensible",
        "defaults, note that you may have to set the", sQuote("height"),
        "argument manually to a sensible value.",
        'This message is shown once per session and may be disabled by setting',
        "options('save_e61.message' = FALSE). See ?save_e61 for more details."))
      options("save_e61.message" = FALSE)
    }


    if (!grepl("(\\.png|\\.svg)", filename))
      stop("Only .svg and .png file formats are currently supported.")

    if (!is.null(resize)) {
      if (!grepl("\\.png", filename))
        stop("The file format must be .png")
      if (!is.numeric(resize))
        stop("resize must be numeric.")

      # Rescale elements as required
      width <- width * resize
      height <- height * resize
      dpi <- dpi * resize
      scale <-
        scale / resize # scale works inversely to size for reasons

    }

    ggplot2::ggsave(
      filename,
      plot = plot,
      width = width,
      height = height,
      units = units,
      scale = scale,
      dpi = dpi,
      ...
    )
  }
