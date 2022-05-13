#' Saves ggplot graphs with sensible defaults
#'
#' @description Designed to save ggplot graphs made with the e61 theme with
#'   sensible defaults that ensure the text size is appropriately proportioned
#'   given default sizing.
#'
#'   See \code{\link[ggplot2]{ggsave}} for details on custom function arguments.
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

e61_save <-
  function(filename,
           resize = NULL,
           width = 8,
           height = 6,
           units = "in",
           scale = 1,
           dpi = 100,
           ...) {

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
      width = width,
      height = height,
      units = units,
      scale = scale,
      dpi = dpi,
      ...
    )
  }
