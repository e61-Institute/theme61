#' Saves ggplot graphs with sensible defaults
#'
#' @description Designed to save ggplot graphs made with the e61 theme with
#'   sensible defaults that ensure the text size is appropriately proportioned
#'   given default sizing.
#'
#'   Currently the only file formats supported are \code{.svg} (preferred) and
#'   \code{.png}. SVG is a modern vector graphics file format which means it can
#'   be scaled up and down in size without blurring or becoming pixelated. Use
#'   the PNG file format in the rare case that vector graphics are not
#'   supported.
#'
#'   See \code{\link[ggplot2]{ggsave}} for details on custom function arguments.
#'
#' @details Setting the correct height and width parameters is quite difficult
#'   due to the way that ggplot translates ggplot objects from pixels into
#'   physical dimensions (inches or centimetres). Font sizes are also
#'   transformed differently to other graph elements which adds another
#'   dimension of difficulty. This is why \code{save_e61} generally requires you
#'   to provide your own height and width arguments after some trial and error,
#'   and produces loud messages if you stick to the defaults, which I have tried
#'   to make not terrible.
#'
#'   If the width/height arguments are off, then the file you output will have
#'   excess space on the left/right or top/bottom (if the values are too high),
#'   or the graph itself will be shrunk and look weird (if the values are too
#'   low).
#'
#' @param resize Rescales the graph and text. Useful when you need a very large
#'   or small graph and cannot use a vector graphics format. This only works
#'   when saving to the PNG file format. A value of 2 doubles the graph
#'   dimensions.
#' @param filename File name to create on disk. Remember you must provide the
#'   file extension, e.g. \code{.svg}.
#' @param plot Plot object to save. Defaults to the last plot displayed so
#'   usually you do not need to provide this explicitly.
#' @param width Plot width in cm. Defaults to 8.5.
#' @param height Plot height in cm. The function will attempt to calculate an
#'   appropriate height based on the labels you have provided, but this is
#'   sensitive to small changes in the graph text so you should check if the
#'   automatic value is aesthetically appropriate (no excess whitespace).
#'   Otherwise, the function will default a value of 9 but this is unlikely to
#'   be appropriate.
#' @inheritDotParams ggplot2::ggsave scale dpi
#' @export

save_e61 <-
  function(filename,
           plot = ggplot2::last_plot(),
           width = NULL,
           height = NULL,
           resize = NULL,
           scale = 1,
           dpi = 100,
           ...) {

    if (!grepl("(\\.png|\\.svg)", filename))
      stop("You must provide a file extension. Only .svg and .png file formats are currently supported.")

    # Check if graph is horizontal
    flip <- isTRUE("CoordFlip" %in% class(ggplot2::ggplot_build(plot)$layout$coord))

    # Calculate graph height based on the graph labels for normal orientation graphs
    if (is.null(height) && !flip) {

      h <- 6.5

      # Helper function that counts the number of occurrences of \n
      n_count <- function(text)
        nchar(text) - nchar(gsub("\n", "", text, fixed = TRUE))

      # Calculate the height adjustment needed
      if (!is.null(plot$labels$title)) {
        t_adj <- 0.6 + n_count(plot$labels$title) * 0.3

      } else {
        t_adj <- 0
      }

      if (!is.null(plot$labels$subtitle)) {
        st_adj <- 0.5 + n_count(plot$labels$subtitle) * 0.3

      } else {
        st_adj <- 0
      }

      if (!is.null(plot$labels$caption)) {
        cp_adj <- 0.5 + n_count(plot$labels$caption) * 0.3

      } else {
        cp_adj <- 0
      }

    height <- h + t_adj + st_adj + cp_adj

    cli::cli_text(cli::col_green("Note: save_e61() has automatically set the height to ", height, ". Please check if this is actually appropriate for your graph."))

    }

    # Message for the user to specify their own height to dimension graphs
    # correctly. This message runs after the above so when the automatic height
    # setting is used it does not trigger.
    if (is.null(height) || isTRUE(getOption("save_e61.message"))) {

      cli::cli_text(cli::col_red("Note: When you use ", sQuote("save_e61()"), " to save images with defaults, you should set the ", sQuote("height"), " argument manually to your own value to avoid excess/insufficient whitespace on the rendered image."))
      cli::cli_text("Unfortunately the only way to check this is to open the rendered graphic and inspect it visually.")
      cli::cli_text("This message is shown if you leave 'height = NULL' and the automatic height functionality is not used. It may be disabled by setting options('save_e61.message' = FALSE). See ?save_e61 for more details.")

      options('save_e61.message' = FALSE)
    }

    # Fallback default dimensions if not otherwise specified
    if (is.null(width) && !flip) width <- 8.5
    if (is.null(height) && !flip) height <- 9

    # When coord_flip() is used to make a plot horizontal, the default dims are
    # too small
    if (is.null(width) && flip) width <- 17
    if (is.null(height) && flip) height <- 12

    if (!is.null(resize)) {
      if (!grepl("\\.png", filename))
        stop("The 'resize' argument is not supported unless the file format is .png")
      if (!is.numeric(resize))
        stop("'resize' must be numeric.")

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
      units = "cm",
      scale = scale,
      dpi = dpi,
      ...
    )
  }
