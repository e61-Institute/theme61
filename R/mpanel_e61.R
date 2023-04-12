#' Create a multi-panel graph with e61 formatting
#'
#' @description Wrapper around \link[ggpubr]{ggarrange} and \code{labs_e61} to
#'   create multi-panel graphs with appropriate title and footer formatting.
#'
#'   This function is designed for creating 2x2 panel graphs, although it should
#'   work for any arrangement of panels (e.g. 2x1, 3x2, etc.). Your mileage may
#'   vary.
#'
#'   Subtitles are currently not supported in the multi-panel header.
#'
#'   When saving multi-panel graphs using \code{save_e61()} you will need to
#'   change (increase) the width and height arguments to reflect the larger size
#'   of multi-panel graphs.
#'
#' @inheritDotParams ggpubr::ggarrange
#' @inheritParams labs_e61
#'
#' @return ggplot2 object
#' @export
#' @examples
#'  gg <- ggplot2::ggplot() +
#'    labs_e61(title = "Figure", y = "%") +
#'    scale_y_continuous_e61(limits = c(0, 10, 2.5)) +
#'    theme_e61()
#'
#'  mpanel_e61(gg, gg, gg, gg,
#'    title = "Multi-panel graph title",
#'    footnotes = c("Footnote 1", "Footnote 2"),
#'    sources = c("Source 1", "Source 2"))

mpanel_e61 <-
  function(...,
           title,
           footnotes = NULL,
           sources = NULL,
           title_max_char = 35,
           footnote_max_char = 55,
           title_wrap = TRUE,
           footnote_wrap = TRUE
           ) {

    # Stop titles from being too long by wrapping it to multiple lines
    if (title_wrap) {
      title <- paste(strwrap(title, width = title_max_char), collapse = "\n")
    }

    # Footnotes
    if (!is.null(footnotes)) {

      # Sense check inputs
      if (!is.vector(footnotes) || !is.character(footnotes))
        stop("footnotes must be a vector of strings.")

      # Stops footnote text from spilling over the RHS of graphs if they are lengthy
      if (footnote_wrap) {
        footnotes <-
          sapply(footnotes, function(x)
            paste(strwrap(x, width = footnote_max_char), collapse = "\n"))
      }

      # Creates the correct number of asterisks
      footnotes <- data.frame(n = seq(1, length(footnotes)), text = footnotes)
      footnotes$n <- strrep("*", footnotes$n)
      footnotes <- paste0(footnotes$n, " ", footnotes$text)

    }

    # Sources
    if (!is.null(sources)) {

      # Sense check inputs
      if (!is.vector(sources) || !is.character(sources))
        stop("sources must be a vector of strings.")

      # Source list should be in alphabetical order
      sources <- sort(sources)

      # Construct the list of sources
      source_list <- paste(sources, collapse = "; ")
      sources <-
        paste0(ifelse(length(sources) > 1, "Sources: ", "Source: "), source_list)

      # Stops sources text from spilling over the RHS of graphs if they are
      # lengthy
      if (footnote_wrap) {
        sources <-
          paste(strwrap(sources, width = footnote_max_char), collapse = "\n")
      }

    }

    # Put it all together
    caption <- paste0(c(footnotes, sources), collapse = "\n")
    if (caption == "") caption <- NULL # Return NULL caption if blank

    # Put together the panels
    gg <- ggpubr::ggarrange(...)

    gg <- ggpubr::annotate_figure(
      gg,
      top = ggpubr::text_grob(
        title,
        face = "bold",
        size = 12
        ),
      bottom = ggpubr::text_grob(
        caption,
        just = "left",
        x = 0.05,
        size = 9
        )
      )

    return(gg)

}
