#' Create a multi-panel graph with e61 formatting
#'
#' @description Wrapper around \link[cowplot]{plot_grid} and \code{labs_e61} to
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
#' @inheritParams labs_e61
#' @inheritParams cowplot::plot_grid
#' @param title_max_char,footnote_max_char Numeric. Set the maximum number of
#'   characters per line in the title, subtitle, sources or footnotes. The
#'   default is roughly appropriate for the default graph dimensions in
#'   \code{e61_save}.
#' @param title_wrap,footnote_wrap Logical. Enables text wrapping for the title,
#'   subtitle, sources or footnotes. Defaults to TRUE.
#' @param title_adj Rescales the size of the title text (default is 1). 2
#'   doubles the font size.
#' @param rel_heights A numeric vector giving the relative proportions of each
#'   graph component (title, plots, footer (optional)).
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
           plotlist = NULL,
           title,
           footnotes = NULL,
           sources = NULL,
           title_max_char = 90,
           footnote_max_char = 130,
           title_wrap = TRUE,
           footnote_wrap = TRUE,
           title_adj = 1,
           ncol = 2,
           nrow = NULL,
           rel_heights = NULL,
           align = c("none", "h", "v", "hv"),
           axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr")
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

    # Gather the plots
    plots <- c(list(...), plotlist)
    if (is.null(nrow)) {
      nrow <- ceiling(length(plots) / ncol)
    }

    # Put together the panels
    gg <- cowplot::plot_grid(plotlist = plots,
                             align = align,
                             axis = axis,
                             ncol = ncol,
                             nrow = nrow
                             )

    title <-
      cowplot::ggdraw() +
      cowplot::draw_label(title,
                 fontface = "bold",
                 x = 0.5, hjust = 0.5,
                 size = 12 * title_adj
      )

    if (!is.null(caption)) {

      footer <-
        cowplot::ggdraw() +
        cowplot::draw_label(caption,
                   x = 0, hjust = 0,
                   size = 9
        ) +
        ggplot2::theme(plot.margin = margin(0, 0, 0, 3))

    }

    # Adjust the footer height depending on how much text there is
    if (!is.null(caption)) {
      cap_h <- 0.05 * (n_count(caption) + 1)

      if (is.null(rel_heights)) rel_heights <- c(0.05, 1, cap_h)

      gg <- cowplot::plot_grid(
        title, gg, footer,
        ncol = 1,
        rel_heights = rel_heights
      )
    } else {

      if (is.null(rel_heights)) rel_heights <- c(0.05, 1)

      gg <- cowplot::plot_grid(
        title, gg,
        ncol = 1,
        rel_heights = rel_heights
      )
    }

    # Add some extra info on the multi-panel attributes
    attr(gg, "panel_rows") <- nrow
    attr(gg, "panel_cols") <- ncol

    return(gg)

}
