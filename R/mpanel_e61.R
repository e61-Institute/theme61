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
#' @param ... Plot objects to put on the panel.
#' @inheritParams labs_e61
#' @inheritParams cowplot::plot_grid
#' @param title_max_char,footnote_max_char Numeric. Set the maximum number of
#'   characters per line in the title, subtitle, sources or footnotes. The
#'   default is roughly appropriate for the default graph dimensions in
#'   \code{e61_save}.
#' @param title_wrap,footnote_wrap Logical. Enables text wrapping for the title,
#'   subtitle, sources or footnotes. Defaults to TRUE.
#' @param title_adj Rescales the size of the title text to be slightly larger
#'   than the titles of the subplots (default is 1.1). 2 doubles the font size.
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
#'    subtitle = "Graph subtitle",
#'    footnotes = c("Footnote 1", "Footnote 2"),
#'    sources = c("Source 1", "Source 2"))

mpanel_e61 <-
  function(...,
           plotlist = NULL,
           title,
           subtitle = NULL,
           footnotes = NULL,
           sources = NULL,
           title_max_char = 90,
           subtitle_max_char = 90,
           footnote_max_char = 130,
           title_wrap = TRUE,
           subtitle_wrap = TRUE,
           footnote_wrap = TRUE,
           title_adj = 1.1,
           ncol = 2,
           nrow = NULL,
           rel_heights = NULL,
           align = c("v", "none", "h", "hv"),
           axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr")
           ) {

    # Prep header/footer text
    lab_text <- label_wrap(
      title = title,
      subtitle = subtitle,
      footnotes = footnotes,
      sources = sources,
      title_max_char = title_max_char,
      subtitle_max_char = subtitle_max_char,
      footnote_max_char = footnote_max_char,
      title_wrap = title_wrap,
      subtitle_wrap = subtitle_wrap,
      footnote_wrap = footnote_wrap
    )

    # Gather the plots
    plots <- c(list(...), plotlist)
    if (is.null(nrow)) {
      nrow <- ceiling(length(plots) / ncol)
    }

    # Put together the panels
    panels <- cowplot::plot_grid(plotlist = plots,
                             align = align,
                             axis = axis,
                             ncol = ncol,
                             nrow = nrow
                             )

    # These all need to be lists
    panels <- list(panels)
    lab_head <- list()
    lab_foot <- list()

    lab_head$title <-
      cowplot::ggdraw() +
      cowplot::draw_label(
        lab_text$title,
        fontface = "bold",
        x = 0.5,
        hjust = 0.5,
        size = 12 * title_adj
      )

    if (!is.null(lab_text$subtitle)) {
      lab_head$subtitle <-
        cowplot::ggdraw() +
        cowplot::draw_label(
          lab_text$subtitle,
          fontface = "plain",
          x = 0.5,
          hjust = 0.5,
          size = 12
        )

    }

    if (!is.null(lab_text$caption)) {

      lab_foot$footer <-
        cowplot::ggdraw() +
        cowplot::draw_label(lab_text$caption,
                            x = 0,
                            hjust = 0,
                            size = 9) +
        ggplot2::theme(plot.margin = margin(0, 0, 0, 3))

    }

    # Space for subtitle if required
    sub_h <- if (!is.null(lab_text$subtitle)) 0.05 else NULL

    # Adjust the footer height depending on how much text there is
    cap_h <-
      if (!is.null(lab_text$caption)) 0.05 * (n_count(lab_text$caption) + 1) else NULL

    # Use automatically generated relative heights if the user does not specify their own
    if (is.null(rel_heights)) rel_heights <- c(0.05, sub_h, 1, cap_h)

    gg <- cowplot::plot_grid(
      plotlist = c(lab_head, panels, lab_foot),
      ncol = 1,
      rel_heights = rel_heights
    )

    # Add some extra info on the multi-panel attributes
    attr(gg, "panel_rows") <- nrow
    attr(gg, "panel_cols") <- ncol

    return(gg)

}
