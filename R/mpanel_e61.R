#' Create a multi-panel graph with e61 formatting
#'
#' @description Wrapper around \link[cowplot]{plot_grid} and \code{labs_e61} to
#'   create multi-panel graphs with appropriate title and footer formatting.
#'
#'   This function is designed for creating 2x2 panel graphs, although it should
#'   work for any arrangement of panels (e.g. 2x1, 3x2, etc.). Your mileage may
#'   vary.
#'
#'   When saving multi-panel graphs using \code{save_e61()} you will need to
#'   change (increase) the width and height arguments to reflect the larger size
#'   of multi-panel graphs.
#'
#' @details \strong{Read this if your titles and stuff are getting cut off.}
#'
#'   The function tries to be smart when setting the values for
#'   \code{rel_heights}, but it can be incorrect due to differences in the
#'   amount of text in each component.
#'
#'   The way \code{mpanel_e61} works is that the title, subtitle, graph panels
#'   and footers are all separate components that are then put together after
#'   they have been independently generated. The purpose of \code{rel_heights}
#'   is to specify what proportion of the final plot each of these components
#'   require. As a result, if the values are too small for a given component,
#'   part of it will get visually cut off.
#'
#'   The function tries to increase the amount of space given to a component if,
#'   for example, your title has two lines, or you write a really long footnote
#'   that spans many lines of text. But this is an inexact science. In all
#'   likelihood, you will need to specify your own values, which why this
#'   paragraph of text tries to explain the underlying function so you aren't
#'   just blindly inputting numbers.
#'
#'   The default values for \code{rel_heights} for a 1-line title, 1-line
#'   subtitle, 1-line footnote and 1-line sources list is \code{c(0.05, 0.05, 1,
#'   0.1)}. If you only have a title and footnotes/sources then only 3 values
#'   are needed: \code{c(0.05, 1, 0.1)}.
#'
#'   These values are all relative ratios, meaning, in the second example, the
#'   title gets \eqn{\frac{0.05}{1.15}}, or around 4 per cent of the total graph
#'   height. This means that if your graph panels are really tall (e.g. you make
#'   a 3x2 multi-panel), you will need to reduce the share of the space
#'   allocated to the titles or you will have extra whitespace (I think? Haven't
#'   actually tested this).
#'
#' @param ... Plot objects to put on the panel.
#' @inheritParams labs_e61
#' @inheritParams cowplot::plot_grid
#' @param title_adj Rescales the size of the title text to be slightly larger
#'   than the titles of the subplots (default is 1.1). 2 doubles the font size.
#' @param rel_heights A numeric vector giving the relative proportions of each
#'   graph component (title, plots, footer (optional)). See the Details for more
#'   detail.
#' @param show_height Logical. Prints a message showing the \code{rel_heights}
#'   used when producing the graph. Mostly used for testing. Defaults to FALSE.
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
           title = NULL,
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
           axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
           show_height = FALSE
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

    # Space for title if required
    t_h <- if (!is.null(lab_text$title)) 0.07 else 0.001

    # Space for subtitle if required
    s_h <- if (!is.null(lab_text$subtitle)) 0.07 else NULL

    # Adjust the footer height depending on how much text there is
    f_h <-
      if (!is.null(lab_text$caption)) 0.06 * (n_count(lab_text$caption) + 1) else NULL

    # Calculate plot height
    p_h <- sum(t_h, s_h, 0.9, f_h) / 1.1
    p_h <- p_h * (1 + (nrow - 1) * 0.4) # Adjustment factor for >1 row plots

    # Use automatically generated relative heights if the user does not specify their own
    if (is.null(rel_heights)) rel_heights <- c(t_h, s_h, p_h, f_h)

    gg <- cowplot::plot_grid(
      plotlist = c(lab_head, panels, lab_foot),
      ncol = 1,
      rel_heights = rel_heights
    )

    # Add some extra info on the multi-panel attributes
    attr(gg, "panel_rows") <- nrow
    attr(gg, "panel_cols") <- ncol
    attr(gg, "panel_head") <- if (is.numeric(t_h) && is.numeric(s_h)) (t_h + s_h) / 0.07 else 0
    attr(gg, "panel_foot") <- if (is.numeric(f_h)) f_h / 0.06 else 0

    # Print the rel_heights used
    if (show_height) message(paste(rel_heights, collapse = ", "))

    return(gg)

}
