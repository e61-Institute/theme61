#' Add On-graph Labels to Graphs
#'
#' Wrapper function around \code{annotate()} that helps you add labels for
#' lines, columns or other elements directly onto the graph plot as a
#' replacement for legends outside the plot area.
#'
#' Automatically chooses the correct colour in the theme61 colour palette if you
#' provide the total number of labels, and the relative ordering of each label.
#'
#' @param label String. Label text to be displayed.
#' @param x Numeric or string. X-axis position of the label text.
#' @param y Numeric or string. Y-axis position of the label text.
#' @param n_labs Integer. Total number of labels on the graph.
#' @param n Integer. Indicate which (*n*th) element this label corresponds to.
#' @param size Integer. Size of the text, default (3.5) should be appropriate in
#'   most cases.
#'
#' @return ggplot2 object
#' @export
plot_label <- function(label, x, y, n_labs, n, size = 3.5) {

  # Automatically convert dates to dates if specified, so the user doesn't have
  # to wrap dates in as.Date() which saves some room.
  if (class(try(as.Date(x), silent = TRUE)) != "try-error") {
    x <- as.Date(x)
  }

  annotate("text", label = label, x = x, y = y, size = size,
           colour = e61_palette(n_labs)[[n]])

}
