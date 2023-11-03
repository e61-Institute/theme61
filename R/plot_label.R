#' Add on-graph labels to graphs
#'
#' @description Add text labels for lines, columns or other elements directly
#'   onto the graph plot. This is preferred over using legends.
#'
#' @param label String vector. Label text to be displayed.
#' @param x Numeric or string vector. X-axis positions of the label text.
#' @param y Numeric or string vector. Y-axis positions of the label text.
#' @param colour Optional vector of manually-selected colours. Default uses the
#'   order of colours in the e61 palette.
#' @param size Integer. Size of the text, the default size should be appropriate
#'   in most cases.
#' @param hjust A numeric value from 0-1. Adjusts the alignment of the text. 0
#'   left-aligns (default), 0.5 centre-aligns and 1 right-aligns.
#' @param geom String. Either "text" (default) or "label". "label" adds a white
#'   box around the text which could be useful sometimes.
#'
#' @return ggplot2 object
#' @export
plot_label <-
  function(label,
           x,
           y,
           colour = NA,
           size = 3.5,
           hjust = 0,
           geom = c("text", "label"),
           angle = 0) {

    geom <- match.arg(geom)

    if (!all.equal(length(label), length(x), length(y)))
      stop("The number of x and y positions must equal the number of labels.")

    # df requires vectors to be equal lengths
    if (length(colour) == 1 && is.na(colour)) {
      colour <- rep(colour, length(label))
    } else if (length(colour) != length(label)) {
      stop("The number of colours must equal the number of labels.")
    }

    len_chk <- function(vec, len = length(label)) {
      if (length(vec) == len) return(vec)

      if (length(vec) != 1) stop(substitute(vec), " must be length ", length(label), " or 1.")

      return(rep(vec, len))
    }

    plot_lab <- data.frame(
      label = label,
      x = x,
      y = y,
      colour = colour,
      size = len_chk(size),
      hjust = len_chk(hjust),
      geom = len_chk(geom),
      angle = len_chk(angle)
    )

    plot_lab$n_labs <- nrow(plot_lab)
    plot_lab$n <- 1:nrow(plot_lab)

    # This converts the data.frame into a list of lists, where the 1st level list
    # corresponds to each label, and the elements of the 2nd level list are the
    # components needed for plot_label()
    plot_lab <- split(plot_lab, seq(nrow(plot_lab)))
    plot_lab <- lapply(plot_lab, as.list)

    # Runs through each list element and calls plot_label() to generate the labels
    retval <-
      lapply(plot_lab, function(x) {
        plot_lab_(
          label = x$label,
          x = x$x,
          y = x$y,
          n_labs = x$n_labs,
          n = x$n,
          colour = x$colour,
          size = x$size,
          hjust = x$hjust,
          geom = x$geom,
          angle = x$angle
        )
      })

    return(retval)

  }

#' Helper function to add a single on-graph label to a graph
#' @noRd
plot_lab_ <-
  function(label,
           x,
           y,
           n_labs = NA,
           n = NA,
           colour = NA,
           size = 3.5,
           hjust = 0,
           geom = c("text", "label"),
           angle = 0) {
    geom <- match.arg(geom)

    if (is.na(colour) && (is.na(n_labs) || is.na(n)))
      stop("If 'colour' is not provided then you must specify values for 'n_labs' and 'n'.")

    if (is.na(colour)) colour <- palette_e61(n_labs)[[n]]

    # Automatically convert dates to dates if specified, so the user doesn't have
    # to wrap dates in as.Date() which saves some room.
    if (class(try(as.Date(as.character(x)), silent = TRUE)) != "try-error") {
      x <- as.Date(x)
    }

    # Only add the functionality to adjust the size of the plot labels if the user did not manually enter a size
    if(size == 3.5){
      attr(label, "adj_plot_label") <- TRUE
    }

    annotate(geom = geom, label = label, x = x, y = y, size = size, colour = colour, hjust = hjust, angle = angle)
  }

#' @rdname plot_label
#' @export
plab <- plot_label

#' @rdname plot_label
#' @export
mplab <- plot_label
