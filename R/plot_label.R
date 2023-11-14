#' Add on-graph labels to graphs
#'
#' @description Add text labels for lines, columns or other elements directly
#'   onto the graph plot. This is preferred over using legends.
#'
#' @param label String vector. Label text to be displayed.
#' @param x Numeric or string vector. X-axis positions of the label text.
#' @param y Numeric or string vector. Y-axis positions of the label text.
#' @param colour (optional) Vector of colour names or strings. Default uses the
#'   e61 palette.
#' @param size (optional) Integer. Size of the text, the default size should be
#'   appropriate in most cases.
#' @param hjust (optional) A numeric value from 0-1. Adjusts the alignment of
#'   the text. 0 left-aligns (default), 0.5 centre-aligns and 1 right-aligns.
#' @param geom (optional) String. Either "text" (default) or "label". "label"
#'   adds a white box around the text which could be useful sometimes.
#' @param angle (optional) Numeric. Rotate the labels. Defaults to 0 which is
#'   normal left-to-right text.
#' @param facet_name,facet_value (optional) String. Specify the name of the
#'   facetting variable in `facet_name` and the panel to show the labels in
#'   using `facet_value`. Defaults to NULL which shows the labels on all facets.
#'   You must specify both `facet_name` and `facet_value` or leave both as NULL.
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
           angle = 0,
           facet_name = NULL,
           facet_value = NULL
           ) {

    if (!all.equal(length(label), length(x), length(y)))
      stop("The number of x and y positions must equal the number of labels.")

    geom <- match.arg(geom)

    if (!is.null(facet_name) && length(facet_name) != 1)
      stop("facet_name must be a string of length 1.")

    if (xor(is.null(facet_value), is.null(facet_name)))
      stop("You must provide both `facet_name` and `facet_value` or leave both as NULL.")

    # Set up colours
    if (length(colour) == 1 && is.na(colour)) {
      colour <- palette_e61(length(label))
    } else if (length(colour) == 1 && !is.na(colour)) {
      colour <- rep(colour, length(label))
    } else if (length(colour) != length(label)) {
      stop("The number of colours must equal the number of labels.")
    } else if (length(colour) == length(label)) {
      colour <- palette_e61(length(label))
    }

    # Set up facets
    if (!is.null(facet_name) && length(facet_value) == 1) {
      facet_value <- rep(facet_value, length(label))
    } else if (!is.null(facet_value) && length(facet_value) != length(label)) {
      stop("facet_value must be 1 or equal to the number of labels.")
    }

    # Automatically convert dates to dates if specified, so the user doesn't have
    # to wrap dates in as.Date() which saves some room.
    if (class(try(as.Date(as.character(x)), silent = TRUE)) != "try-error") {
      x <- as.Date(x)
    }

    # Arg length checking helper
    len_chk <- function(vec, len = length(label)) {
      if (length(vec) == len) return(vec)
      if (length(vec) != 1) stop(substitute(vec), " must be length ", length(label), " or 1.")
      return(rep(vec, len))
    }

    plot_lab_data <- data.table::data.table(
      label = label,
      x = x,
      y = y,
      colour = colour,
      size = len_chk(size),
      hjust = len_chk(hjust),
      angle = len_chk(angle),
      facet = facet_value
    )

    # Facet column needs to have the same name as the facetting variable
    if (!is.null(facet_name)) {
      data.table::setnames(plot_lab_data, old = "facet", new = facet_name)
    }

    # Create the geom
    if (geom == "text") {
      retval <- geom_text(
        data = plot_lab_data,
        mapping = aes(x, y, label = label),
        colour = colour, size = size, hjust = hjust, angle = angle
      )
    } else if (geom == "label") {
      retval <- geom_label(
        data = plot_lab_data,
        mapping = aes(x, y, label = label),
        colour = colour, size = size, hjust = hjust, angle = angle
      )
    }

    # Only add the functionality to adjust the size of the plot labels if the
    # user did not manually enter a size
    if (size == 3.5) {
      attr(retval, "adj_plot_label") <- TRUE
    }

    return(retval)

  }

#' @rdname plot_label
#' @export
plab <- plot_label
