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
#' @return Object to add to a ggplot (via `+`).
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
           facet_value = NULL) {

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

    structure(
      list(
        label = label,
        x = x,
        y = y,
        colour = colour,
        size = size,
        hjust = hjust,
        geom = geom,
        angle = angle,
        facet_name = facet_name,
        facet_value = facet_value,
        # preserve current behaviour: mark TRUE if default size used
        adj_plot_label = isTRUE(all.equal(size, 3.5))
      ),
      class = "theme61_plot_label"
    )
  }

# --- internal helpers ---------------------------------------------------------

.theme61_plab_len_chk <- function(vec, len) {
  if (length(vec) == len) return(vec)
  if (length(vec) != 1) stop(deparse(substitute(vec)), " must be length ", len, " or 1.")
  rep(vec, len)
}

.theme61_find_facet_proto <- function(plot, facet_name) {
  if (is.null(facet_name) || !nzchar(facet_name)) return(NULL)

  # Prefer plot$data if present
  if (!is.null(plot$data) && facet_name %in% names(plot$data)) {
    return(plot$data[[facet_name]])
  }

  # Fall back to any layer data that contains the facet var
  for (ly in plot$layers) {
    d <- ly$data
    if (!is.null(d) && facet_name %in% names(d)) {
      return(d[[facet_name]])
    }
  }

  NULL
}

.theme61_coerce_to_proto <- function(values, proto) {
  if (is.null(proto)) return(values)

  v <- as.character(values)

  if (is.ordered(proto)) {
    return(ordered(v, levels = levels(proto)))
  }
  if (is.factor(proto)) {
    return(factor(v, levels = levels(proto)))
  }

  values
}

.theme61_build_plot_label_layer <- function(object, plot) {
  n <- length(object$label)

  plot_lab_data <- data.table::data.table(
    label = object$label,
    x = object$x,
    y = object$y,
    colour = object$colour,
    size = .theme61_plab_len_chk(object$size, n),
    hjust = .theme61_plab_len_chk(object$hjust, n),
    angle = .theme61_plab_len_chk(object$angle, n),
    facet = object$facet_value
  )

  # Facet column needs to have the same name as the faceting variable
  if (!is.null(object$facet_name)) {
    data.table::setnames(plot_lab_data, old = "facet", new = object$facet_name)

    # coerce facet var to match plot prototype (preserve ordered levels)
    proto <- .theme61_find_facet_proto(plot, object$facet_name)
    plot_lab_data[[object$facet_name]] <-
      .theme61_coerce_to_proto(plot_lab_data[[object$facet_name]], proto)
  }

  if (object$geom == "text") {
    retval <- ggplot2::geom_text(
      data = plot_lab_data,
      mapping = ggplot2::aes(x, y, label = label),
      colour = object$colour, size = object$size, hjust = object$hjust, angle = object$angle,
      inherit.aes = FALSE
    )
  } else {
    retval <- ggplot2::geom_label(
      data = plot_lab_data,
      mapping = ggplot2::aes(x, y, label = label),
      colour = object$colour, size = object$size, hjust = object$hjust, angle = object$angle,
      inherit.aes = FALSE
    )
  }

  if (isTRUE(object$adj_plot_label)) {
    attr(retval, "adj_plot_label") <- TRUE
  }

  retval
}

# Register S3 methods

#' @export
update_ggplot.theme61_plot_label <- function(object, plot, ...) {
  plot + .theme61_build_plot_label_layer(object, plot)
}

#' @export
ggplot_add.theme61_plot_label <- function(object, plot, object_name) {
  plot + .theme61_build_plot_label_layer(object, plot)
}

#' @rdname plot_label
#' @export
plab <- plot_label
