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
#' @param ... Optional named vectors. If the plot is facetted, you can restrict
#'   the label(s) to a specific panel by supplying the facetting variable(s) as
#'   named arguments (e.g. `grp = "1"`). For facet grids, supply all facet vars.
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
           facet_variable = NULL,
           ...) {

    # Hard deprecate old args if used
    if (!is.null(facet_name) || !is.null(facet_value)) {
      lifecycle::deprecate_stop(
        when = "0.7.1",
        what = "theme61::plot_label(facet_name = '', facet_value = '')",
        with = "theme61::plot_label(<facet_var> = <facet_value>)",
        details = paste0(
          "Facet targeting is now done by passing the facet variable(s) directly.\n",
          "Example:\n",
          "  plot_label('a point', 1.25, 1, grp = '1')"
        )
      )
    }

    if (!all.equal(length(label), length(x), length(y)))
      stop("The number of x and y positions must equal the number of labels.")

    geom <- match.arg(geom)

    # Set up colours
    if (length(colour) == 1 && is.na(colour)) {
      colour <- palette_e61(length(label))
    } else if (length(colour) == 1 && !is.na(colour)) {
      colour <- rep(colour, length(label))
    } else if (length(colour) != length(label)) {
      stop("The number of colours must equal the number of labels.")
    }

    # Automatically convert dates to dates if specified, so the user doesn't have
    # to wrap dates in as.Date() which saves some room.
    if (class(try(as.Date(as.character(x)), silent = TRUE)) != "try-error") {
      x <- as.Date(x)
    }

    extra <- list(...)

    # If user supplied extras, they must all be named (facet vars etc.)
    if (length(extra)) {
      nms <- names(extra)
      if (is.null(nms) || any(!nzchar(nms))) {
        stop(
          "All additional arguments to plot_label() must be named.\n",
          "Example: plot_label('a point', 1.25, 1, grp = '1')"
        )
      }
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
        extra = extra,
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

  if (!is.null(plot$data) && facet_name %in% names(plot$data)) {
    return(plot$data[[facet_name]])
  }

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

# Best-effort extractor of facet variable names from a ggplot
.theme61_get_facet_vars <- function(plot) {
  f <- plot$facet
  if (is.null(f) || is.null(f$params)) return(character())

  # facet_wrap(~a + b)
  if (!is.null(f$params$facets)) {
    facets <- f$params$facets
    nm <- names(facets)
    if (!is.null(nm) && all(nzchar(nm))) return(nm)
    return(vapply(facets, function(q) rlang::as_name(q), character(1)))
  }

  # facet_grid(rows ~ cols)
  vars <- character()

  if (!is.null(f$params$rows)) {
    rows <- f$params$rows
    if (inherits(rows, "quosure")) vars <- c(vars, rlang::as_name(rows))
    if (is.list(rows)) vars <- c(vars, vapply(rows, rlang::as_name, character(1)))
  }

  if (!is.null(f$params$cols)) {
    cols <- f$params$cols
    if (inherits(cols, "quosure")) vars <- c(vars, rlang::as_name(cols))
    if (is.list(cols)) vars <- c(vars, vapply(cols, rlang::as_name, character(1)))
  }

  vars <- unique(vars)
  vars <- vars[vars != "."]
  vars
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
    angle = .theme61_plab_len_chk(object$angle, n)
  )

  # Add any extra columns (facet vars, etc.)
  if (length(object$extra)) {
    for (nm in names(object$extra)) {
      v <- object$extra[[nm]]
      if (length(v) == 1) v <- rep(v, n)
      if (length(v) != n) {
        stop("`", nm, "` must be length 1 or the number of labels (", n, ").")
      }
      plot_lab_data[[nm]] <- v
    }
  }

  facet_vars <- .theme61_get_facet_vars(plot)

  if (length(facet_vars)) {
    have <- intersect(facet_vars, names(plot_lab_data))

    # If user tried to target panels but didn't provide all facet vars -> helpful error
    if (length(have) > 0 && length(have) < length(facet_vars)) {
      missing <- setdiff(facet_vars, have)
      stop(
        "This plot is facetted by: ", paste(facet_vars, collapse = ", "), "\n",
        "To place labels in a specific panel, supply *all* facet variables as named arguments.\n",
        "Missing: ", paste(missing, collapse = ", "), "\n",
        "Example: plot_label('a', 1, 1, ",
        paste0(facet_vars, " = '...'", collapse = ", "),
        ")"
      )
    }

    if (length(have) == length(facet_vars)) {
      # User supplied all facet vars: coerce to plot prototypes (preserve ordered levels)
      for (fv in facet_vars) {
        proto <- .theme61_find_facet_proto(plot, fv)
        if (is.null(proto)) {
          stop(
            "Facet variable `", fv, "` was not found in the plot data.\n",
            "Check that you used the correct facetting variable name."
          )
        }
        plot_lab_data[[fv]] <- .theme61_coerce_to_proto(plot_lab_data[[fv]], proto)
      }
    } else {
      # User supplied no facet vars: expand labels across existing panels so per-row
      # aesthetics (colour/size/...) get replicated consistently.
      built <- ggplot2::ggplot_build(plot)
      lay <- built$layout$layout

      # Keep only facet columns that actually exist in layout (defensive)
      facet_vars2 <- facet_vars[facet_vars %in% names(lay)]
      if (length(facet_vars2)) {
        panels <- unique(lay[, facet_vars2, drop = FALSE])

        # Cross join: each panel gets all label rows
        idx <- rep(seq_len(nrow(panels)), each = n)
        plot_lab_data <- plot_lab_data[rep(seq_len(.N), times = nrow(panels))]
        for (fv in facet_vars2) {
          plot_lab_data[[fv]] <- panels[[fv]][idx]
        }
      }
    }
  }

  # IMPORTANT: use per-row vectors from plot_lab_data so they match any expansion
  if (object$geom == "text") {
    retval <- ggplot2::geom_text(
      data = plot_lab_data,
      mapping = ggplot2::aes(x, y, label = label),
      colour = plot_lab_data$colour,
      size   = plot_lab_data$size,
      hjust  = plot_lab_data$hjust,
      angle  = plot_lab_data$angle,
      inherit.aes = FALSE
    )
  } else {
    retval <- ggplot2::geom_label(
      data = plot_lab_data,
      mapping = ggplot2::aes(x, y, label = label),
      colour = plot_lab_data$colour,
      size   = plot_lab_data$size,
      hjust  = plot_lab_data$hjust,
      angle  = plot_lab_data$angle,
      inherit.aes = FALSE
    )
  }

  if (isTRUE(object$adj_plot_label)) {
    attr(retval, "adj_plot_label") <- TRUE
  }

  retval
}

# ggplot2 v4+ hook
#' @export
update_ggplot.theme61_plot_label <- function(object, plot, ...) {
  plot + .theme61_build_plot_label_layer(object, plot)
}

# Back-compat hook (still used by ggplot2 add_ggplot path)
#' @export
ggplot_add.theme61_plot_label <- function(object, plot, object_name, ...) {
  update_ggplot.theme61_plot_label(object, plot, ...)
}

#' @rdname plot_label
#' @export
plab <- plot_label
