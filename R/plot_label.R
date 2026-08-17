#' Add on-graph labels to graphs
#'
#' @description Add text labels for lines, columns or other elements directly
#'   onto the graph plot. This is preferred over using legends.
#'
#' @param label (optional, see Details) String vector. Label text to be
#'   displayed. If omitted, derived from a `scale_colour_manual()`/
#'   `scale_fill_manual()` on the plot, if there is one.
#' @param x (optional, see Details) Numeric or string vector. X-axis
#'   positions of the label text. If supplied, this exact position is
#'   always used.
#' @param y (optional, see Details) Numeric or string vector. Y-axis
#'   positions of the label text. See `x`.
#' @param colour (optional, see Details) Vector of colour names or strings.
#'   Defaults to a `scale_colour_manual()`/`scale_fill_manual()` on the plot,
#'   if there is one, else the e61 palette.
#' @param size (optional) Integer. Size of the text, the default size should be
#'   appropriate in most cases.
#' @param hjust (optional) A numeric value from 0-1. Adjusts the alignment of
#'   the text. 0 left-aligns (default), 0.5 centre-aligns and 1 right-aligns.
#' @param geom (optional) String. Either "text" (default) or "label". "label"
#'   adds a white box around the text which could be useful sometimes.
#' @param angle (optional) Numeric. Rotate the labels. Defaults to 0 which is
#'   normal left-to-right text. See Details for how this interacts with `x`/`y`.
#' @param panel Optional named list. If the plot is facetted, you can restrict
#'   the label(s) to a specific panel by supplying the facetting variable(s) as
#'   a named list, see Details for the syntax.
#' @param auto_position Logical. If TRUE (default), `save_e61()` will try to
#'   automatically reposition the label to a nearby, non-overlapping spot on
#'   the chart. See Details for exactly when this applies and how positions
#'   are chosen. Set to FALSE to always use the exact `x`/`y` you supply --
#'   `x` and `y` are then required.
#' @param print_position Logical. If TRUE, print the plot's final
#'   (auto-positioned) label `label`/`x`/`y` to the console, as copy-pasteable
#'   `plot_label()` arguments, whenever the plot is displayed -- no need to
#'   call `save_e61()` first. Useful for grabbing the chosen positions once
#'   so you can pin them (or hand-tweak just one or two) instead of
#'   auto-positioning every time. Defaults to FALSE.
#' @param facet_name,facet_value `r lifecycle::badge("deprecated")`
#'
#' @details
#' ## Default label text and colour
#' If the plot has a `scale_colour_manual()`/`scale_fill_manual()` (checked
#' in that order; a theme61 wrapper that constructs one, e.g.
#' `scale_colour_e61_aus()`, counts too), `label` and `colour` can be
#' derived from it instead of the e61 palette:
#' * If you omit `label` entirely, it defaults to that scale's own levels
#'   (i.e. what a legend would show), in their resolved order -- so
#'   `plot_label()` with no arguments at all labels every series using its
#'   exact data value and assigned colour.
#' * If you supply `label` but omit `colour`, each label's colour is taken
#'   from the scale in the same order -- this assumes `label` is written in
#'   the same order as the scale's levels. If `label` has more entries than
#'   the scale has levels for, this is skipped and the e61 palette is used
#'   instead (rather than guessing a partial match).
#' * An explicit `colour` always wins outright over any of the above.
#'
#' Only a scale with fixed, explicit values (`scale_colour_manual()`/
#' `scale_fill_manual()`) is used this way -- an algorithmic discrete scale
#' (e.g. `scale_colour_e61()`, `scale_colour_brewer()`) doesn't count, since
#' it has no fixed "colours the user chose" to read.
#'
#' ## Automatic positioning
#' When `auto_position = TRUE` (the default), `save_e61()` tries to move the
#' label to a nearby, non-overlapping spot -- but only for single-panel
#' (unfacetted) charts where the label's colour matches a
#' line/point/column/area/`geom_pointbar()` series in the plot (`colour` for
#' lines, points and `geom_pointbar()`, `fill` for columns and areas), and
#' only for unrotated text (`angle = 0`). For an area series, the label is
#' placed fully inside the band where there's room, recoloured to contrast
#' with the fill, or outside it (in the fill's own colour) where the band is
#' too narrow. For a `geom_pointbar()` series, the buffer accounts for the
#' full error-bar extent, not just the point.
#'
#' If you supply `x`/`y`, that position is always used exactly as given --
#' the placement algorithm never runs for that label. If you don't, the
#' fallback order is: (1) a good spot found by the placement algorithm; (2)
#' any collision-free spot on the chart (i.e. empty space), even if it's not
#' a particularly good one; (3) the centre of the panel, so the label stays
#' visible rather than vanishing.
#'
#' A facetted plot, or rotated text (`angle != 0`), has no automatic
#' positioning to fall back on, so `x`/`y` are required in those cases (as
#' they are whenever `auto_position = FALSE`).
#'
#' Set `theme61.auto_label = FALSE` (see [set_t61_options()]) to turn
#' automatic positioning off globally -- `x`/`y` are then always required,
#' the same as `auto_position = FALSE`, and no auto-positioning work is
#' attempted at all (no performance cost from the feature).
#'
#' ## Facet targeting
#' The syntax for getting labels to appear on certain facet panels is as
#' follows.
#'
#' For facet wraps, supply a named list with the facetting variable name(s)
#' and the facet value(s) you want the labels to appear on. For example, to
#' get labels to appear only on panel `1`, use `panel = list(grp = "1")`. If
#' you have 2 labels that you want to appear on panels `1` and `2`, use `panel
#' = list(grp = c("1", "2"))`.
#'
#' For facet grids, you need to supply both the x- and y-dimension facet
#' variables to get the plot labels to appear correctly. For example, if your
#' facet variables are `r` and `c`, use `panel = list(r = "A", c = "1")` to
#' get the labels to appear on the panel at row `A` and column `1`. If you
#' have 2 labels you want to appear on panel `A1` and `B2`, use
#' `panel = list(r = c("A", "B"), c = c("1", "2"))`.
#'
#' @return Object to add to a ggplot (via `+`).
#' @export
plot_label <-
  function(label = NULL,
           x = NULL,
           y = NULL,
           colour = NA,
           size = 3.5,
           hjust = 0,
           geom = c("text", "label"),
           angle = 0,
           panel = NULL,
           auto_position = TRUE,
           print_position = FALSE) {

    if (is.null(x) != is.null(y)) {
      cli::cli_abort("`x` and `y` must be supplied together, or both omitted.")
    }
    if (!isTRUE(auto_position) && is.null(x)) {
      cli::cli_abort("`x` and `y` are required when `auto_position = FALSE` (there's no automatic positioning to fall back on).")
    }
    if (isTRUE(auto_position) && is.null(x) && isFALSE(getOption("theme61.auto_label", TRUE))) {
      cli::cli_abort(
        "`x`/`y` are required because automatic positioning is disabled (`theme61.auto_label = FALSE`) -- see `?set_t61_options`."
      )
    }
    if (isTRUE(auto_position) && is.null(x) && any(angle != 0)) {
      cli::cli_abort(
        "`x`/`y` are required when `angle != 0` (automatic positioning doesn't apply to rotated text -- see `?plot_label`)."
      )
    }
    # label/colour length checks are repeated in .build_plot_label_layer(),
    # since label may still be unresolved here (its length isn't known until
    # then, and colour defaulting needs `plot`, unavailable until then too).
    if (!is.null(x) && !is.null(label) && (length(label) != length(x) || length(x) != length(y))) {
      cli::cli_abort("The number of x and y positions must equal the number of labels.")
    }

    geom <- match.arg(geom)

    if (length(colour) != 1 && !is.null(label) && length(colour) != length(label)) {
      cli::cli_abort("The number of colours must equal the number of labels.")
    }

    # Automatically convert dates to dates if specified, so the user doesn't have
    # to wrap dates in as.Date() which saves some room.
    if (!is.null(x) && class(try(as.Date(as.character(x)), silent = TRUE)) != "try-error") {
      x <- as.Date(x)
    }

    # If user supplied extras, they must all be named (facet vars etc.)
    if (!is.null(panel)) {
      if (!is.list(panel) || is.null(names(panel)) || any(!nzchar(names(panel)))) {
        cli::cli_abort(
          "`panel` must be a named list.\nExample: panel = list(grp = 'A')"
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
        panel = panel,
        auto_position = auto_position,
        print_position = print_position,
        # Marks whether the default size was used, so update_plot_label()
        # (aes_labs.R) can scale it with the chart's base_size -- an
        # explicit custom size is left alone instead.
        adj_plot_label = identical(size, 3.5)
      ),
      class = "e61_plot_label"
    )
  }

# Internal helpers ----

.plab_len_chk <- function(vec, len) {
  if (length(vec) == len) return(vec)
  var_name <- deparse(substitute(vec))
  if (length(vec) != 1) cli::cli_abort("{var_name} must be length {len} or 1.")
  rep(vec, len)
}

#' Detect a user-supplied scale_colour_manual()/scale_fill_manual() on
#' `plot` (or a theme61 wrapper built on one, e.g. scale_colour_e61()) and
#' return its levels and their assigned colours, in the plot's own
#' resolved order -- i.e. exactly the (level, colour) pairing a legend
#' would show, correctly reflecting `reverse = TRUE`, a `values` vector
#' with more entries than actually appear in the data, etc.
#'
#' colour is checked before fill (matches which aesthetic
#' t61_match_label_series() treats as primary for most geoms -- see
#' autolabel-apply.R). Returns NULL if neither aesthetic has an explicit
#' scale: an algorithmic discrete scale (scale_colour_brewer(),
#' scale_colour_hue(), ...) doesn't count, since there's no fixed "colours
#' the user chose" to detect -- only a palette function to sample from.
#'
#' A scale's breaks/mapping aren't trained against the data until the plot
#' is actually built, so this needs a real ggplot2::ggplot_build() to
#' resolve correctly -- gated behind a cheap, build-free check first (the
#' scale's own constructor call), so that cost is only paid when a manual
#' scale is actually present.
#' @return list(breaks = <chr>, colours = <chr>), same length and order, or
#'   NULL if no manual colour/fill scale was found.
#' @noRd
.detect_manual_scale <- function(plot) {
  for (aes_name in c("colour", "fill")) {
    sc <- plot@scales$get_scales(aes_name)
    if (is.null(sc) || is.null(sc$call)) next

    is_manual <- identical(rlang::call_name(sc$call), paste0("scale_", aes_name, "_manual"))
    if (!is_manual) next

    built <- tryCatch(ggplot2::ggplot_build(plot), error = function(e) NULL)
    if (is.null(built)) next

    trained <- built$plot@scales$get_scales(aes_name)
    breaks <- tryCatch(trained$get_breaks(), error = function(e) NULL)
    breaks <- breaks[!is.na(breaks)]
    if (length(breaks) == 0) next

    colours <- tryCatch(trained$map(breaks), error = function(e) NULL)
    if (is.null(colours) || length(colours) != length(breaks)) next

    return(list(breaks = as.character(breaks), colours = as.character(colours)))
  }

  NULL
}

.find_facet_proto <- function(plot, facet_name) {
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

.coerce_to_proto <- function(values, proto) {
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
.get_facet_vars <- function(plot) {
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

.build_plot_label_layer <- function(object, plot) {

  facet_vars_chk <- .get_facet_vars(plot)
  if (is.null(object$x) && length(facet_vars_chk)) {
    cli::cli_abort(
      "`x`/`y` are required when the plot is facetted (automatic positioning doesn't apply to facetted plots -- see `?plot_label`)."
    )
  }

  # label and/or colour left unresolved by plot_label() (it has no access
  # to `plot`) when either should be derived from a scale_colour_manual()/
  # scale_fill_manual() on the plot -- see ?plot_label. Detected at most
  # once, and only when actually needed (a real ggplot_build(), so not
  # free -- see .detect_manual_scale()).
  colour_is_default <- length(object$colour) == 1 && is.na(object$colour)
  scale_info <- if (is.null(object$label) || colour_is_default) {
    .detect_manual_scale(plot)
  } else {
    NULL
  }

  label <- object$label
  if (is.null(label)) {
    if (is.null(scale_info)) {
      cli::cli_abort(
        "`label` is required -- the plot has no scale_colour_manual()/scale_fill_manual() to derive default labels from. See `?plot_label`."
      )
    }
    label <- scale_info$breaks
  }
  n <- length(label)

  if (!is.null(object$x) && (n != length(object$x) || length(object$x) != length(object$y))) {
    cli::cli_abort("The number of x and y positions must equal the number of labels.")
  }

  colour <- object$colour
  if (colour_is_default) {
    if (!is.null(scale_info) && n <= length(scale_info$colours)) {
      # Assumes label is in the same order as the scale's own levels --
      # exactly true when label itself just came from scale_info$breaks
      # above; otherwise it's on the caller to match the order themselves
      # (see ?plot_label).
      colour <- scale_info$colours[seq_len(n)]
    } else {
      colour <- palette_e61(n)
    }
  } else if (length(colour) == 1) {
    colour <- rep(colour, n)
  } else if (length(colour) != n) {
    cli::cli_abort("The number of colours must equal the number of labels.")
  }

  # x/y are optional when auto_position = TRUE (see ?plot_label); NA
  # placeholders keep the layer's data well-formed until t61_apply_autolabel()
  # resolves a real position at save time. A facetted plot without x/y is
  # rejected above rather than silently rendering invisible (NA-position) text.
  plot_lab_data <- data.table::data.table(
    label  = label,
    x      = if (is.null(object$x)) rep(NA_real_, n) else object$x,
    y      = if (is.null(object$y)) rep(NA_real_, n) else object$y,
    colour = colour,
    size   = .plab_len_chk(object$size, n),
    hjust  = .plab_len_chk(object$hjust, n),
    angle  = .plab_len_chk(object$angle, n),
    auto_position = .plab_len_chk(object$auto_position, n),
    print_position = .plab_len_chk(object$print_position, n)
  )

  facet_vars <- .get_facet_vars(plot)

  # Panel targeting (explicit, avoids name collisions with base columns)
  panel <- object$panel
  panel_names <- if (is.null(panel)) character() else names(panel)

  if (!is.null(panel)) {
    if (!is.list(panel) || is.null(panel_names) || any(!nzchar(panel_names))) {
      cli::cli_abort(
        "`panel` must be a named list.\nExample: plot_label('a', 1, 1, panel = list(grp = 'A'))"
      )
    }

    # If the plot is facetted, require all facet vars to be present in panel
    if (length(facet_vars)) {
      have <- intersect(facet_vars, panel_names)

      if (length(have) > 0 && length(have) < length(facet_vars)) {
        missing <- setdiff(facet_vars, have)
        facet_vars_txt <- paste(facet_vars, collapse = ", ")
        missing_txt <- paste(missing, collapse = ", ")
        example_txt <- paste0(facet_vars, " = '...'", collapse = ", ")
        cli::cli_abort(
          "This plot is facetted by: {facet_vars_txt}\nTo place labels in a specific panel, supply *all* facet variables in `panel`.\nMissing: {missing_txt}\nExample: plot_label('a', 1, 1, panel = list({example_txt}))"
        )
      }

      # If none match, user likely supplied wrong names (or plot facet vars are nonstandard)
      if (length(have) == 0) {
        panel_names_txt <- paste(panel_names, collapse = ", ")
        facet_vars_txt <- paste(facet_vars, collapse = ", ")
        cli::cli_abort(
          "`panel` names ({panel_names_txt}) do not match the plot's facet variables ({facet_vars_txt})."
        )
      }
    }

    # Add panel columns to label data (length 1 or n)
    for (nm in panel_names) {
      v <- panel[[nm]]
      if (length(v) == 1) v <- rep(v, n)
      if (length(v) != n) {
        cli::cli_abort("`panel${nm}` must be length 1 or the number of labels ({n}).")
      }
      plot_lab_data[[nm]] <- v
    }
  }

  # Facet handling
  if (length(facet_vars)) {
    if (!is.null(panel)) {
      # Targeted labels: coerce supplied facet vars to the plot prototypes
      for (fv in facet_vars) {
        proto <- .find_facet_proto(plot, fv)
        if (is.null(proto)) {
          cli::cli_abort(
            "Facet variable `{fv}` was not found in the plot data.\nCheck that you used the correct facetting variable name."
          )
        }
        if (fv %in% names(plot_lab_data)) {
          plot_lab_data[[fv]] <- .coerce_to_proto(plot_lab_data[[fv]], proto)
        }
      }
    } else {
      # Untargeted labels: expand across panels so per-row aesthetics replicate safely
      built <- ggplot2::ggplot_build(plot)
      lay <- built$layout$layout

      facet_vars2 <- facet_vars[facet_vars %in% names(lay)]
      if (length(facet_vars2)) {
        panels <- unique(lay[, facet_vars2, drop = FALSE])

        idx <- rep(seq_len(nrow(panels)), each = n)

        plot_lab_data <- plot_lab_data[rep(seq_len(.N), times = nrow(panels))]

        for (fv in facet_vars2) {
          plot_lab_data[[fv]] <- panels[[fv]][idx]
        }
      }
    }
  }

  # Create the geom
  geom_fn <- if (object$geom == "text") ggplot2::geom_text else ggplot2::geom_label
  retval <- geom_fn(
    data = plot_lab_data,
    mapping = ggplot2::aes(x, y, label = label),
    colour = plot_lab_data$colour,
    size   = plot_lab_data$size,
    hjust  = plot_lab_data$hjust,
    angle  = plot_lab_data$angle,
    inherit.aes = FALSE
  )

  if (isTRUE(object$adj_plot_label)) {
    attr(retval, "adj_plot_label") <- TRUE
  }

  retval
}

# New methods ----

# ggplot2 v4+ hook
#' @export
update_ggplot.e61_plot_label <- function(object, plot, ...) {
  plot + .build_plot_label_layer(object, plot)
}

# Back-compat hook (still used by ggplot2 add_ggplot path)
#' @export
ggplot_add.e61_plot_label <- function(object, plot, object_name, ...) {
  update_ggplot.e61_plot_label(object, plot, ...)
}

#' @rdname plot_label
#' @export
plab <- plot_label
