# Wires the autolabel engine (mask/collision/distance/los/selection/
# orchestrator, see autolabel.R) into plot_label()/save_e61().
#
# save_single() calls t61_apply_autolabel() once the chart's final
# width/height are known; it finds plot_label() layers eligible for
# auto-positioning, matches each to a data series by colour (see
# t61_match_label_series()), and asks the engine for a better spot.
# Anything not v1 scope (facetted plots, auto_position = FALSE, rotated
# text) with an explicit x/y keeps that position rather than erroring.
# Without one, plot_label() itself refuses to construct a facetted or
# rotated label in the first place (there's nothing safe to fall back
# on), so that combination never reaches here at all. A colour that
# matches no series still gets a position via the engine's own fallback
# tiers (see t61_autolabel_plot()).

#' Find a "point", "line", "column", "area" or "pointbar" data layer in a
#' plot whose resolved colour matches a label's colour -- this is treated
#' as the series the label belongs to.
#'
#' Line/point/pointbar series are matched on their `colour` aesthetic;
#' column/bar and area series are matched on `fill` instead, since those
#' charts almost always map the series to `fill`, not `colour` (there's
#' usually no visible stroke colour to match against).
#'
#' `geom_pointbar()` (this package's point + error-bar wrapper) draws two
#' separate layers with the same colour -- a GeomErrorbar layer, then a
#' GeomPoint layer on top. Matching GeomErrorbar as its own "pointbar" type
#' means it wins the match first (it's earlier in layer/draw order), so the
#' error bars' vertical extent is what the buffer is measured against, not
#' just the point -- the GeomPoint half is never reached for that label.
#'
#' v1 scope: only GeomLine/GeomPoint/GeomCol/GeomBar/GeomArea/GeomRibbon/
#' GeomErrorbar layers are matched.
#'
#' @param layers plot@layers.
#' @param built_data ggplot2::ggplot_build(plot)$data (same length/order as
#'   layers).
#' @param colour The label's colour (anything grDevices::col2rgb() accepts).
#' @return For "point"/"line": list(x=, y=, geom_type=). For "column":
#'   list(xmin=, xmax=, ymin=, ymax=, geom_type=) (one entry per bar,
#'   already position-adjusted, e.g. dodged). For "area": list(x=, ymin=,
#'   ymax=, fill=, geom_type=) (fill is the matched colour itself, carried
#'   through for t61_place_label_area()'s outside-placement fallback and
#'   contrast-colour decision). For "pointbar": list(x=, y=, ymin=, ymax=,
#'   geom_type=). NULL if nothing matches.
#' @noRd
t61_match_label_series <- function(layers, built_data, colour) {
  target_rgb <- tryCatch(grDevices::col2rgb(colour), error = function(e) NULL)
  if (is.null(target_rgb)) return(NULL)

  for (i in seq_along(layers)) {
    geom_class <- class(layers[[i]]$geom)

    geom_type <- if ("GeomLine" %in% geom_class) {
      "line"
    } else if ("GeomPoint" %in% geom_class) {
      "point"
    } else if ("GeomCol" %in% geom_class || "GeomBar" %in% geom_class) {
      "column"
    } else if ("GeomArea" %in% geom_class || "GeomRibbon" %in% geom_class) {
      "area"
    } else if ("GeomErrorbar" %in% geom_class) {
      "pointbar"
    } else {
      NA_character_
    }

    if (is.na(geom_type)) next

    d <- built_data[[i]]
    if (is.null(d)) next

    if (identical(geom_type, "column")) {
      if (is.null(d$fill) || is.null(d$xmin) || is.null(d$xmax) ||
          is.null(d$ymin) || is.null(d$ymax)) next
      match_colour <- d$fill
    } else if (identical(geom_type, "area")) {
      if (is.null(d$fill) || is.null(d$x) || is.null(d$ymin) || is.null(d$ymax)) next
      match_colour <- d$fill
      alpha <- if (is.null(d$alpha)) rep(1, nrow(d)) else ifelse(is.na(d$alpha), 1, d$alpha)
    } else if (identical(geom_type, "pointbar")) {
      if (is.null(d$colour) || is.null(d$x) || is.null(d$ymin) || is.null(d$ymax)) next
      match_colour <- d$colour
    } else {
      if (is.null(d$colour) || is.null(d$x) || is.null(d$y)) next
      match_colour <- d$colour
    }

    d_rgb <- tryCatch(grDevices::col2rgb(match_colour), error = function(e) NULL)
    if (is.null(d_rgb)) next

    is_match <- colSums(abs(d_rgb - as.vector(target_rgb))) == 0
    if (!any(is_match)) next

    if (identical(geom_type, "column")) {
      ord <- order(d$xmin[is_match])
      return(list(
        xmin = d$xmin[is_match][ord], xmax = d$xmax[is_match][ord],
        ymin = d$ymin[is_match][ord], ymax = d$ymax[is_match][ord],
        geom_type = geom_type
      ))
    }

    if (identical(geom_type, "area")) {
      ord <- order(d$x[is_match])
      return(list(
        x = d$x[is_match][ord], ymin = d$ymin[is_match][ord], ymax = d$ymax[is_match][ord],
        fill = match_colour[is_match][1], alpha = alpha[is_match][1], geom_type = geom_type
      ))
    }

    if (identical(geom_type, "pointbar")) {
      ord <- order(d$x[is_match])
      y_match <- if (is.null(d$y)) (d$ymin[is_match] + d$ymax[is_match]) / 2 else d$y[is_match]
      return(list(
        x = d$x[is_match][ord], y = y_match[ord],
        ymin = d$ymin[is_match][ord], ymax = d$ymax[is_match][ord],
        geom_type = geom_type
      ))
    }

    ord <- order(d$x[is_match])
    return(list(x = d$x[is_match][ord], y = d$y[is_match][ord], geom_type = geom_type))
  }

  NULL
}

#' Gather every plot_label() row eligible for auto-positioning across all
#' of a plot's layers, and build the `labels` data frame t61_autolabel_plot()
#' expects, plus parallel layer/row indices to write resolved positions
#' back to afterwards.
#' @noRd
t61_collect_autolabel_targets <- function(plot) {

  label_layers <- which(vapply(plot@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$auto_position)
  }, logical(1)))

  empty <- list(labels = data.frame(), layer_idx = integer(0), row_idx = integer(0))
  if (length(label_layers) == 0) return(empty)

  built_data <- ggplot2::ggplot_build(plot)$data

  layer_idx <- integer(0); row_idx <- integer(0)
  text <- character(0); geom_type <- character(0)
  hjust <- numeric(0); size_mm <- numeric(0)
  fallback_x <- numeric(0); fallback_y <- numeric(0)
  is_date_x <- logical(0); is_date_y <- logical(0)
  series <- list()

  for (i in label_layers) {
    ly <- plot@layers[[i]]
    d  <- ly$data
    n  <- nrow(d)

    # colour/hjust/size/angle are passed to geom_text()/geom_label() as
    # literal (non-aes) args in .build_plot_label_layer(), so ggplot2
    # stores them in aes_params (as per-row vectors), not data -- data's
    # copies of these columns are inert. x/y/label ARE aes-mapped, so data
    # is the source of truth for those.
    colours <- if (is.null(ly$aes_params$colour)) d$colour else ly$aes_params$colour
    hjusts  <- if (is.null(ly$aes_params$hjust))  d$hjust  else ly$aes_params$hjust
    sizes   <- if (is.null(ly$aes_params$size))   d$size   else ly$aes_params$size
    angles  <- if (is.null(ly$aes_params$angle))  d$angle  else ly$aes_params$angle

    for (r in seq_len(n)) {
      if (!isTRUE(d$auto_position[r])) next
      # Rotated text is out of v1 scope entirely; renders exactly where
      # given, same as auto_position = FALSE. plot_label() itself refuses
      # to construct a rotated label with no x/y (there's nothing safe to
      # fall back on), so reaching here always means an explicit position.
      if (!isTRUE(all.equal(angles[r], 0))) next

      match <- t61_match_label_series(plot@layers, built_data, colours[r])
      # Not skipped even when no series matches: still eligible for the
      # fallback tiers in t61_autolabel_plot() (the caller's own x/y if
      # supplied, else any empty space, else the panel centre -- see
      # ?plot_label) rather than leaving the label invisible.
      # series = list() is the "no series matched" sentinel.

      layer_idx <- c(layer_idx, i)
      row_idx   <- c(row_idx, r)
      text      <- c(text, d$label[r])
      geom_type <- c(geom_type, if (is.null(match)) NA_character_ else match$geom_type)
      hjust     <- c(hjust, hjusts[r])
      size_mm   <- c(size_mm, sizes[r])
      fallback_x <- c(fallback_x, d$x[r])
      fallback_y <- c(fallback_y, d$y[r])

      if (is.null(match)) {
        is_date_x <- c(is_date_x, inherits(d$x[r], "Date"))
        is_date_y <- c(is_date_y, inherits(d$y[r], "Date"))
      } else {
        # Derived from the matched series' own data, not the label's
        # fallback x/y -- the fallback may just be NA now that x/y are
        # optional, which would otherwise always read as "not a date".
        match_x <- if (identical(match$geom_type, "column")) match$xmin else match$x
        match_y <- if (match$geom_type %in% c("column", "area")) match$ymin else match$y
        is_date_x <- c(is_date_x, inherits(match_x, "Date"))
        is_date_y <- c(is_date_y, inherits(match_y, "Date"))
      }

      series[[length(series) + 1]] <- if (is.null(match)) {
        list()
      } else if (identical(match$geom_type, "column")) {
        list(xmin = match$xmin, xmax = match$xmax, ymin = match$ymin, ymax = match$ymax)
      } else if (identical(match$geom_type, "area")) {
        list(x = match$x, ymin = match$ymin, ymax = match$ymax, fill = match$fill, alpha = match$alpha)
      } else if (identical(match$geom_type, "pointbar")) {
        list(x = match$x, y = match$y, ymin = match$ymin, ymax = match$ymax)
      } else {
        list(x = match$x, y = match$y)
      }
    }
  }

  if (length(text) == 0) return(empty)

  labels <- data.frame(
    text = text, geom_type = geom_type, hjust = hjust, size_mm = size_mm,
    fallback_x = fallback_x, fallback_y = fallback_y,
    stringsAsFactors = FALSE
  )
  labels$series <- series

  list(labels = labels, layer_idx = layer_idx, row_idx = row_idx,
       is_date_x = is_date_x, is_date_y = is_date_y)
}

#' Cheap check: does this plot have any plot_label() layer with
#' print_position = TRUE? Used to gate print.e61_ggplot()'s console-print
#' path so printing a plot doesn't pay for a full save_single() resolve
#' unless the user actually asked for the positions.
#' @noRd
t61_has_print_position_labels <- function(plot) {
  any(vapply(plot@layers, function(ly) {
    !is.null(ly$data) && !is.null(ly$data$print_position) &&
      isTRUE(any(ly$data$print_position, na.rm = TRUE))
  }, logical(1)))
}

#' Drop plot_label() layers from a plot before rendering the occupancy
#' mask, so a label's stale fallback position doesn't get treated as ink
#' that blocks a better spot for itself or another label. Layers not
#' created by plot_label() (e.g. a user's own geom_text()) are untouched,
#' and still count as obstacles.
#' @noRd
t61_strip_autolabel_layers <- function(plot) {
  keep <- vapply(plot@layers, function(ly) {
    is.null(ly$data) || is.null(ly$data$auto_position)
  }, logical(1))

  plot@layers <- plot@layers[keep]
  plot
}

#' Format a vector as a copy-pasteable `c(...)` literal for
#' t61_print_label_positions(). Dates and strings get quoted; plot_label()
#' already auto-converts a quoted "YYYY-MM-DD" x/y back to Date, so no
#' as.Date() wrapper is needed. Numbers are rounded to 4 significant
#' figures -- these are candidate-grid pixel positions, not meaningful
#' data values, so full float precision would just be noise.
#' @noRd
t61_format_label_vec <- function(x) {
  if (inherits(x, "Date")) {
    return(paste0("c(", paste0('"', format(x, "%Y-%m-%d"), '"', collapse = ", "), ")"))
  }
  if (is.character(x)) {
    return(paste0("c(", paste0('"', x, '"', collapse = ", "), ")"))
  }
  paste0("c(", paste0(signif(x, 4), collapse = ", "), ")")
}

#' Print the final label/x/y positions as copy-pasteable plot_label()
#' arguments, so a user who wants to pin the auto-placed positions (or
#' hand-tweak just one or two) has an exact starting point instead of
#' reading coordinates off the rendered chart.
#'
#' Emitted as a message (not printed/returned), so it's a side effect of
#' rendering the plot rather than part of any return value -- the caller
#' (t61_apply_autolabel(), and in turn save_single()/print.e61_ggplot())
#' still returns the plot itself either way.
#' @noRd
t61_print_label_positions <- function(text, x, y, is_date_x, is_date_y) {
  x_out <- if (any(is_date_x)) as.Date(x, origin = "1970-01-01") else x
  y_out <- if (any(is_date_y)) as.Date(y, origin = "1970-01-01") else y

  code <- paste0(
    t61_format_label_vec(text), ", ",
    t61_format_label_vec(x_out), ", ",
    t61_format_label_vec(y_out)
  )

  cli::cli_alert_info("Label positions (paste into {.fn plot_label} to pin them):")
  message(code)
}

#' Automatically reposition eligible plot_label() text away from its
#' fallback position, using the autolabel engine. Called from
#' save_single() once the chart's final width/height (cm) are known.
#'
#' Fails safe: any error anywhere in matching/placement leaves `plot`
#' unmodified (labels keep their user-supplied x/y) rather than breaking
#' save_e61() for existing users.
#'
#' @param print_positions Logical. If TRUE, print the final label/x/y as
#'   copy-pasteable plot_label() arguments (see t61_print_label_positions()).
#' @param fast Logical. Skip the mask render and scored search, using a
#'   cheap render-free placement instead (see t61_place_label_fast()) for
#'   any label without an explicit x/y. Used for the automatic Viewer
#'   preview on print(), where iteration speed matters more than an
#'   optimal spot; save_e61() itself always resolves the real position.
#' @noRd
t61_apply_autolabel <- function(plot, width_cm, height_cm, print_positions = FALSE, fast = FALSE) {

  # theme61.auto_label = FALSE restores the pre-feature behaviour (x/y
  # always required -- enforced in plot_label() itself) and must incur no
  # performance cost at all, so this is checked first and unconditionally,
  # before targets are even collected (which alone requires a full
  # ggplot_build()).
  if (isFALSE(getOption("theme61.auto_label", TRUE))) return(plot)

  targets <- tryCatch(t61_collect_autolabel_targets(plot), error = function(e) NULL)
  if (is.null(targets) || nrow(targets$labels) == 0) return(plot)

  # fast = TRUE means t61_place_label_fast() (no collision search) ran
  # instead of the real search -- only worth flagging when a label actually
  # needed that heuristic (no explicit x/y to fall back on).
  if (isTRUE(fast) && getOption("theme61.autolabel_fast_msg", default = TRUE) &&
      any(is.na(targets$labels$fallback_x) | is.na(targets$labels$fallback_y))) {
    cli::cli_alert_info(
      "Auto-positioned {.fn plot_label} text in this preview uses a quick placement heuristic, not the real collision-avoiding search -- labels may overlap here even when {.fn save_e61} would place them cleanly. Save the graph with {.fn save_e61} to see (and use) the actual auto-positioned labels. This message appears once per session; to see it again, run {.code options(theme61.autolabel_fast_msg = TRUE)}.",
      wrap = TRUE
    )
    options(theme61.autolabel_fast_msg = FALSE)
  }

  plot_for_mask <- t61_strip_autolabel_layers(plot)

  result <- tryCatch(
    t61_autolabel_plot(plot_for_mask, targets$labels, width_cm = width_cm, height_cm = height_cm, fast = fast),
    error = function(e) NULL
  )
  if (is.null(result)) return(plot)

  # A ggplot2 Layer is a ggproto object, i.e. an environment -- writing
  # `plot@layers[[i]]$data <- ...` mutates that shared environment in
  # place, so it reaches every other reference to the SAME layer too (e.g.
  # a plot printed once, then saved separately still points at the same
  # layer object), breaking the "leaves `plot` unmodified" contract above.
  # Its `data` is additionally a data.table, which mutates its columns by
  # reference on top of that. Cloning the layer (via ggproto(NULL, ...),
  # ggplot2's usual way of getting an independent copy) and copying its
  # data.table, once per layer right before its first write, undoes both.
  copied_layers <- integer(0)

  for (k in seq_len(nrow(result))) {
    if (!isTRUE(result$placed[k])) next # fallback already in place: nothing to update

    i <- targets$layer_idx[k]
    r <- targets$row_idx[k]

    if (!(i %in% copied_layers)) {
      old_layer <- plot@layers[[i]]
      plot@layers[[i]] <- ggplot2::ggproto(NULL, old_layer, data = data.table::copy(old_layer$data))
      copied_layers <- c(copied_layers, i)
    }

    plot@layers[[i]]$data$x[r] <- result$x[k]
    plot@layers[[i]]$data$y[r] <- result$y[k]

    # Only set for "area" labels placed inside their band (see
    # t61_place_label_area()): the contrast colour, overriding the fill
    # colour the label was matched on. colour/hjust/size/angle are stored
    # as per-row aes_params vectors, not data -- see
    # t61_collect_autolabel_targets()'s note on why.
    if (!is.na(result$colour[k])) {
      plot@layers[[i]]$aes_params$colour[r] <- result$colour[k]
    }
  }

  if (isTRUE(print_positions)) {
    t61_print_label_positions(result$text, result$x, result$y, targets$is_date_x, targets$is_date_y)
  }

  plot
}
