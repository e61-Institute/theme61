# Wires the autolabel engine (mask/collision/distance/los/selection/
# orchestrator, see autolabel.R) into plot_label()/save_e61() (issue #159).
#
# Design: plot_label(x, y) keeps x/y as a required "fallback" position (the
# Phase-1 placeholder from the issue discussion). save_single() calls
# t61_apply_autolabel() once the chart's final width/height are known; it
# finds plot_label() layers eligible for auto-positioning, matches each to
# a data series by colour (see t61_match_label_series()), and asks the
# engine for a better spot. Anything not v1 scope (facetted plots, no
# colour match, bar/area series, rotated text, auto_position = FALSE)
# silently keeps the fallback position rather than erroring.

#' Find a "point" or "line" data layer in a plot whose resolved colour
#' matches a label's colour -- this is treated as the series the label
#' belongs to (see issue #159 comment: labels are matched to series by
#' colour, in the order the user supplies them).
#'
#' v1 scope: only GeomLine/GeomPoint layers are matched, mirroring
#' t61_distance_to_series()'s supported geom_types.
#'
#' @param layers plot@layers.
#' @param built_data ggplot2::ggplot_build(plot)$data (same length/order as
#'   layers).
#' @param colour The label's colour (anything grDevices::col2rgb() accepts).
#' @return list(x=, y=, geom_type=) for the first matching layer (in draw
#'   order), or NULL if nothing matches.
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
    } else {
      NA_character_
    }

    if (is.na(geom_type)) next

    d <- built_data[[i]]
    if (is.null(d) || is.null(d$colour) || is.null(d$x) || is.null(d$y)) next

    d_rgb <- tryCatch(grDevices::col2rgb(d$colour), error = function(e) NULL)
    if (is.null(d_rgb)) next

    is_match <- colSums(abs(d_rgb - as.vector(target_rgb))) == 0
    if (!any(is_match)) next

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
      if (!isTRUE(all.equal(angles[r], 0))) next # v1 scope: axis-aligned text only

      match <- t61_match_label_series(plot@layers, built_data, colours[r])
      if (is.null(match)) next # no matching line/point series: keep fallback

      layer_idx <- c(layer_idx, i)
      row_idx   <- c(row_idx, r)
      text      <- c(text, d$label[r])
      geom_type <- c(geom_type, match$geom_type)
      hjust     <- c(hjust, hjusts[r])
      size_mm   <- c(size_mm, sizes[r])
      fallback_x <- c(fallback_x, d$x[r])
      fallback_y <- c(fallback_y, d$y[r])
      series[[length(series) + 1]] <- list(x = match$x, y = match$y)
    }
  }

  if (length(text) == 0) return(empty)

  labels <- data.frame(
    text = text, geom_type = geom_type, hjust = hjust, size_mm = size_mm,
    fallback_x = fallback_x, fallback_y = fallback_y,
    stringsAsFactors = FALSE
  )
  labels$series <- series

  list(labels = labels, layer_idx = layer_idx, row_idx = row_idx)
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

#' Automatically reposition eligible plot_label() text away from its
#' fallback position, using the autolabel engine. Called from
#' save_single() once the chart's final width/height (cm) are known.
#'
#' Fails safe: any error anywhere in matching/placement leaves `plot`
#' unmodified (labels keep their user-supplied x/y) rather than breaking
#' save_e61() for existing users.
#' @noRd
t61_apply_autolabel <- function(plot, width_cm, height_cm) {

  targets <- tryCatch(t61_collect_autolabel_targets(plot), error = function(e) NULL)
  if (is.null(targets) || nrow(targets$labels) == 0) return(plot)

  plot_for_mask <- t61_strip_autolabel_layers(plot)

  result <- tryCatch(
    t61_autolabel_plot(plot_for_mask, targets$labels, width_cm = width_cm, height_cm = height_cm),
    error = function(e) NULL
  )
  if (is.null(result)) return(plot)

  for (k in seq_len(nrow(result))) {
    if (!isTRUE(result$placed[k])) next # fallback already in place: nothing to update

    i <- targets$layer_idx[k]
    r <- targets$row_idx[k]
    plot@layers[[i]]$data$x[r] <- result$x[k]
    plot@layers[[i]]$data$y[r] <- result$y[k]
  }

  plot
}
