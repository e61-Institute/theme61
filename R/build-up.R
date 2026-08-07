# Helper functions for save_e61(build_up = TRUE), which saves a sequence of
# files that progressively reveal one category/series more than the last, for
# stepping a chart across several PowerPoint slides.
#
# The shared idea across every chart type: never remove rows from the data,
# only blank the value(s) of rows that aren't revealed yet (to zero, or to NA
# - see classify_layer_kind()). That way the axes, scales and dimensions
# computed once from the complete data stay identical across every step.

#' Does this layer fall back to the plot's default data, rather than having
#' its own?
#' @noRd
layer_uses_plot_data <- function(layer) {
  is.null(layer$data) || inherits(layer$data, "waiver")
}

#' Get the data frame that actually feeds a layer
#' @noRd
get_layer_data <- function(plot, layer) {
  if (layer_uses_plot_data(layer)) plot@data else layer$data
}

#' Get the name of the column mapped to a given aesthetic on a layer, falling
#' back to the plot's default mapping (mirrors how ggplot2 resolves aesthetics)
#' @noRd
get_mapped_var <- function(plot, layer, aes_name) {
  val <- layer$mapping[[aes_name]]
  if (is.null(val)) val <- plot@mapping[[aes_name]]
  if (is.null(val)) return(NULL)

  var <- deparse(val)
  gsub("~", "", var)
}

#' First of fill/colour/group that is mapped on a layer
#' @noRd
get_group_var <- function(plot, layer) {
  for (aes_name in c("fill", "colour", "group")) {
    var <- get_mapped_var(plot, layer, aes_name)
    if (!is.null(var)) return(list(aes = aes_name, var = var))
  }
  NULL
}

#' Whether a layer's geom should be hidden by zeroing its value (bar/area-like
#' geoms, where a reserved slot / stack contribution needs to stay at 0) or by
#' blanking it to NA (point/line-like geoms, where a drawn 0 would be a fake
#' data point)
#' @noRd
classify_layer_kind <- function(layer) {
  zero_geoms <- c("GeomBar", "GeomCol", "GeomArea", "GeomRibbon")

  if (any(class(layer$geom) %in% zero_geoms)) "zero" else "na"
}

#' Get the on-plot order of the categories for a given aesthetic. Uses the
#' trained scale where possible (respects factor levels, fct_reorder(),
#' explicit scale limits, etc.) so the reveal order always matches what's
#' actually drawn; falls back to the raw data if there's no trained scale.
#' @noRd
get_categories <- function(plot, aes_name, var, data) {
  if (aes_name %in% c("x", "fill", "colour", "color")) {
    scale <- tryCatch(layer_scales(plot)[[aes_name]], error = function(e) NULL)

    if (!is.null(scale) && is.function(scale$get_limits)) {
      lims <- tryCatch(scale$get_limits(), error = function(e) NULL)

      if (!is.null(lims) && length(lims) > 0 && !anyNA(lims)) return(as.character(lims))
    }
  }

  vals <- data[[var]]

  if (is.factor(vals)) return(levels(vals))

  as.character(sort(unique(vals)))
}

#' Work out whether `cats` (in scale order) already runs bottom-to-top in a
#' stacked layer's drawing, or needs reversing. Determined empirically off a
#' single ggplot_build() rather than assumed, since position_stack(reverse=)
#' and ggplot2 defaults can flip which end is on the bottom.
#' @noRd
resolve_stack_order <- function(plot, layer_index, cats) {
  built <- tryCatch(ggplot_build(plot)$data[[layer_index]], error = function(e) NULL)

  if (is.null(built) || is.null(built$ymin) || nrow(built) < 2) return(cats)

  # geom_area() pads the built data with an extra zero-valued row just
  # outside the data range on each side (for smooth rendering of the area
  # edge) - every group is 0 there, which is uninformative for telling top
  # from bottom. Pick the x-value with the largest total stacked height
  # instead, to make sure we land on a real data row.
  ux <- unique(built$x)
  totals <- vapply(ux, function(xx) sum(built$ymax[built$x == xx], na.rm = TRUE), numeric(1))
  ref_x <- ux[which.max(totals)]

  ref <- built[built$x == ref_x, ]
  ref <- ref[order(ref$group), ]

  if (nrow(ref) > 1 && ref$ymin[1] > ref$ymin[nrow(ref)]) rev(cats) else cats
}

#' Every layer that is fed by the same data as the reference layer - these all
#' get blanked together using the same reveal sequence (e.g. geom_pointbar()'s
#' point + error bar layers, or a bar chart plus a plot_label() data layer)
#' @noRd
find_shared_data_layers <- function(plot, ref_layer) {
  ref_uses_plot_data <- layer_uses_plot_data(ref_layer)
  ref_data <- get_layer_data(plot, ref_layer)

  unname(which(vapply(plot@layers, function(l) {
    if (layer_uses_plot_data(l) != ref_uses_plot_data) return(FALSE)
    if (ref_uses_plot_data) return(TRUE)

    identical(get_layer_data(plot, l), ref_data)
  }, logical(1))))
}

#' Blank the value aesthetics (y/ymin/ymax/yend) of the hidden rows (`hide`,
#' a logical mask into `d`) of a layer's data.table `d`, using that layer's
#' own blanking rule (zero or NA)
#' @noRd
blank_layer_data <- function(plot, layer, d, hide) {
  value_vars <- unique(unlist(lapply(c("y", "ymin", "ymax", "yend"),
                                     function(a) get_mapped_var(plot, layer, a))))
  value_vars <- intersect(value_vars, names(d))

  blank_val <- if (classify_layer_kind(layer) == "zero") 0 else NA_real_

  for (v in value_vars) d[hide, (v) := blank_val]

  d
}

#' build_up for a single, ungrouped series over a continuous x - there's no
#' discrete category to step through, so instead cut the x-axis into `n`
#' cumulative steps.
#' @noRd
build_up_continuous <- function(plot, ref_idx, n) {
  ref_layer <- plot@layers[[ref_idx]]
  ref_data <- get_layer_data(plot, ref_layer)

  x_var <- get_mapped_var(plot, ref_layer, "x")

  if (is.null(x_var))
    stop("build_up could not identify the x variable mapped to this layer.")

  x_vals <- sort(unique(ref_data[[x_var]]))

  if (length(x_vals) < 2)
    stop("build_up needs at least 2 distinct categories or x-values to build up - this chart doesn't have enough data.")

  if (is.null(n)) n <- min(length(x_vals), 10)

  if (!is.numeric(n) || length(n) != 1 || n < 2)
    stop("build_up_n must be a single number of at least 2.")

  n <- min(n, length(x_vals))

  cut_idx <- unique(pmax(1, round(seq_len(n) / n * length(x_vals))))
  cutoffs <- x_vals[cut_idx]

  targets <- find_shared_data_layers(plot, ref_layer)

  steps <- lapply(cutoffs, function(cutoff) {
    lapply(targets, function(i) {
      layer_i <- plot@layers[[i]]
      layer_x_var <- get_mapped_var(plot, layer_i, "x")

      d <- data.table::as.data.table(get_layer_data(plot, layer_i))

      if (is.null(layer_x_var) || !(layer_x_var %in% names(d))) return(d)

      hide <- d[[layer_x_var]] > cutoff

      blank_layer_data(plot, layer_i, d, hide)
    })
  })

  list(steps = steps, targets = targets)
}

#' Work out the build_up sequence for a plot: which layers to touch, and the
#' (already-blanked) data.table to substitute in at each step.
#' @noRd
resolve_build_up <- function(plot, build_up_n = NULL) {

  recognised_geoms <- c("GeomBar", "GeomCol", "GeomArea", "GeomRibbon",
                        "GeomPoint", "GeomLine", "GeomPath",
                        "GeomErrorbar", "GeomPointrange", "GeomLinerange")

  ref_idx <- unname(which(vapply(plot@layers, function(l) any(class(l$geom) %in% recognised_geoms), logical(1))))

  if (length(ref_idx) == 0)
    stop("build_up is not supported for this chart type. Supported geoms: geom_col()/geom_bar(), geom_area()/geom_ribbon(), or geom_point()/geom_line()/geom_path()/geom_pointbar().")

  ref_idx <- ref_idx[1]
  ref_layer <- plot@layers[[ref_idx]]

  # Step through the x-axis categories if x is discrete (the bar chart case in
  # #253), otherwise through a discrete fill/colour/group if there is one (the
  # stacked area / multi-line case), otherwise fall back to a continuous,
  # cumulative build along x.
  x_scale <- tryCatch(layer_scales(plot)$x, error = function(e) NULL)
  x_is_discrete <- !is.null(x_scale) && inherits(x_scale, "ScaleDiscrete")

  step_aes <- NULL
  step_var <- NULL

  if (x_is_discrete) {
    step_aes <- "x"
    step_var <- get_mapped_var(plot, ref_layer, "x")

  } else {
    grp <- get_group_var(plot, ref_layer)

    if (!is.null(grp)) {
      step_aes <- grp$aes
      step_var <- grp$var
    }
  }

  if (is.null(step_aes)) return(build_up_continuous(plot, ref_idx, build_up_n))

  ref_data <- get_layer_data(plot, ref_layer)
  cats <- get_categories(plot, step_aes, step_var, ref_data)

  # Not really a categorical build (e.g. a single ungrouped line) - fall back
  # to the continuous, cumulative build instead
  if (length(cats) < 2) return(build_up_continuous(plot, ref_idx, build_up_n))

  # For a stacked reference layer, reveal bottom-to-top rather than in
  # whatever order the scale happens to use
  if (step_aes != "x" && classify_layer_kind(ref_layer) == "zero") {
    cats <- resolve_stack_order(plot, ref_idx, cats)
  }

  targets <- find_shared_data_layers(plot, ref_layer)

  steps <- lapply(seq_along(cats), function(k) {
    revealed <- cats[seq_len(k)]

    lapply(targets, function(i) {
      layer_i <- plot@layers[[i]]

      layer_step_var <- get_mapped_var(plot, layer_i, step_aes)

      d <- data.table::as.data.table(get_layer_data(plot, layer_i))

      if (is.null(layer_step_var) || !(layer_step_var %in% names(d))) return(d)

      hide <- !(as.character(d[[layer_step_var]]) %in% revealed)

      blank_layer_data(plot, layer_i, d, hide)
    })
  })

  list(steps = steps, targets = targets)
}
