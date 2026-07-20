#' Method for theme61 plots to add default scales at build time
#' @export
ggplot_build.e61_plot <- function(plot, ...) {

  # Classify + realise defensively, in case ggplot_build() is reached without
  # going through print.e61_plot() or save_e61() first (e.g. a direct
  # ggplot_build()/ggplotGrob() call). Both steps are no-ops if already done.
  plot2 <- finalise_e61_plot(plot)
  plot2 <- maybe_add_default_scales(plot2)
  plot2 <- maybe_adjust_facet_spacing(plot2)
  plot2 <- maybe_leftalign_discrete_y_text(plot2)

  # prevent recursion: drop our class before calling ggplot2 build
  class(plot2) <- setdiff(class(plot2), c("e61_map", "e61_plot"))

  ggplot2::ggplot_build(plot2, ...)
}

# Helpers ----

# Find the first mapping for aes_name from plot mapping, else from layers.
# Returns list(quo = <quosure>, data = <data.frame>) or NULL.
find_aes <- function(plot, aes_name) {

  if (!is.null(plot@data)) {
    plot_data <- plot@data
  } else {
    stop("Plot has no data.")
  }

  # plot-level mapping first
  if (!is.null(plot@mapping[[aes_name]])) {
    return(list(quo = plot@mapping[[aes_name]], data = plot_data))
  }

  # layer-level mapping next
  for (ly in plot@layers) {
    if (!is.null(ly$mapping[[aes_name]])) {
      d <- ly$data

      if (is.null(d) || inherits(d, "waiver")) d <- plot_data

      return(list(quo = ly$mapping[[aes_name]], data = d))
    }
  }

  NULL
}

# Safe version of eval_tidy that returns NULL rather than an error if it fails
safe_eval_tidy <- function(quo, data) {
  tryCatch(
    rlang::eval_tidy(quo, data = rlang::as_data_mask(data)),
    error = function(e) NULL
  )
}

#' @noRd
infer_aes_type <- function(plot, aes_name) {
  # Returns "continuous", "discrete", or NA (unknown/unsafe)

  # Collect mapping quosures for this aesthetic from plot + layers
  quos <- list()
  if (!is.null(plot@mapping[[aes_name]])) {
    quos <- c(quos, list(plot@mapping[[aes_name]]))
  }
  for (ly in plot@layers) {
    if (!is.null(ly$mapping[[aes_name]])) {
      quos <- c(quos, list(ly$mapping[[aes_name]]))
    }
  }

  if (length(quos) == 0) return(NA_character_)

  # Skip computed aesthetics
  is_unsafe_mapping <- function(q) {
    expr <- rlang::quo_get_expr(q)
    txt <- paste(deparse(expr), collapse = "")
    grepl("after_stat\\(|after_scale\\(|stat\\(", txt)
  }

  # Candidate datasets (layer overrides plot); ignore waiver
  data_candidates <- list(plot@data)
  for (ly in plot@layers) data_candidates <- c(data_candidates, list(ly$data))
  data_candidates <- Filter(function(d) !is.null(d) && !inherits(d, "waiver"), data_candidates)
  if (length(data_candidates) == 0) return(NA_character_)

  types <- character(0)

  for (q in quos) {
    if (is_unsafe_mapping(q)) next

    expr <- rlang::quo_get_expr(q)

    # If mapping is a bare symbol, pull directly from data to avoid name collisions
    if (rlang::is_symbol(expr)) {
      nm <- rlang::as_string(expr)
      val <- NULL
      for (d in data_candidates) {
        if (!is.null(names(d)) && nm %in% names(d)) {
          val <- d[[nm]]
          break
        }
      }
      if (is.null(val) || is.function(val)) next

    } else {
      # Otherwise evaluate safely in a data mask
      val <- NULL
      for (d in data_candidates) {
        val <- safe_eval_tidy(q, data = d)
        if (!is.null(val)) break
      }
      if (is.null(val) || is.function(val)) next
    }

    if (is.numeric(val) || inherits(val, c("Date", "POSIXct", "POSIXt", "difftime"))) {
      types <- c(types, "continuous")
    } else {
      types <- c(types, "discrete")
    }
  }

  if (length(types) == 0) return(NA_character_)
  if (any(types == "continuous")) "continuous" else "discrete"
}

#' @noRd
infer_discrete_nlevels <- function(plot, aes_name) {

  # Find mapping quosure
  q <- plot@mapping[[aes_name]]
  if (is.null(q)) {
    for (ly in plot@layers) {
      q <- ly$mapping[[aes_name]]
      if (!is.null(q)) break
    }
  }
  if (is.null(q)) return(NA_integer_)

  expr <- rlang::quo_get_expr(q)

  # Candidate data: layer data then plot data
  data_candidates <- lapply(plot@layers, `[[`, "data")
  data_candidates <- c(data_candidates, list(plot@data))
  data_candidates <- Filter(Negate(is.null), data_candidates)
  if (length(data_candidates) == 0) return(NA_integer_)

  # If mapping is a bare symbol, pull directly from data to avoid name collisions
  if (rlang::is_symbol(expr)) {
    nm <- rlang::as_string(expr)
    for (d in data_candidates) {
      if (!is.null(names(d)) && nm %in% names(d)) {
        val <- d[[nm]]
        # Discrete-ish only
        if (is.function(val) || is.numeric(val) ||
            inherits(val, c("Date", "POSIXct", "POSIXt", "difftime"))) {
          return(NA_integer_)
        }
        return(length(unique(as.character(val))))
      }
    }
    return(NA_integer_)
  }

  # Skip computed aesthetics (too hard to infer pre-build)
  expr_txt <- paste(deparse(expr), collapse = "")
  if (grepl("after_stat\\(|after_scale\\(|stat\\(", expr_txt)) return(NA_integer_)

  # Fallback: evaluate expression safely
  val <- NULL
  for (d in data_candidates) {
    val <- tryCatch(rlang::eval_tidy(q, data = d), error = function(e) NULL)
    if (!is.null(val)) break
  }
  if (is.null(val) || is.function(val)) return(NA_integer_)

  if (is.numeric(val) || inherits(val, c("Date", "POSIXct", "POSIXt", "difftime"))) {
    return(NA_integer_)
  }

  length(unique(as.character(val)))
}

#' @noRd
abort_too_many_discrete_levels <- function(aes_name, n, max_n) {
  rlang::abort(
    paste0(
      "theme61 does not support more than ", max_n,
      " discrete ", aes_name, " values automatically (", n, " requested). ",
      "Please supply your own ", aes_name, " scale (e.g. ",
      if (aes_name == "colour") "scale_colour_manual()" else "scale_fill_manual()",
      ")."
    )
  )
}

# Function to add default secondary y or x scales
maybe_add_default_scales <- function(plot) {

  # ---- Y scale ----
  if (is.null(plot@scales$get_scales("y"))) {

    info <- find_aes(plot, "y")

    if (!is.null(info) && !is.null(info$data)) {

      y_val <- safe_eval_tidy(info$quo, data = info$data)

      if (is.numeric(y_val) || is.integer(y_val)) {

        rng <- range(y_val, na.rm = TRUE)

        if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] == rng[2]) {

          plot <- plot + scale_y_continuous_e61(expand_bottom = 0.15, expand_top = 0.15)
        } else {

          plot <- plot + scale_y_continuous_e61()
        }
      }
    }
  }

  # ---- X scale ----
  if (is.null(plot@scales$get_scales("x"))) {

    info <- find_aes(plot, "x")

    if (!is.null(info) && !is.null(info$data)) {

      x_val <- safe_eval_tidy(info$quo, data = info$data)

      if (is.numeric(x_val) || is.integer(x_val)) {

        plot <- plot + scale_x_continuous_e61()
      }
    }
  }

  # ---- Colour scale ----
  if (is.null(plot@scales$get_scales("colour")) && !is.null(find_aes(plot, "colour"))) {

    typ <- infer_aes_type(plot, "colour")

    if (identical(typ, "discrete")) {
      max_n <- getOption("theme61.max_discrete_colours", 12L)

      lev_n <- infer_discrete_nlevels(plot, "colour")
      if (!is.na(lev_n) && lev_n > max_n) {
        abort_too_many_discrete_levels("colour", lev_n, max_n)
      }

      plot <- plot + scale_colour_e61()
    } else if (identical(typ, "continuous")) {
      plot <- plot + scale_colour_e61(discrete = FALSE)
    }
  }

  # ---- Fill scale ----
  if (is.null(plot@scales$get_scales("fill")) && !is.null(find_aes(plot, "fill"))) {

    typ <- infer_aes_type(plot, "fill")

    if (identical(typ, "discrete")) {
      max_n <- getOption("theme61.max_discrete_fills", 12L)

      lev_n <- infer_discrete_nlevels(plot, "fill")
      if (!is.na(lev_n) && lev_n > max_n) {
        abort_too_many_discrete_levels("fill", lev_n, max_n)
      }

      plot <- plot + scale_fill_e61()
    } else if (identical(typ, "continuous")) {
      plot <- plot + scale_fill_e61(discrete = FALSE)
    }
  }

  plot
}

# Add facet panel spacing that depends on theme61 facet settings.
#
# theme61 masks facet_wrap/facet_grid to default axes = "all". Drawing axes on
# every panel needs more breathing room, but when axes are only on margins (the
# ggplot2 default-style), large spacing just wastes space.
maybe_adjust_facet_spacing <- function(plot) {

  axes <- attr(plot@facet, "t61_axes", exact = TRUE)

  # Only adjust if the facet was created via theme61 wrappers.
  if (is.null(axes)) return(plot)

  # Respect any user-specified panel spacing.
  th <- plot@theme
  if (!is.null(th$panel.spacing) ||
      !is.null(th$panel.spacing.x) ||
      !is.null(th$panel.spacing.y)) {
    return(plot)
  }

  if (isTRUE(axes == "all")) {
    plot <- plot + theme(panel.spacing.x = unit(2, "lines"),
                         panel.spacing.y = unit(2, "lines"))
  } else {
    # ggplot2 default is 0.5 lines
    plot <- plot + theme(panel.spacing.x = unit(0.5, "lines"),
                         panel.spacing.y = unit(0.5, "lines"))
  }

  plot
}

# Left-align y-axis text when the y-axis is categorical.
maybe_leftalign_discrete_y_text <- function(plot) {

  if (!has_discrete_y_scale(plot)) return(plot)

  th <- plot@theme

  # Respect any user-specified alignment for y-axis text.
  has_hjust <- function(el) inherits(el, "element_text") && !is.null(el$hjust)

  theme_args <- list()

  if (!has_hjust(th$axis.text.y)) theme_args$axis.text.y <- element_text(hjust = 0)
  if (!has_hjust(th$axis.text.y.right)) theme_args$axis.text.y.right <- element_text(hjust = 0)

  if (length(theme_args) == 0) return(plot)

  plot + do.call(theme, theme_args)
}
