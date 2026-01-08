#' @export
ggplot_build.e61_ggplot <- function(plot, ...) {

  plot2 <- maybe_add_default_scales(plot)

  # prevent recursion: drop our class before calling ggplot2 build
  class(plot2) <- setdiff(class(plot2), "e61_ggplot")

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

  # collect mapping quosures for this aesthetic from plot + layers
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

  # helper: does a quosure reference after_stat/after_scale etc?
  is_unsafe_mapping <- function(q) {
    lab <- rlang::quo_get_expr(q)
    # don't try to infer types for computed aesthetics
    txt <- paste(deparse(lab), collapse = "")
    grepl("after_stat\\(|after_scale\\(|stat\\(", txt, fixed = FALSE)
  }

  types <- character(0)

  for (i in seq_along(quos)) {
    q <- quos[[i]]
    if (is_unsafe_mapping(q)) next

    # Determine data to evaluate against (layer data overrides plot data)
    # If layer@data is NULL, ggplot2 will use plot@data; that's fine.
    data_candidates <- list(plot@data)
    for (ly in plot@layers) {
      data_candidates <- c(data_candidates, list(ly$data))
    }
    data_candidates <- Filter(Negate(is.null), data_candidates)
    if (length(data_candidates) == 0) next

    # try evaluating in each candidate dataset until one works
    val <- NULL
    ok <- FALSE
    for (d in data_candidates) {
      val <- tryCatch(rlang::eval_tidy(q, data = d), error = function(e) NULL)
      if (!is.null(val)) { ok <- TRUE; break }
    }
    if (!ok) next

    # infer type
    if (is.numeric(val) || inherits(val, c("Date", "POSIXct", "POSIXt", "difftime"))) {
      types <- c(types, "continuous")
    } else {
      # factor/character/logical/etc
      types <- c(types, "discrete")
    }
  }

  if (length(types) == 0) return(NA_character_)

  # conservative rule: if any continuous, treat as continuous
  if (any(types == "continuous")) "continuous" else "discrete"
}

#' @noRd
infer_discrete_nlevels <- function(plot, aes_name) {

  # Find the first mapping for this aes (plot mapping first, then layers)
  q <- plot@mapping[[aes_name]]
  if (is.null(q)) {
    for (ly in plot@layers) {
      q <- ly$mapping[[aes_name]]
      if (!is.null(q)) break
    }
  }
  if (is.null(q)) return(NA_integer_)

  # Don’t attempt for computed aesthetics
  expr_txt <- paste(deparse(rlang::quo_get_expr(q)), collapse = "")
  if (grepl("after_stat\\(|after_scale\\(|stat\\(", expr_txt)) return(NA_integer_)

  # Candidate datasets: layer data then plot data
  data_candidates <- lapply(plot@layers, `[[`, "data")
  data_candidates <- c(data_candidates, list(plot@data))
  data_candidates <- Filter(Negate(is.null), data_candidates)
  if (length(data_candidates) == 0) return(NA_integer_)

  val <- NULL
  for (d in data_candidates) {
    val <- tryCatch(rlang::eval_tidy(q, data = d), error = function(e) NULL)
    if (!is.null(val)) break
  }
  if (is.null(val)) return(NA_integer_)

  # If it’s not discrete-ish, don’t report levels
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

    typ <- infer_aes_type(plot, "colour")  # or whatever you called it

    if (identical(typ, "discrete")) {
      max_n <- getOption("theme61.max_discrete_colours", 12L)

      # infer number of discrete levels safely
      lev_n <- infer_discrete_nlevels(plot, "colour")  # see helper below
      if (!is.na(lev_n) && lev_n > max_n) {
        abort_too_many_discrete_levels("colour", lev_n, max_n)
      }

      plot <- plot + scale_colour_discrete_e61()
    } else if (identical(typ, "continuous")) {
      plot <- plot + scale_colour_continuous_e61()
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

      plot <- plot + scale_fill_discrete_e61()
    } else if (identical(typ, "continuous")) {
      plot <- plot + scale_fill_continuous_e61()
    }
  }


  plot
}

#' @export
ggplot_build.e61_ggplot <- function(plot, ...) {

  plot2 <- maybe_add_default_scales(plot)

  # prevent recursion: drop our class before calling ggplot2 build
  class(plot2) <- setdiff(class(plot2), "e61_ggplot")

  ggplot2::ggplot_build(plot2, ...)
}
