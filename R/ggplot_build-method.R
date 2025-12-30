# Find the first mapping for aes_name from plot mapping, else from layers.
# Returns list(quo = <quosure>, data = <data.frame>) or NULL.
find_aes <- function(plot, aes_name) {

  if (!is.null(plot@data)) {
    plot_data <- plot@data
  } else if (!is.null(plot$data)) {
    plot_data <- plot$data
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

  plot
}

#' @export
ggplot_build.e61_ggplot <- function(plot, ...) {

  plot2 <- maybe_add_default_scales(plot)

  # prevent recursion: drop our class before calling ggplot2 build
  class(plot2) <- setdiff(class(plot2), "e61_ggplot")

  ggplot2::ggplot_build(plot2, ...)
}
