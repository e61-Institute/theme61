#' Knit print method for theme61 plots
#'
#' Plots shown in knitr/Quarto documents never go through [save_e61()], so
#' knitr draws them at the chunk's `fig.width`/`fig.height` (7 x 5in by
#' default) - roughly twice the size `save_e61()` lays a chart out for. Text
#' keeps its absolute point size while the panel stretches, so it looks far
#' too small. To compensate, the text is scaled by how much larger the figure
#' is than the chart `save_e61()` would make, before printing as usual.
#'
#' Set the `theme61.knit_scale_text` option to FALSE to turn this off.
#'
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.e61_plot <- function(x, options = knitr::opts_current$get(), ...) {

  if (isTRUE(getOption("theme61.iterate_mode", FALSE)) ||
      isFALSE(getOption("theme61.knit_scale_text", TRUE))) {
    return(NextMethod())
  }

  # Printing registers the scaled copy as last_plot(); restore the original
  # so a later bare save_e61() doesn't save the enlarged text.
  on.exit(ggplot2::set_last_plot(x), add = TRUE)

  knitr::normal_print(t61_scale_text(x, t61_knit_text_scale(x, options$fig.width, options$fig.height)))
}

#' Ratio of a knitr figure's size to the size save_e61() would give the same
#' chart, so the figure looks like the saved chart scaled to fit inside it.
#' Uses whichever dimension is relatively tighter so a short, wide figure
#' can't blow the text up past the panel.
#' @noRd
t61_knit_text_scale <- function(plot, fig_width, fig_height) {
  if (!is.numeric(fig_width) || !is.numeric(fig_height)) return(1)

  ref <- t61_saved_size(plot)

  min(fig_width / ref[["width"]], fig_height / ref[["height"]])
}

#' Approximate save_e61()'s chart size in inches from a single layout pass,
#' using the same width rules and height calculation as save_single() but
#' skipping its label wrapping, axis rescaling and auto-positioning.
#' @noRd
t61_saved_size <- function(plot) {
  plot <- finalise_e61_plot(plot)

  full_width <- length(plot@facet$params) != 0 || inherits(plot@coordinates, "CoordFlip")
  width <- cm_to_in(if (full_width) 18.59 else 18.59 / 2)

  # Text-sized units are measured when converted, so convert on the same
  # device save_e61() measures with rather than whichever one is current.
  t61_with_device({
    g <- t61_quiet_na_removal(ggplot2::ggplotGrob(maybe_add_default_scales(plot)))
    null_ht <- grid::unitType(g$heights) == "null"
    null_wd <- grid::unitType(g$widths) == "null"

    known_ht <- sum(grid::convertHeight(g$heights[!null_ht], "in", valueOnly = TRUE))
    known_wd <- sum(grid::convertWidth(g$widths[!null_wd], "in", valueOnly = TRUE))
    panel_ht <- (width - known_wd) * sum(as.numeric(g$heights[null_ht])) / sum(as.numeric(g$widths[null_wd]))

    c(width = width, height = known_ht + panel_ht)
  })
}

#' Scale a plot's base text size (and so every rel() text size in the theme),
#' the theme61 margins derived from it, inline font sizes in the labels, and
#' fixed-size text layers such as plot_label().
#' @noRd
t61_scale_text <- function(plot, scale) {
  if (isTRUE(all.equal(scale, 1))) return(plot)

  base_size <- (plot@theme$text$size %||% getOption("theme61.base_size", 10)) * scale

  plot <- plot +
    theme(text = element_text(size = base_size)) +
    update_margins(plot@theme, base_size, plot@theme$legend.title)

  # labs_e61() fixes title/subtitle/caption sizes with inline markup.
  for (lab in c("title", "subtitle", "caption")) {
    plot@labels[[lab]] <- t61_scale_inline_pt(plot@labels[[lab]], scale)
  }

  for (i in seq_along(plot@layers)) {
    layer <- plot@layers[[i]]
    if (inherits(layer$geom, c("GeomText", "GeomLabel")) && is.numeric(layer$aes_params$size)) {
      # Layers are environments: scale a child so the user's plot isn't
      # mutated (and rescaled again) on every render. A new name is needed as
      # ggproto() looks its parent up by name, lazily.
      scaled <- ggplot2::ggproto(NULL, layer)
      scaled$aes_params$size <- layer$aes_params$size * scale
      plot@layers[[i]] <- scaled
    }
  }

  plot
}

#' Multiply every `font-size:<n>pt` in a label's markup by `scale`.
#' @noRd
t61_scale_inline_pt <- function(text, scale) {
  if (!is.character(text) || !any(grepl("font-size:", text, fixed = TRUE))) return(text)

  # regmatches<- drops attributes, which labs_e61() uses for wrap flags.
  out <- text
  m <- gregexpr("(?<=font-size:)\\s*[0-9.]+(?=pt)", out, perl = TRUE)
  regmatches(out, m) <- lapply(regmatches(out, m), function(x) format(as.numeric(x) * scale, digits = 4))
  attributes(out) <- attributes(text)
  out
}
