#' Knit print method for theme61 plots
#'
#' Plots shown in knitr/Quarto documents never go through [save_e61()], so
#' knitr draws them at the chunk's `fig.width`/`fig.height` (7 x 5in by
#' default) - roughly twice the size `save_e61()` lays a chart out for. Text,
#' lines and points keep their absolute sizes while the panel stretches, so
#' they look far too small. To compensate, every absolute size is scaled by
#' how much larger the figure is than the chart `save_e61()` would make, so
#' the figure looks like the saved chart scaled to fit.
#'
#' Set the `theme61.knit_rescale` option to FALSE to turn this off.
#'
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.e61_plot <- function(x, options = knitr::opts_current$get(), ...) {

  width <- options$fig.width
  height <- options$fig.height

  if (isTRUE(getOption("theme61.iterate_mode", FALSE)) ||
      isFALSE(getOption("theme61.knit_rescale", TRUE)) ||
      !is.numeric(width) || !is.numeric(height)) {
    return(NextMethod())
  }

  scale <- t61_knit_scale(x, width, height)
  print(t61_plots_pane_plot(x, width_cm = width * 2.54, height_cm = height * 2.54, scale = scale))

  # Printing registered the rescaled copy as last_plot(); restore the
  # original so a later bare save_e61() doesn't save the enlarged version.
  ggplot2::set_last_plot(x)

  invisible(x)
}

#' Ratio of a knitr figure's size to the size save_e61() would give the same
#' chart, so the figure looks like the saved chart scaled to fit inside it.
#' Uses whichever dimension is relatively tighter so a short, wide figure
#' can't blow the chart up past the panel.
#' @noRd
t61_knit_scale <- function(plot, fig_width, fig_height) {
  ref <- t61_saved_size(plot)

  min(fig_width / ref[["width"]], fig_height / ref[["height"]])
}

#' Approximate save_e61()'s chart size in inches from a single layout pass,
#' using the same width rules and height calculation as save_single() but
#' skipping its label wrapping, axis rescaling and auto-positioning.
#' @noRd
t61_saved_size <- function(plot) {
  plot <- finalise_e61_plot(plot)
  max_width <- cm_to_in(18.59)
  flipped <- inherits(plot@coordinates, "CoordFlip")

  # Text-sized units are measured when converted, so convert on the same
  # device save_e61() measures with rather than whichever one is current.
  t61_with_device({
    g <- t61_quiet_na_removal(ggplot2::ggplotGrob(maybe_add_default_scales(plot)))
    null_ht <- grid::unitType(g$heights) == "null"
    null_wd <- grid::unitType(g$widths) == "null"

    known_ht <- sum(grid::convertHeight(g$heights[!null_ht], "in", valueOnly = TRUE))
    known_wd <- sum(grid::convertWidth(g$widths[!null_wd], "in", valueOnly = TRUE))
    n_cols <- length(unique(g$layout$l[grepl("^panel", g$layout$name)]))

    # Mirrors save_single(): a single column of panels gets half the page
    # width, more get all of it, and coord_flip() charts get all of it but
    # with the panels capped at half.
    panel_wd <- if (flipped) {
      min(max_width - known_wd, max_width / 2)
    } else if (n_cols == 1) {
      max_width / 2 - known_wd
    } else {
      max_width - known_wd
    }

    panel_ht <- panel_wd * sum(as.numeric(g$heights[null_ht])) / sum(as.numeric(g$widths[null_wd]))

    c(width = panel_wd + known_wd, height = known_ht + panel_ht)
  })
}

#' Scale every absolute size in a (finalised) plot by `scale`: theme text,
#' lines, margins and spacing, the theme's geom defaults (line widths, point
#' sizes, default text size), fixed sizes set on layers, and inline font
#' sizes in labs_e61() markup. Relative sizes (rel(), "lines", "npc") follow
#' along on their own.
#' @noRd
t61_rescale_plot <- function(plot, scale) {
  if (isTRUE(all.equal(scale, 1))) return(plot)

  # save_e61() resizes margins for the chart's base size; start from there.
  base_size <- plot@theme$text$size %||% getOption("theme61.base_size", 10)
  plot <- plot + update_margins(plot@theme, base_size, plot@theme$legend.title)

  theme <- ggplot2::complete_theme(plot@theme)
  for (nm in names(theme)) {
    theme[[nm]] <- t61_scale_theme_entry(theme[[nm]], scale)
  }
  plot@theme <- theme

  # labs_e61() fixes title/subtitle/caption sizes with inline markup.
  for (lab in c("title", "subtitle", "caption")) {
    plot@labels[[lab]] <- t61_scale_inline_pt(plot@labels[[lab]], scale)
  }

  for (i in seq_along(plot@layers)) {
    layer <- plot@layers[[i]]
    fixed <- intersect(names(layer$aes_params), c("size", "linewidth", "stroke"))
    fixed <- fixed[vapply(layer$aes_params[fixed], is.numeric, logical(1))]
    if (length(fixed) == 0) next

    # Layers are environments: scale a child so the user's plot isn't
    # mutated (and rescaled again) on every render. A new name is needed as
    # ggproto() looks its parent up by name, lazily.
    scaled <- ggplot2::ggproto(NULL, layer)
    scaled$aes_params[fixed] <- lapply(layer$aes_params[fixed], `*`, scale)
    plot@layers[[i]] <- scaled
  }

  plot
}

t61_abs_unit_types <- c("points", "bigpts", "picas", "mm", "cm", "inches", "dida", "cicero", "scaledpts")

#' Scale one complete-theme entry: an absolute unit (e.g. axis.ticks.length),
#' or the absolute sizes inside an element.
#' @noRd
t61_scale_theme_entry <- function(entry, scale) {
  if (grid::is.unit(entry)) return(t61_scale_abs_unit(entry, scale))
  if (!inherits(entry, c("element", "ggplot2::element"))) return(entry)

  # S7 elements keep their properties as attributes; ggtext's
  # element_markdown() is a plain list. Both support [[.
  fields <- c(names(attributes(entry)), names(entry))

  for (f in intersect(fields, c("size", "linewidth", "borderwidth", "fontsize", "pointsize"))) {
    value <- entry[[f]]
    if (is.numeric(value) && !inherits(value, "rel")) entry[[f]] <- value * scale
  }
  for (f in intersect(fields, c("margin", "padding", "r"))) {
    entry[[f]] <- t61_scale_abs_unit(entry[[f]], scale)
  }

  entry
}

#' Scale a unit only if it is entirely absolute: "lines"/"npc"/"null" units
#' already scale with text or the panel.
#' @noRd
t61_scale_abs_unit <- function(u, scale) {
  if (grid::is.unit(u) && all(grid::unitType(u) %in% t61_abs_unit_types)) u * scale else u
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
