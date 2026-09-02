#' Re-export the ggplot2 verbs theme61's examples and typical usage call
#' directly, so they're available without attaching ggplot2 explicitly
#' alongside the masked ggplot()/labs()/etc.
#'
#' @noRd
#' @export
aes <- ggplot2::aes
#' @noRd
#' @export
geom_area <- ggplot2::geom_area
#' @noRd
#' @export
geom_bar <- ggplot2::geom_bar
#' @noRd
#' @export
geom_col <- ggplot2::geom_col
#' @noRd
#' @export
geom_errorbar <- ggplot2::geom_errorbar
#' @noRd
#' @export
geom_line <- ggplot2::geom_line
#' @noRd
#' @export
geom_path <- ggplot2::geom_path
#' @noRd
#' @export
geom_point <- ggplot2::geom_point
#' @noRd
#' @export
geom_ribbon <- ggplot2::geom_ribbon
#' @noRd
#' @export
geom_sf <- ggplot2::geom_sf
#' @noRd
#' @export
geom_text <- ggplot2::geom_text
#' @noRd
#' @export
coord_flip <- ggplot2::coord_flip
#' @noRd
#' @export
coord_sf <- ggplot2::coord_sf
#' @noRd
#' @export
position_stack <- ggplot2::position_stack
#' @noRd
#' @export
scale_colour_brewer <- ggplot2::scale_colour_brewer
#' @noRd
#' @export
scale_colour_gradient <- ggplot2::scale_colour_gradient
#' @noRd
#' @export
scale_colour_manual <- ggplot2::scale_colour_manual
#' @noRd
#' @export
scale_fill_manual <- ggplot2::scale_fill_manual

#' Masks ggplot2::ggplot to add e61_plot class and apply theme_e61
#'
#' This wrapper tags the plot so theme61 can inject default scales at build time.
#'
#' @noRd
#' @export
ggplot <- function(data = NULL,
                   mapping = aes(),
                   ...,
                   environment = parent.frame()) {

  p <- ggplot2::ggplot(data = data,
                       mapping = mapping,
                       ...,
                       environment = environment)

  p <- as_e61_plot(p)

  # Applied eagerly (not deferred to save/preview) so a plot is fully themed
  # the moment it's created, for any code that touches it directly. Skipped
  # in iterate_mode, which opts out of all automatic theme61 styling.
  if (isTRUE(getOption("theme61.auto_theme", TRUE)) &&
      !isTRUE(getOption("theme61.iterate_mode", FALSE))) {
    # Merging theme_e61() can silently open the session's default device
    # (confirmed on Windows) -- guard against it.
    p <- t61_with_device(p + theme_e61())
  }

  p
}

#' Masks ggplot2::ggsave to encourage users to use save_e61
#'
#' @noRd
#' @export
ggsave <- function(...) {

  # theme61.iterate_mode: don't redirect to save_e61() at all, just pass
  # straight through to ggplot2::ggsave().
  if (isTRUE(getOption("theme61.iterate_mode", FALSE))) {
    return(ggplot2::ggsave(...))
  }

  # Throw warning message (unless testing)
  if (!isTRUE(getOption("quiet_mask", FALSE)))
    cli::cli_bullets(c("x" = "Your function arguments have been passed to save_e61() automatically. Please use save_e61() instead of ggsave() to ensure your graphs conform to the e61 style correctly. If you still want to use ggplot2's ggsave(), provide the namespace explicitly."))

  save_e61(...)
}

#' Masks ggplot2::labs to encourage users to use labs_e61
#'
#' @noRd
#' @export
labs <- function(...) {

  # theme61.iterate_mode: don't redirect to labs_e61() at all, just pass
  # straight through to ggplot2::labs().
  if (isTRUE(getOption("theme61.iterate_mode", FALSE))) {
    return(ggplot2::labs(...))
  }

  # Throw warning message (unless testing)
  if (!isTRUE(getOption("quiet_mask", FALSE)))
    cli::cli_bullets(c("x" = "Your function arguments have been passed to labs_e61() automatically. Please use labs_e61() instead of labs() to ensure your graphs conform to the e61 style correctly. If you still want to use ggplot2's labs(), provide the namespace explicitly."))

  labs_e61(...)
}

#' Shared body for facet_wrap()/facet_grid() -- identical apart from which
#' ggplot2 facet function they wrap. `axes_missing` must be `missing(axes)`
#' evaluated in the caller's frame (facet_wrap()/facet_grid() itself),
#' since evaluating it in here would always see axes as supplied (it's
#' always forwarded explicitly by the caller).
#' @noRd
.mask_facet <- function(facet_fn, axes_missing, ..., axes) {

  # theme61.iterate_mode: don't force axes = "all", just pass straight
  # through to ggplot2's facet function (defaults to axes = "margins"),
  # unless the user explicitly asked for a specific axes value.
  if (isTRUE(getOption("theme61.iterate_mode", FALSE))) {
    if (axes_missing) return(facet_fn(...))
    return(facet_fn(..., axes = axes))
  }

  f <- facet_fn(..., axes = axes)
  attr(f, "t61_axes") <- axes
  f
}

#' Masks ggplot2::facet_wrap to set axes to "all" better distinguish facet
#' panels
#'
#' @noRd
#' @export
facet_wrap <- function(..., axes = "all") {
  .mask_facet(ggplot2::facet_wrap, missing(axes), ..., axes = axes)
}

#' Masks ggplot2::facet_grid to set axes to "all" better distinguish facet
#' panels
#'
#' @noRd
#' @export
facet_grid <- function(..., axes = "all") {
  .mask_facet(ggplot2::facet_grid, missing(axes), ..., axes = axes)
}
