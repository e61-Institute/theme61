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
    p <- p + theme_e61()
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

#' Masks ggplot2::facet_wrap to set axes to "all" better distinguish facet
#' panels
#'
#' @noRd
#' @export
facet_wrap <- function(..., axes = "all") {

  # theme61.iterate_mode: don't force axes = "all", just pass straight
  # through to ggplot2::facet_wrap() (defaults to axes = "margins"), unless
  # the user explicitly asked for a specific axes value.
  if (isTRUE(getOption("theme61.iterate_mode", FALSE))) {
    if (missing(axes)) return(ggplot2::facet_wrap(...))
    return(ggplot2::facet_wrap(..., axes = axes))
  }

  f <- ggplot2::facet_wrap(..., axes = axes)
  attr(f, "t61_axes") <- axes
  return(f)

}

#' Masks ggplot2::facet_grid to set axes to "all" better distinguish facet
#' panels
#'
#' @noRd
#' @export
facet_grid <- function(..., axes = "all") {

  # theme61.iterate_mode: don't force axes = "all", just pass straight
  # through to ggplot2::facet_grid() (defaults to axes = "margins"), unless
  # the user explicitly asked for a specific axes value.
  if (isTRUE(getOption("theme61.iterate_mode", FALSE))) {
    if (missing(axes)) return(ggplot2::facet_grid(...))
    return(ggplot2::facet_grid(..., axes = axes))
  }

  f <- ggplot2::facet_grid(..., axes = axes)
  attr(f, "t61_axes") <- axes
  return(f)
}
