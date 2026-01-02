#' Masks ggplot2::ggplot to use theme_e61() by default
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
                       environment = environment) +
    theme_e61()

  as_e61_plot(p)

}

#' Generic function to coerce plots to e61_plot class
#' @export
as_e61_plot <- function(x, ...) {
  UseMethod("as_e61_plot")
}

#' Method for plain ggplots
#' @export
as_e61_plot.ggplot <- function(x, ...) {
  if (!inherits(x, "e61_plot")) {
    class(x) <- c("e61_plot", class(x))
  }
  x
}

#' Method for existing e61_plot class objects
#' @export
as_e61_plot.e61_plot <- function(x, ...) {
  x
}

#' Method that works for lists of plots
#' @export
as_e61_plot.list <- function(x, ...) {
  lapply(x, as_e61_plot)
}

#' Method that fails for non-plots
#' @export
as_e61_plot.default <- function(x, ...) {
  stop(
    "Object of class ", paste(class(x), collapse = "/"),
    " cannot be converted to an e61 plot"
  )
}

#' Masks ggplot2::ggsave to encourage users to use save_e61
#'
#' @noRd
#' @export
ggsave <- function(...) {

  # Throw warning message (unless testing)
  if (!isTRUE(getOption("quiet_wrap")))
    cli::cli_bullets(c("x" = "Your function arguments have been passed to save_e61() automatically. Please use save_e61() instead of ggsave() to ensure your graphs conform to the e61 style correctly. If you still want to use ggplot2's ggsave(), provide the namespace explicitly."))

  save_e61(...)
}

#' Masks ggplot2::labs to encourage users to use labs_e61
#'
#' @noRd
#' @export
labs <- function(...) {

  # Throw warning message (unless testing)
  if (!isTRUE(getOption("quiet_wrap")))
    cli::cli_bullets(c("x" = "Your function arguments have been passed to labs_e61() automatically. Please use labs_e61() instead of labs() to ensure your graphs conform to the e61 style correctly. If you still want to use ggplot2's labs(), provide the namespace explicitly."))

  labs_e61(...)
}

#' Masks ggplot2::facet_wrap to set axes to "all" better distinguish facet
#' panels
#'
#' @noRd
#' @export
facet_wrap <- function(..., axes = "all") {

  ggplot2::facet_wrap(..., axes = axes)
}

#' Masks ggplot2::facet_grid to set axes to "all" better distinguish facet
#' panels
#'
#' @noRd
#' @export
facet_grid <- function(..., axes = "all") {

  ggplot2::facet_grid(..., axes = axes)
}
