#' Custom geom combining points and error bars
#'
#' This geom combines the functionality of geom_point() and geom_errorbar(),
#' drawing error bars behind points with separate control over their parameters.
#'
#' @param mapping Set of aesthetic mappings created by aes(). Supports all
#'   aesthetics from both geom_point and geom_errorbar.
#' @param data The data to be displayed in this layer
#' @param stat The statistical transformation to use on the data
#' @param position Position adjustment
#' @param na.rm If FALSE, the default, missing values are removed with a
#'   warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics
#' @param point.size Size for points (overrides size aesthetic for points only)
#' @param errorbar.width Width of error bar caps
#' @param errorbar.linewidth Line width for error bars
#' @param ... Other arguments passed on to layer()
#'
#' @section Aesthetics: geom_pointbar() understands the following aesthetics
#'   (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{y}
#'   \item \strong{ymin} (for error bars)
#'   \item \strong{ymax} (for error bars)
#'   \item alpha
#'   \item colour
#'   \item fill
#'   \item group
#'   \item shape (points only)
#'   \item size (points only, use point.size parameter for override)
#'   \item stroke (points only)
#'   \item linetype (error bars only)
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' df <- data.frame(
#'   x = 1:5,
#'   y = c(2, 3, 1, 4, 3),
#'   ymin = c(1.5, 2.2, 0.5, 3.1, 2.3),
#'   ymax = c(2.5, 3.8, 1.5, 4.9, 3.7)
#' )
#'
#' ggplot(df, aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
#'   geom_pointbar()
#'
#' # With custom styling
#' ggplot(df, aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
#'   geom_pointbar(
#'     point.size = 3,
#'     errorbar.width = 0.2,
#'     colour = "blue",
#'     errorbar.linewidth = 1
#'   )
#'
#' @export
geom_pointbar <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          point.size = NULL,
                          errorbar.width = 0.1,
                          errorbar.linewidth = 0.5,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  # Extract additional parameters
  params <- list(...)

  # Create modified mappings for each geom
  errorbar_mapping <- mapping
  point_mapping <- mapping

  # If point_mapping has ymin/ymax, remove them
  if (!is.null(point_mapping)) {
    point_mapping$ymin <- NULL
    point_mapping$ymax <- NULL
  }

  # Create the layers list
  layers <- list()

  # Error bar layer (drawn first, behind points)
  errorbar_params <- params
  errorbar_params$width <- errorbar.width
  errorbar_params$linewidth <- errorbar.linewidth
  errorbar_params$na.rm <- na.rm

  layers[[1]] <- suppressWarnings(layer(
    data = data,
    mapping = errorbar_mapping,
    stat = stat,
    geom = GeomErrorbar,
    position = position,
    show.legend = if (is.na(show.legend)) FALSE else show.legend,
    inherit.aes = inherit.aes,
    params = errorbar_params
  ))

  # Point layer (drawn second, in front of error bars)
  point_params <- params
  if (!is.null(point.size)) {
    point_params$size <- point.size
  }
  point_params$na.rm <- na.rm

  layers[[2]] <- suppressWarnings(layer(
    data = data,
    mapping = point_mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = point_params
  ))

  # Return the layers
  return(layers)
}
