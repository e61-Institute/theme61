#' e61 themed graph options
#'
#' Applies the e61 theme to ggplot graphs and provides arguments to adjust graph
#' appearance. If you are looking to change the appearance of titles or labels,
#' check the arguments in [labs_e61], which are probably what you are looking
#' for.
#'
#' @param legend Character. Legend position, "none" (default) hides the legend.
#' @param legend_position A numeric vector of length two setting the placement
#'   of legends that have the "inside" position. Takes values between 0 and 1.
#' @param legend_title Logical. Include legend title? Defaults to FALSE.
#' @param aspect_ratio Numeric. Sets the aspect ratio of the graph panel.
#' @param background Character. Default is "white". For all graphs that you
#'   save, you should control the background colour using the `bg_colour`
#'   argument in `save_e61`, not here.
#' @param base_family Character. Chart font family. Default for notes is PT
#'   Sans.
#' @param base_line_size Numeric. Default line width.
#' @param base_rect_size Numeric. Default rect width.
#'
#' @return \code{theme_e61} returns a ggplot2 object.
#' @import ggplot2
#' @rdname theme_e61
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'   geom_point() +
#'   theme_e61()
#' }
#'

theme_e61 <- function(
    legend = c("none", "bottom", "top", "left", "right", "inside"),
    legend_position = NULL,
    legend_title = FALSE,
    aspect_ratio = 0.75,
    background = "white",
    base_family = "pt-sans",
    base_line_size = points_to_mm(0.75),
    base_rect_size = points_to_mm(1)
    ) {

  legend <- match.arg(legend)

  if (legend == "inside") {
    if (!is.numeric(legend_position) || length(legend_position) != 2)
      stop("legend_position needs to be a length two numeric vector.")

    if (!(data.table::between(legend_position[[1]], 0, 1) | data.table::between(legend_position[[2]], 0, 1)))
      stop("Both legend_position values must be between 0 and 1.")
  }

  base_family <- if (is_testing()) "sans" else "pt-sans"

  base_size <- getOption("t61_base_size", default = 10)

  half_line <- base_size / 2

  ret <-
    theme(
      line = element_line(colour = "black", linewidth = points_to_mm(0.5)),
      rect = element_rect(fill = "white", colour = NA),
      text = element_text(colour = "black", family = base_family, size = base_size),
      aspect.ratio = NULL,

      # Axes and grid
      axis.line.x = element_line(colour = "black", linewidth = points_to_mm(0.4)),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_blank(),
      axis.text = element_text(size = rel(0.9), colour = "black"),

      # x-axis title
      axis.title.x = element_text(size = rel(1), margin = margin(t = half_line)),

      # Legend styling (none, or inline preferred)
      legend.position = "none",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = rel(0.9)),

      # Panel styling
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#D9D9D9",
                                        linewidth = points_to_mm(0.5)),
      panel.grid.minor = element_blank(),

      # Plot layout
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_blank(),
      plot.title = element_text(
        size = rel(1.3),
        hjust = 0,
        face = "bold",
        margin = margin(b = 4)
      ),
      plot.subtitle = ggtext::element_markdown(
        size = rel(1.15),
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        size = rel(0.8),
        hjust = 0,
        margin = margin(t = 12)
      ),
      plot.margin = margin(t = 14, r = 8, b = 8, l = 8),

      # Strip (facets)
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1), face = "bold")
    )

  # add the basics of the legend
  ret <- ret +
    theme(
      legend.position = legend,
      legend.title = element_blank()
    )

  # add legend position if inside
  if (legend == "inside") {
    ret <- ret +
      theme(
        legend.position.inside = legend_position
      )
  }

  # adjust legend direction based on legend position
  if (data.table::like(legend, "bottom|top", ignore.case = TRUE)) {
    ret <- ret + theme(legend.direction = "horizontal")
  }

  # Adds a grey background option
  if (background == "grey" |  background == "box") {
    ret <- ret + theme(rect = element_rect(fill = e61_greylight6))
  }

  # Adjust spacing between facets if facets used
  if (!inherits(ret$facet, "FacetNull")) {
    ret <- ret %+replace% theme(panel.spacing.x = unit(2, "lines"),
                                panel.spacing.y = unit(2, "lines"))
  }

  # Add attribute to identify it as a theme61 object
  class(ret) <- c("theme_e61", class(ret))
  attr(ret, "t61_obj") <- TRUE

  return(ret)
}

#' e61 themed spatial maps options
#'
#' Applies the e61 theme to ggplot spatial maps to adjust graph appearance. If
#' you are looking to change the appearance of titles or labels, check the
#' arguments in [theme61::labs_e61()], which are probably what you are looking
#' for.
#'
#' @inheritParams theme_e61
#' @return \code{theme_e61_spatial} returns a ggplot2 object.
#' @import ggplot2
#' @export
#' @family map functions
#'
#' @examples
#'
#' \dontrun{
#' library(sf)
#'
#' sa3_shp <- strayr::read_absmap("sa32016")
#'
#' sydney_map <- filter(sa3_shp, gcc_code_2016 == "1GSYD")
#'
#' ggplot(data = sydney_map) +
#'   geom_sf(aes(fill = sa3_code_2016), colour = "black") +
#'   theme_e61_spatial()
#' }
#'
theme_e61_spatial <- function(
    legend = c("none", "bottom", "top", "left", "right", "inside"),
    legend_position = NULL,
    legend_title = FALSE,
    base_family = "pt-sans"
) {
  legend <- match.arg(legend)

  if (legend == "inside") {
    if (!is.numeric(legend_position) || length(legend_position) != 2) {
      stop("legend_position needs to be a length two numeric vector.")

    if (!(data.table::between(legend_position[[1]], 0, 1) | data.table::between(legend_position[[2]], 0, 1)))
      stop("Both legend_position values must be between 0 and 1.")
    }
  }

  base_family <- if (is_testing()) "sans" else base_family
  base_size <- getOption("t61_base_size", default = 10)
  half_line <- base_size / 2

  ret <-
    theme(
      # base text
      text = element_text(
        colour = "black",
        family = base_family,
        size = base_size
      ),
      # titles
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(
        size = rel(1.3),
        hjust = 0,
        face = "bold",
        margin = margin(b = half_line)
      ),
      plot.subtitle = ggtext::element_markdown(
        size = rel(1.15),
        hjust = 0,
        margin = margin(b = base_size)
      ),
      plot.caption = element_text(
        size = rel(0.8),
        hjust = 0,
        margin = margin(t = half_line)
      ),
      plot.margin = margin(t = half_line * 2, r = half_line, b = half_line, l = half_line),

      # panel and axes
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),

      # grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # legend
      legend.position = legend,
      legend.direction = if (grepl("bottom|top", legend)) "horizontal" else "vertical",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = rel(0.9))
    )

  if (legend_title) {
    ret <- ret + theme(legend.title = element_text(size = rel(0.9)))
  } else {
    ret <- ret + theme(legend.title = element_blank())
  }

  if (legend == "inside") {
    ret <- ret + theme(legend.position.inside = legend_position)
  }

  # facet spacing if used
  if (!inherits(ret$facet, "FacetNull")) {
    ret <- ret %+replace% theme(
      panel.spacing.x = unit(2, "lines"),
      panel.spacing.y = unit(2, "lines")
    )
  }

  # Add attribute to identify it as a theme61 object
  class(ret) <- c("theme_e61", class(ret))
  attr(ret, "t61_obj") <- TRUE

  return(ret)
}

#' Converts all legend colours to squares
#'
#' Legend symbols for line graphs default to coloured lines, which can sometimes
#' be hard to read. This function overrides the default and converts the colours
#' to squares. This needs to be used in conjunction with some invisible point
#' geoms so the function has a shape to reshape.
#'
#' @param size Numeric. Control the size of the replacement square. Default of 6
#'   works well when `ymin` or `ymax` are not present.
#' @return ggplot object
#' @export
#' @examples
#' ggplot(
#'   data.frame(x = c(1, 2), y = c(5, 6), group = c("A", "A")),
#'   aes(x, y, colour = group)
#'   ) +
#'   geom_line() +
#'   geom_point(alpha = 0) + # The required "invisible points"
#'   square_legend_symbols()
#'
square_legend_symbols <- function(size = 6) {
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = size, shape = 15)))
}

#' Applies changes to the theme for horizontal bar graphs
#'
#' Horizontal bar graphs made with `coord_flip()` require some changes to
#' the `theme()` in order to look proper. This function wraps those changes
#' up in a convenient function that should be appended at the end of the graph
#' code, after theming functions such as `theme_e61()` have been called.
#'
#' @param x_adj Numeric. Adjusts the vertical position of the x-axis title,
#' the default works for most graphs. A negative value moves the
#' title up, a positive value moves the title down.
#'
#' @return ggplot object
#' @export

format_flip <- function(x_adj = 0) {

  retval <-
    theme(
      panel.grid.major.x = element_line(colour = e61_greylight6, linewidth = points_to_mm(0.5)),
      panel.grid.major.y = element_blank(),
      axis.text.x.top = element_blank(),
      axis.ticks.x.top = element_blank(),
      axis.title.x.top = element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title.x.bottom = element_text(
        margin = margin(t = 0, b = 5),
        hjust = 0.5, angle = 0)
    )

  return(retval)

}

#' Sets the base size for the theme
#'
#' \code{set_base_size} sets the base size for the theme to be used in
#' \code{theme_e61()}. This needs to be set outside of the function because it
#' is called by other functions as part of the graph rendering process.
#' @param base_size Numeric. Graph font size. Default is 10.
#' @return \code{set_base_size} is used for its side effects.
#' @rdname theme_e61
#' @export
set_base_size <- function(base_size) {
  if (!is.numeric(base_size) || length(base_size) != 1 || base_size <= 0) {
    stop("base_size must be a single positive number.")
  }

  options(t61_base_size = base_size)
  invisible()
}

# Internal helper functions ----

# Dimensioning functions
points_to_mm <- function(points) {
  as.numeric(grid::convertX(ggplot2::unit(points, "points"), "mm"))[1]
}

cm_to_in <- function(cm, round = FALSE) {
  inches <- cm / 2.54

  if (isTRUE(round)) {
    round(inches, 2)
  } else {
    inches
  }
}

in_to_cm <- function(inches, round = FALSE) {
  cm <- 2.54 * inches

  if (isTRUE(round)) {
    round(cm, 2)
  } else {
    cm
  }
}

# Reposition y-axis titles to the top
y_title_top <- function(adj, fix_left) {

  if (class(adj) != "numeric") stop("adj must be a number.")
  if (!length(adj) %in% c(1, 2)) stop("adj must be a single value or a vector of 2 values.")

  if (length(adj) == 1) {

    adj_left <- adj
    adj_right <- adj

  } else {

    adj_left <- adj[[1]]
    adj_right <- adj[[2]]

  }

  ret <-
    theme(
      axis.title.y.left = element_text(margin = margin(l = 5 + fix_left, r = adj_left), vjust = 1, angle = 0),
      axis.title.y.right = element_text(margin = margin(l = adj_right, r = 5), vjust = 1, angle = 0)
    )

  return(ret)
}

#' Tell ggplot2 what to do when someone does + theme_e61()
#' @method ggplot_add theme_e61
#' @export
ggplot_add.theme_e61 <- function(object, plot, object_name) {
  # 1) merge in all the theme bits the way ggplot2 normally would
  plot <- ggplot2:::ggplot_add.theme(object, plot, object_name)
  # 2) now copy your flag from the theme onto the plot
  attr(plot, "t61_obj") <- attr(object, "t61_obj")
  plot
}
