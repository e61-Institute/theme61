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



#' e61 themed graph options
#'
#' @param base_size Numeric. Chart font size. Default is 14.
#' @param base_family Character. Chart font family. Default is Arial.
#' @param base_line_size Numeric. Default line width.
#' @param base_rect_size Numeric. Default rect width.
#' @param background Character. Options are "white" (default) or "grey".
#' @param legend Character. Legend position, use "none" (default) to hide the
#'   legend.
#' @param legend_title Logical. Include Legend Title? Defaults to FALSE.
#' @param panel_borders Logical. Show panel borders? Defaults to TRUE.
#'
#' @return ggplot2 object
#' @import ggplot2
#' @export
#'
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#' geom_point() +
#' e61_colour_manual(n = 3) +
#' theme_e61()
#'

theme_e61 <- function(base_size = 14,
                      base_family = "Arial",
                      base_line_size = points_to_mm(0.75),
                      base_rect_size = points_to_mm(1),
                      background = "white",
                      legend = c("none", "bottom", "top", "left", "right"),
                      legend_title = FALSE,
                      panel_borders = TRUE) {

  # Consider restoring this as a standalone font installation function for
  # first time running at a later date if we choose to go with a custom font.
  # sysfonts::font_add_google("Quattrocento Sans", "Quattrocento Sans")
  # showtext::showtext_auto()

  half_line <- base_size / 2

  ret <-
    theme(
      line = element_line(
        colour = e61_greylight6,
        size = base_line_size,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = background,
        colour = e61_greylight6,
        size = base_rect_size,
        linetype = 0
      ),
      text = element_text(
        colour = "black",
        family = base_family,
        face = "plain",
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        debug = FALSE,
        margin = margin(),
        size = base_size
      ),
      axis.line = element_line(size = points_to_mm(1),
                               colour = "black"),
      axis.line.x = NULL,
      axis.line.y = NULL,
      axis.line.y.right = element_blank(),
      axis.text = element_text(size = rel(1)),
      axis.text.x = element_text(
        margin = margin(t = base_size / 4,
                        unit = "pt"),
        vjust = 1
      ),
      axis.text.x.top = element_text(margin = margin(b = base_size / 5),
                                     vjust = 0),
      axis.text.y = element_text(margin = margin(r = base_size / 5),
                                 hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = base_size / 5),
                                       hjust = 0),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.ticks.length.x = unit(-1*half_line / 2, "pt"), # Puts ticks inside graph
      axis.ticks.length.x.top = NULL,
      axis.ticks.length.x.bottom = NULL,
      axis.ticks.length.y = NULL,
      axis.ticks.length.y.left = NULL,
      axis.ticks.length.y.right = NULL,
      axis.title = element_text(size = rel(1)),
      axis.title.x = element_text(margin = margin(t = half_line / 2),
                                  vjust = 1),
      axis.title.x.top = element_text(margin = margin(b = half_line / 2),
                                      vjust = 0),
      axis.title.y = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),
      legend.background = element_rect(colour = NA),
      legend.title = element_blank(),
      legend.spacing = unit(half_line, "pt"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = margin(),
      legend.key = element_rect(fill = "white",
                                colour = "white"),
      legend.key.size = unit(1, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(
        size = rel(1),
        margin = margin(l = 0,
                        r = base_size / 4, unit = "pt")
      ),
      legend.text.align = 0,
      legend.title.align = NULL,
      legend.position = legend,
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.margin = margin(0, 0,
                                 0, 0, "cm"),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(half_line, "pt"),
      panel.background = element_rect(colour = NA),
      panel.border = element_rect(
        linetype = 1,
        size = points_to_mm(2),
        colour = "black",
        fill = NA
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = e61_greylight6,
                                  size = points_to_mm(0.5)),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1, "lines"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,
      strip.background = element_blank(),
      strip.text = element_text(
        colour = "black",
        size = rel(1),
        margin = margin(0.8 * half_line,
                        0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
      ),
      strip.text.x = NULL,
      strip.text.y = element_text(angle = -90),
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = unit(half_line / 2,
                                   "pt"),
      strip.switch.pad.wrap = unit(half_line / 2,
                                   "pt"),
      plot.background = element_rect(),
      plot.title.position = "panel",
      plot.caption.position = "panel",
      plot.title = element_text(
        size = rel(1.15),
        hjust = 0.5,
        vjust = 1,
        colour = "black",
        face = "bold",
        margin = margin(b = half_line)
      ),
      plot.subtitle = element_text(
        size = rel(1),
        colour = "black",
        hjust = 0.5,
        vjust = 1,
        margin = margin(
          t = 0, r = 0, b = base_size * .5, l = 0,
          unit = "pt"
          )
      ),
      plot.caption = element_text(
        family = base_family,
        size = rel(0.8),
        hjust = 0,
        vjust = 1,
        colour = "black",
        margin = ggplot2::margin(t = 15)
      ),
      plot.tag = element_text(
        size = rel(1.2),
        hjust = 0.5,
        vjust = 0.5
      ),
      plot.tag.position = "topleft",
      plot.margin = unit(c(0.5, 0.6, 0.1, 0.01), "lines"),
      complete = TRUE
    )

  # add panel borders if the user requests them
  if (legend_title) {
    ret <- ret %+replace%
      theme(legend.title = element_text(size = rel(1),
                           margin = margin(l = 0,
                           r = base_size / 4, unit = "pt")))
  }


  # Remove panel borders if requested
  if (!panel_borders) {
    ret <- ret %+replace%
      theme(panel.border = element_blank())
  }


  # Adds a grey background option
  if (background == "grey" |  background == "box") {
    ret <- ret +
      ggplot2::theme(rect = element_rect(fill = e61_greylight6))
  }

  return(ret)

}

#' Reposition y-axis titles to the top
#'
#' Moves the y-axis titles from the side (ggplot default) to the top of the
#' y-axis and rotates the text to be horizontal.
#'
#' @param adj Either a single numeric to adjust left and right axis titles
#'   simultaneously or a vector of 2 numerics to adjust each axis title
#'   separately. More negative values move the text closer to the graph panel.
#'   Defaults to -18 which seems to work well for y-axis with 1-3 character-wide
#'   values.
#' @param fix_left Optional. Sometimes if the value of the \code{adj} argument
#'   is too negative, the margins on the left side of the graph start to cut off
#'   some of the text. Provide a small positive value (5?) to correct this.
#' @return ggplot object
#' @import ggplot2
#' @export

e61_y_title_top <- function(adj = -18, fix_left = 0) {

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

#' Converts all legend colours to squares
#'
#' Legend symbols for line graphs default to coloured lines, which can sometimes
#' be hard to read. This function overrides the default and converts the colours
#' to squares.
#'
#' @return ggplot object
#' @import ggplot2
#' @export

square_legend_symbols <- function() {
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 6, shape = 15)))
}
