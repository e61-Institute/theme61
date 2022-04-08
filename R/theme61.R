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
#' @param base_size Numeric. Chart font size. Default 10.
#' @param base_family Character. Chart font family. Default Quattrocento Sans
#' @param base_line_size Numeric. Default line width.
#' @param base_rect_size Numeric. Default rect width.
#' @param background Character. Default chart background colour.
#' @param legend Character. Legend position.
#' @param legend_title Logical. Include Legend Title?
#' @param panel_borders Logical. Add panel borders?
#'
#' @return ggplot2 object
#' @import ggplot2
#' @export
#'
#' @examples
#' ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#' geom_point() +
#' grattan_colour_manual(n = 3) +
#' theme_e61()
#'

theme_e61 <- function(base_size = 14,
                      base_family = "Quattrocento Sans",
                      base_line_size = points_to_mm(0.75),
                      base_rect_size = points_to_mm(1),
                      background = "white",
                      legend = "none",
                      legend_title = FALSE,
                      panel_borders = FALSE) {

  sysfonts::font_add_google("Quattrocento Sans", "Quattrocento Sans")
  showtext::showtext_auto()

  half_line <- base_size / 2

  ret <-
    theme(
      line = element_line(
        colour = theme61::e61_greylight6,
        size = base_line_size,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = background,
        colour = theme61::e61_greylight6,
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
      axis.line = element_line(
        size = points_to_mm(1),
        colour = "black"
      ),
      axis.line.x = NULL,
      axis.line.y = NULL,
      axis.text = element_text(size = rel(1)),
      axis.text.x = element_text(margin = margin(t = base_size / 5,
                                                 unit = "pt"),
                                 vjust = 1),
      axis.text.x.top = element_text(margin = margin(b = base_size / 5),
                                     vjust = 0),
      axis.text.y = element_text(margin = margin(r = base_size / 5),
                                 hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = base_size / 5),
                                       hjust = 0),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.ticks.length.x = NULL,
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
        margin = margin(r = half_line /
                          2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line /
                          2),
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
      legend.text = element_text(size = rel(1),
                                 margin = margin(l = 0,
                                                 r = base_size / 4, unit = "pt")),
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
      panel.border = element_blank(),
      panel.grid = element_line(colour = theme61::e61_greylight6,
                                size = points_to_mm(0.5)),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1,
                           "lines"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,
      strip.background = element_rect(fill="black"),
      strip.text = element_text(
        colour = "white",
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
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(
        size = rel(1),
        hjust = 0,
        vjust = 1,
        colour = theme61::e61_greydark2,
        face = "bold",
        margin = margin(b = half_line)
      ),
      plot.subtitle = element_text(
        colour = theme61::e61_greydark4,
        hjust = 0,
        vjust = 1,
        margin = margin(t = 0,
                        r = 0,
                        b = base_size * .75,
                        l = 0,
                        unit = "pt")
      ),
      plot.caption = element_text(
        family = base_family,
        size = rel(0.555),
        hjust = 0,
        vjust = 1,
        colour = "black",
        face = "italic",
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
  if(legend_title){
    ret <- ret %+replace%
      theme(legend.title = element_text(size = rel(1),
                           margin = margin(l = 0,
                           r = base_size / 4, unit = "pt")))
  }


  if (panel_borders) {
    ret <- ret %+replace%
      theme(panel.border = element_rect(
        linetype = 1,
        size = points_to_mm(2),
        colour = "black",
        fill = NA
      ))
  }


  if (background == "orange" |  background == "box") {
    ret <- ret +
      ggplot2::theme(rect = element_rect(fill = e61_greylight6))
  }

  ret

}
