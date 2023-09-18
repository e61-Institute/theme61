#' Get the known width of the plot
#' plot The plot to find the heigh for
#' is_mpanel Logical. Is the plot an mpanel chart.
#' @noRd
get_known_width <- function(plot, is_mpanel){
  if(is_mpanel){

    width <- attr(plot, "known_width")

  } else {
    p <- ggplot2::ggplotGrob(plot)

    width <- sum(grid::convertWidth(p$widths, "cm", valueOnly = TRUE), na.rm = T)
  }

  return(width)
}

#' Get the known height of the plot
#' plot The plot to find the height for.
#' is_mpanel Logical. Is the plot an mpanel chart.
#' @noRd
get_known_height <- function(plot, is_mpanel){

  if(is_mpanel){

    height <- attr(plot, "known_height")

  } else {
    p <- ggplot2::ggplotGrob(plot)

    height <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))
  }

  return(height)
}

#' Get the widths of non-zero, non-NULL grobs
#' ggplotGrob - The set of grobs for the plot we want to find widths for
#' grob_name - The name of the grob you want to find the width of
#' @noRd
get_grob_width <- function(ggplotGrob, grob_name){

  grob <- ggplotGrob$grobs[which(stringr::str_detect(ggplotGrob$layout$name, paste0("^", grob_name)))]

  # if it is a single grob - non faceted ggplots
  if(!is.null(grob$name)){

    if(grob$name == "NULL") {
      width <- 0

    } else if(!is.null(grob$width)){
      width <- sum(grid::convertWidth(grob$width, "cm", valueOnly = TRUE), na.rm = T)

    } else if(!is.null(grob$widths)){
      width <- sum(grid::convertWidth(grob$widths, "cm", valueOnly = TRUE), na.rm = T)

    } else {
      width <- 0
    }

  # if it is a faceted ggplot, then return the first non-zero width - to avoid double counting
  } else if(is.list(grob)) {

    width <- 0

    for(i in seq_along(grob)){

      temp_grob <- grob[[i]]

      if(temp_grob$name == "NULL") {
        width <- 0

      } else if(!is.null(temp_grob$width)){
        width <- sum(grid::convertWidth(temp_grob$width, "cm", valueOnly = TRUE), na.rm = T)

      } else if(!is.null(temp_grob$widths)){
        width <- sum(grid::convertWidth(temp_grob$widths, "cm", valueOnly = TRUE), na.rm = T)

      } else {
        width <- 0
      }

      # return the first non-zero width
      if(width != 0) break
    }
  }

  return(width)
}
