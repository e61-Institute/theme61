#' Get the known width of the plot
#' @param plot The plot to find the heigh for
#' @param is_mpanel Logical. Is the plot an mpanel chart.
#' @rdname e61_aes_labs
#' @export
get_known_width <- function(plot, is_mpanel){
  if(is_mpanel){

    width <- attr(plot, "known_width")

  } else {
    p <- ggplot2::ggplotGrob(plot)

    width <- sum(grid::convertWidth(p$widths, "cm", valueOnly = TRUE))
  }

  return(width)
}

#' Get the known height of the plot
#' @param plot The plot to find the heigh for.
#' @param is_mpanel Logical. Is the plot an mpanel chart.
#' @rdname e61_aes_labs
#' @export
get_known_height <- function(plot, is_mpanel){

  if(is_mpanel){

    height <- attr(plot, "known_height")

  } else {
    p <- ggplot2::ggplotGrob(plot)

    height <- sum(grid::convertHeight(p$heights, "cm", valueOnly = TRUE))
  }

  return(height)
}
