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

#' Get the heighs of non-zero, non-NULL grobs
#' ggplotGrob - The set of grobs for the plot we want to find widths for
#' grob_name - The name of the grob you want to find the width of
#' @noRd
get_grob_height <- function(ggplotGrob, grob_name){

  grob <- ggplotGrob$grobs[which(stringr::str_detect(ggplotGrob$layout$name, paste0("^", grob_name)))]

  # if it is a single grob - non faceted ggplots
  if(!is.null(grob$name)){

    if(grob$name == "NULL") {
      height <- 0

    } else if(!is.null(grob$height)){
      height <- sum(grid::convertHeight(grob$height, "cm", valueOnly = TRUE), na.rm = T)

    } else if(!is.null(grob$heights)){
      height <- sum(grid::convertHeight(grob$heights, "cm", valueOnly = TRUE), na.rm = T)

    } else {
      height <- 0
    }

  # if it is a faceted ggplot, then return the first non-zero height - to avoid double counting
  } else if(is.list(grob)) {

    height <- 0

    for(i in seq_along(grob)){

      temp_grob <- grob[[i]]

      if(temp_grob$name == "NULL") {
        height <- 0

      } else if(!is.null(temp_grob$height)){
        height <- sum(grid::convertHeight(temp_grob$height, "cm", valueOnly = TRUE), na.rm = T)

      } else if(!is.null(temp_grob$heights)){
        height <- sum(grid::convertHeight(temp_grob$heights, "cm", valueOnly = TRUE), na.rm = T)

      } else {
        height <- 0
      }

      # return the first non-zero height
      if(height != 0) break
    }
  }

  return(height)
}
