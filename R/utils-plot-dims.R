#' Get the width/height of a single grob (or list of grobs, for faceted
#' ggplots) along the given dimension - shared implementation for
#' get_grob_width()/get_grob_height().
#' grob - A single grob, or a list of grobs (faceted ggplots).
#' dim - "width" or "height".
#' @noRd
get_grob_dim <- function(grob, dim){

  convert <- if(dim == "width") grid::convertWidth else grid::convertHeight
  plural <- paste0(dim, "s")

  dim_of <- function(g){
    if(g$name == "NULL") {
      0

    } else if(!is.null(g[[dim]])){
      sum(convert(g[[dim]], "cm", valueOnly = TRUE), na.rm = T)

    } else if(!is.null(g[[plural]])){
      sum(convert(g[[plural]], "cm", valueOnly = TRUE), na.rm = T)

    } else {
      0
    }
  }

  # if it is a single grob - non faceted ggplots
  if(!is.null(grob$name)){
    return(dim_of(grob))
  }

  # if it is a faceted ggplot, then return the first non-zero dimension - to avoid double counting
  if(is.list(grob)) {

    for(i in seq_along(grob)){
      val <- dim_of(grob[[i]])
      if(val != 0) return(val)
    }

    return(0)
  }

  0
}

#' Get the widths of non-zero, non-NULL grobs
#' ggplotGrob - The set of grobs for the plot we want to find widths for
#' grob_name - The name of the grob you want to find the width of
#' @noRd
get_grob_width <- function(ggplotGrob, grob_name){

  grob <- ggplotGrob$grobs[which(stringr::str_detect(ggplotGrob$layout$name, paste0("^", grob_name)))]

  get_grob_dim(grob, "width")
}

#' Get the heighs of non-zero, non-NULL grobs
#' ggplotGrob - The set of grobs for the plot we want to find widths for
#' grob_name - The name of the grob you want to find the width of
#' @noRd
get_grob_height <- function(ggplotGrob, grob_name){

  grob <- ggplotGrob$grobs[which(stringr::str_detect(ggplotGrob$layout$name, paste0("^", grob_name)))]

  get_grob_dim(grob, "height")
}

#' Get a ggplotGrob's outer plot.margin along the given dimension - this is
#' always the first and last row (height) or column (width) of the gtable.
#' dim - "width" or "height".
#' @noRd
get_margin_dim <- function(ggplotGrob, dim){

  convert <- if(dim == "width") grid::convertWidth else grid::convertHeight
  vec <- if(dim == "width") ggplotGrob$widths else ggplotGrob$heights

  sum(convert(vec[c(1, length(vec))], "cm", valueOnly = TRUE))
}
