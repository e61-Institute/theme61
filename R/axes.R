#' Format axes e61-style
#'
#' @param expand_bottom How much to expand bottom axis line by
#' @param expand_top How much to expand top axis line by
#' @param ...
#'
#' @return
#' @export
#'
#' @rdname e61_axes

e61_y_continuous <- function(expand_bottom = 0,
                             expand_top = 0.015,
                             ...) {
    scale_y_continuous(expand = ggplot2::expansion(mult = c(expand_bottom,
                                                            expand_top)),
                       sec.axis = dup_axis(),
                       ...)
  }


scale_y_continuous_e61 <- function(expand_bottom = 0,
                                   expand_top = 0.015,
                                   ...) {

  e61_y_continuous(expand_bottom = expand_bottom,
                   expand_top = expand_top,
                   ...)
}


e61_x_continuous <- function(expand_left = 0,
                             expand_right = 0.015,
                             ...) {



  scale_x_continuous(expand = ggplot2::expansion(mult = c(expand_left,
                                                          expand_right)),
                     ...)

}

#' @rdname e61_axes
#' @export

scale_x_continuous_e61 <- function(expand_left = 0,
                                   expand_right = 0.015,
                                   ...) {

  e61_x_continuous(expand_left = expand_left,
                       expand_right = expand_right,
                       ...)
}
