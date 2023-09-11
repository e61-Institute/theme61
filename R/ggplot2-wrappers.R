#' Masks ggplot2::ggplot to use the e61 colour palette and scales by default
#'
#' @noRd
#' @export
ggplot <-
  function(data = NULL,
           mapping = aes(),
           ...,
           environment = parent.frame()) {

  p <- ggplot2::ggplot(data = data, mapping = mapping, environment = environment) + theme_e61()

  # add e61 y-axis scale if the y-variable is numeric
  if(!is.null(mapping$y)) {

    y_var_name <- ggplot2::quo_name(mapping$y)
    y_var_class <- data[[y_var_name]] %>% class()

    if(y_var_class == "numeric"){
      p <- p + scale_y_continuous_e61(y_top = T)
    }
  }

  # add e61 x-axis scale if the x-variable is numeric
  if(!is.null(mapping$x)) {

    x_var_name <- ggplot2::quo_name(mapping$x)
    x_var_class <- data[[x_var_name]] %>% class()

    if(x_var_class == "numeric"){
      p <- p + scale_x_continuous_e61()
    }
  }

  # scale fill variables
  if(!is.null(mapping$fill)){

    fill_var_name <- ggplot2::quo_name(mapping$fill)

    if(is.null(data[[fill_var_name]]) & stringr::str_detect(fill_var_name, "^factor\\(")){

      fill_var_class <- "factor"

    } else {
      fill_var_class <- data[[fill_var_name]] %>% class()
    }

    if(fill_var_class == "numeric"){
      p <- p + scale_fill_e61(discrete = F)

    } else if(fill_var_class == "factor" | fill_var_class == "character") {

      p <- p + scale_fill_e61()
    }
  }

  # scale colours
  if(!is.null(mapping$colour)){

    colour_var_name <- ggplot2::quo_name(mapping$colour)

    if(is.null(data[[colour_var_name]]) & stringr::str_detect(colour_var_name, "^factor\\(")){

      colour_var_class <- "factor"

    } else {
      colour_var_class <- data[[colour_var_name]] %>% class()
    }

    if(colour_var_class == "numeric"){
      p <- p + scale_colour_e61(discrete = F)

    } else if(colour_var_class == "factor" | colour_var_class == "character") {

      p <- p + scale_colour_e61()
    }
  }

  return(p)
}

#' Masks ggplot2::ggsave to encourage users to use save_e61
#'
#' @noRd
#' @export
ggsave <- function(...) {

  # Throw warning message (unless testing)
  if (!isTRUE(getOption("quiet_wrap")))
    cli::cli_bullets(c("x" = "Please use save_e61() instead of ggsave() to ensure your graphs conform to the e61 style correctly."))

  ggplot2::ggsave(...)
}

#' Masks ggplot2::labs to encourage users to use labs_e61
#'
#' @noRd
#' @export
labs <- function(...) {

  # Throw warning message (unless testing)
  if (!isTRUE(getOption("quiet_wrap")))
    cli::cli_bullets(c("x" = "Please use labs_e61() instead of labs() to ensure your graphs conform to the e61 style correctly."))

  ggplot2::labs(...)
}
