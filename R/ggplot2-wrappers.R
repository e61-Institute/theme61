Masks ggplot2::ggsave to encourage users to use save_e61
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

#' Masks ggplot2::scale_y_continuous to encourage users to use labs_e61
#'
#' @noRd
#' @export
scale_y_continuous <- function(...) {

  # Throw warning message (unless testing)
  if (!isTRUE(getOption("quiet_wrap")))
    cli::cli_bullets(c("x" = "Please use scale_y_continuous_e61() instead of scale_y_continuous() to ensure your graphs conform to the e61 style correctly."))

  ggplot2::scale_y_continuous(...)
}
