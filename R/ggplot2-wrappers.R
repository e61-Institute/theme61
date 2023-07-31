#' Masks ggplot2::ggsave to encourage users to use save_e61
#'
#' @noRd
#' @export
ggsave <- function(...) {

  # Throw warning message
  cli::cli_bullets(c("x" = "Please use save_e61() instead of ggsave() to ensure your graphs conform to the e61 style correctly."))

  ggplot2::ggsave(...)
}

#' Masks ggplot2::labs to encourage users to use labs_e61
#'
#' @noRd
#' @export
labs <- function(...) {

  # Throw warning message
  cli::cli_bullets(c("x" = "Please use labs_e61() instead of labs() to ensure your graphs conform to the e61 style correctly."))

  ggplot2::labs(...)
}
