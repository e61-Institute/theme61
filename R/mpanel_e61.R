#' Create a multi-panel graph with e61 formatting
#'
#' Wrapper around \link[ggpubr]{ggarrange} and \code{labs_e61} to create
#' multi-panel graphs with appropriate title and footer formatting.
#'
#' @inheritDotParams ggpubr::ggarrange
#' @inheritParams labs_e61
#'
#' @return ggplot2 object
#' @export
mpanel_e61 <-
  function(...,
           title,
           subtitle = NULL,
           footnotes = NULL,
           sources = NULL,
           title_max_char = 35,
           subtitle_max_char = 45,
           footnote_max_char = 55,
           title_wrap = TRUE,
           subtitle_wrap = TRUE,
           footnote_wrap = TRUE
           ) {

    gg <- ggpubr::ggarrange(...)

    return(gg)

}
