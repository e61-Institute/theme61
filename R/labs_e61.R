#' Wrapper function to produce e61 style graph titles and footers
#'
#' Simple wrapper around \code{\link[ggplot2]{labs}} that makes it easier to
#' produce good looking footer text when using footnotes and sources.
#'
#' @details The primary purpose of this function is to correctly format footer
#'   text without requiring the user to guess where to put manual line breaks
#'   for long footnotes or put in "Sources:" themselves. It does this by
#'   transforming the \code{footnotes} and \code{sources} arguments into nicely
#'   formatted text that goes into the \code{caption} argument in ggplot2's
#'   \code{labs()} function. Thus, if you are using \code{footnotes} or
#'   \code{sources}, do not supply a \code{caption} argument as well.
#'
#' @param title The text for the title.
#' @param subtitle The text for the subtitle.
#' @param footnotes A vector of footnote text strings. Each new string will be
#'   prepended with *, **, ***, etc. Note you'll need to include the asterisks
#'   in the title/subtitle yourself. Please be sensible with the number of
#'   separate points you include in the graph.
#' @param sources String vector providing the names of sources for the graph.
#' @param x,y String to set the x- and y-axis titles. Note that the x-axis title
#'   is blank (NULL) by default.
#' @param title_max_char,subtitle_max_char,footnote_max_char Numeric. Set the
#'   maximum number of characters per line in the title, subtitle, sources or
#'   footnotes. The default is roughly appropriate for the default graph
#'   dimensions in \code{e61_save}.
#' @param title_wrap,subtitle_wrap,footnote_wrap Logical. Enables text wrapping
#'   for the title, subtitle, sources or footnotes. Defaults to TRUE.
#' @param ... Additional graph component titles (optional).
#'
#' @export
#' @examples
#'   ggplot() +
#'   theme_e61() +
#'   labs_e61(
#'     title = "Graph title*",
#'     subtitle = "Graph subtitle**",
#'     sources = c("A source", "Company name", "Better source"),
#'     footnotes = c("Footnote 1", "Footnote 2")
#'     )

labs_e61 <- function(title = NULL,
                     subtitle = NULL,
                     footnotes = NULL,
                     sources = NULL,
                     title_max_char = 35,
                     subtitle_max_char = 45,
                     footnote_max_char = 55,
                     title_wrap = TRUE,
                     subtitle_wrap = TRUE,
                     footnote_wrap = TRUE,
                     x = NULL,
                     y = ggplot2::waiver(),
                     ...
                     ) {

  if (!is.numeric(title_max_char) || title_max_char < 0)
    stop("title_max_char must be a positive integer.")

  if (!is.numeric(subtitle_max_char) || subtitle_max_char < 0)
    stop("subtitle_max_char must be a positive integer.")

  if (!is.numeric(footnote_max_char) || footnote_max_char < 0)
    stop("footnote_max_char must be a positive integer.")

  if (!is.null(title) && !is.character(title))
    stop("title must be a string.")

  # Stops header/footer text from being too long
  lab_text <- label_wrap(
    title = title,
    subtitle = subtitle,
    footnotes = footnotes,
    sources = sources,
    title_max_char = title_max_char,
    subtitle_max_char = subtitle_max_char,
    footnote_max_char = footnote_max_char,
    title_wrap = title_wrap,
    subtitle_wrap = subtitle_wrap,
    footnote_wrap = footnote_wrap
  )

  label <-
    ggplot2::labs(
      title = lab_text$title,
      subtitle = lab_text$subtitle,
      caption = lab_text$caption,
      x = x,
      y = y,
      ...
    )

  return(label)
}


