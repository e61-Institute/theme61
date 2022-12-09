#' Wrapper function to produce e61 style graph footers
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
#' @param footnotes A vector of footnote text strings. Each new string will be
#'   prepended with \*, \*\*, \*\*\*, etc. Please be sensible with the number of
#'   separate points you include in the graph.
#' @param sources A vector of providing the names of sources for the graph.
#' @param title_max_char Set the maximum number of characters per line in the
#'   title, the default is appropriate for the default graph dimensions in
#'   \code{e61_save}.
#' @param subtitle_max_char Set the maximum number of characters per line in the
#'   subtitle, the default is appropriate for the default graph dimensions in
#'   \code{e61_save}.
#' @param footnote_max_char Set the maximum number of characters per line in the
#'   footer, the default is appropriate for the default graph dimensions in
#'   \code{e61_save}.
#' @inheritDotParams ggplot2::labs
#'
#' @export

labs_e61 <- function(title,
                     subtitle = NULL,
                     footnotes = NULL,
                     sources = NULL,
                     title_max_char = 65L,
                     subtitle_max_char = 75L,
                     footnote_max_char = 90L,
                     ...) {

  # We need this to check for the presence of caption in the passed-through
  # arguments, specifically the caption
  dots <- list(...)

  if ((!is.null(footnotes) || !is.null(sources)) && !is.null(dots$caption))
    stop("Do not use the caption argument if you use the footnotes or sources arguments.")

  if (!is.integer(title_max_char) || title_max_char < 0)
    stop("title_max_char must be a positive integer.")

  if (!is.integer(footnote_max_char) || footnote_max_char < 0)
    stop("footnote_max_char must be a positive integer.")

  if (!is.character(title))
    stop("title must be a string.")

  # Stop titles from being too long by wrapping it to multiple lines
  title <- paste(strwrap(title, width = title_max_char), collapse = "\n")

  if (!is.null(subtitle)) {

    if (!is.character(subtitle))
      stop("subtitle must be a string.")

    subtitle <- paste(strwrap(subtitle, width = subtitle_max_char), collapse = "\n")
  }

  # Footnotes
  if (!is.null(footnotes)) {

    # Sense check inputs
    if (!is.vector(footnotes) || !is.character(footnotes))
      stop("footnotes must be a vector of strings.")

    # Stops footnote text from spilling over the RHS of graphs if they are lengthy
    footnotes <-
      sapply(footnotes, function(x)
        paste(strwrap(x, width = footnote_max_char), collapse = "\n"))

    # Creates the correct number of asterisks
    footnotes <- data.frame(n = seq(1, length(footnotes)), text = footnotes)
    footnotes$n <- strrep("*", footnotes$n)
    footnotes <- paste0(footnotes$n, " ", footnotes$text)

  }

  # Sources
  if (!is.null(sources)) {

    # Sense check inputs
    if (!is.vector(sources) || !is.character(sources))
      stop("sources must be a vector of strings.")

    # Source list should be in alphabetical order
    sources <- sort(sources)

    # Construct the list of sources
    source_list <- paste(sources, collapse = "; ")
    sources <-
      paste0(ifelse(length(sources) > 1, "Sources: ", "Source: "), source_list)

    # Unlikely, but stops sources text from spilling over the RHS of graphs if
    # they are lengthy
    sources <-
      paste(strwrap(sources, width = footnote_max_char), collapse = "\n")

  }

  # Put it all together
  caption <- paste0(c(footnotes, sources), collapse = "\n")
  if (caption == "") caption <- NULL # Return NULL caption if blank

  label <-
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         ...)

  return(label)
}
