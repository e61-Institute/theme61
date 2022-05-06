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
#' @param footnote_max_width Set the maximum number of characters per line in
#'   the footer, the default (90) is appropriate for the default graph
#'   dimensions in \code{e61_save}.
#' @inheritDotParams ggplot2::labs
#'
#' @export

e61_labs <- function(title,
                     subtitle = NULL,
                     footnotes = NULL,
                     sources = NULL,
                     footnote_max_width = 90L,
                     ...) {

  # We need this to check for the presence of caption in the passed-through
  # arguments
  dots <- list(...)

  if ((!is.null(footnotes) || !is.null(sources)) && !is.null(dots$caption))
    stop("Do not use the caption argument if you use the footnotes or sources arguments.")

  if (!is.integer(footnote_max_width) || footnote_max_width < 0)
    stop("footnote_max_width must be a positive integer.")

  if (!is.null(footnotes)) {

    # Sense check inputs
    if (!is.vector(footnotes) || !is.character(footnotes))
      stop("footnotes must be a vector of strings.")

    # Stops footnote text from spilling over the RHS of graphs if they are lengthy
    footnotes <-
      sapply(footnotes, function(x)
        paste(strwrap(x, width = footnote_max_width), collapse = "\n"))

    # Creates the correct number of asterisks
    footnotes <- data.frame(n = seq(1, length(footnotes)), text = footnotes)
    footnotes$n <- strrep("*", footnotes$n)
    footnotes <- paste0(footnotes$n, " ", footnotes$text)

  }


  if (!is.null(sources)) {

    # Sense check inputs
    if (!is.vector(sources) || !is.character(sources))
      stop("sources must be a vector of strings.")

    # Construct the list of sources
    source_list <- paste(sources, collapse = "; ")
    sources <-
      paste0(ifelse(length(sources) > 1, "Sources: ", "Source: "), source_list)

    # Unlikely, but stops sources text from spilling over the RHS of graphs if
    # they are lengthy
    sources <-
      paste(strwrap(sources, width = footnote_max_width), collapse = "\n")

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
