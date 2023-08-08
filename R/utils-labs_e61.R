#' Label text wrapper
#'
#' This is an internal function that supplies the functionality to wrap label
#' text to ensure it does not run beyond the boundaries of the graph. This
#' function is used in functions that accept and produce graph header and footer
#' text.
#'
#' The default values of the function arguments should be set in the parent
#' function rather than here.
#'
#' @noRd
label_wrap <- function(
    title = NULL,
    subtitle = NULL,
    footnotes = NULL,
    sources = NULL,
    title_max_char = NULL,
    subtitle_max_char = NULL,
    footnote_max_char = NULL,
    title_wrap = NULL,
    subtitle_wrap = NULL,
    footnote_wrap = NULL
    ) {

  # Title
  if (!is.null(title)) {

    if (!is.character(title)) stop("title must be a string.")

    if (title_wrap) {
      title <- paste(strwrap(title, width = title_max_char), collapse = "\n")
    }

  }

  # Subtitle
  if (!is.null(subtitle)) {

    if (!is.character(subtitle))
      stop("subtitle must be a string.")

    if (subtitle_wrap) {
      subtitle <- paste(strwrap(subtitle, width = subtitle_max_char), collapse = "\n")
    }
  }

  # Footnotes
  if (!is.null(footnotes)) {

    # Sense check inputs
    if (!is.vector(footnotes) || !is.character(footnotes))
      stop("footnotes must be a vector of strings.")

    # Stops footnote text from spilling over the RHS of graphs if they are lengthy
    if (footnote_wrap) {
      footnotes <-
        sapply(footnotes, function(x)
          paste(strwrap(x, width = footnote_max_char), collapse = "\n"))
    }

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

    # Stops sources text from spilling over the RHS of graphs if they are
    # lengthy
    if (footnote_wrap) {
      sources <-
        paste(strwrap(sources, width = footnote_max_char), collapse = "\n")
    }

  }

  # Put the footer text together
  caption <- paste0(c(footnotes, sources), collapse = "\n")
  if (caption == "") caption <- NULL # Return NULL caption if blank


  # Return elements in a list
  retval <- list(title = title, subtitle = subtitle, caption = caption)

  return(retval)

}
