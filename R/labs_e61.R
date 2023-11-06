#' Add graph titles and footers in the e61 style
#'
#' Provides support for well-formatted titles and footer text with minimal user
#' adjustment needed.
#'
#' You should use vectors in the footnotes and sources to take advantage of the
#' formatting features of this function.
#'
#' The arguments in the function allow you to make changes to the text
#' formatting if required.
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
#' @param title_wrap,subtitle_wrap,footnote_wrap Numeric or
#'   logical. Set the maximum number of characters per line in the title,
#'   subtitle and footer text. Set to \code{FALSE} if you want to turn off text
#'   wrapping. The default is usually appropriate for the default graph
#'   dimensions in \link{save_e61}.
#' @param ... Additional optional arguments passed to \link[ggplot2]{labs}.
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
                     title_wrap = NULL,
                     subtitle_wrap = NULL,
                     footnote_wrap = NULL,
                     x = NULL,
                     y = ggplot2::waiver(),
                     ...
                     ) {

  # Deprecation code
  lifecycle::deprecate_stop(when = "0.6.0",
                            what = "scale_colour_e61(title_max_char)",
                            details = c("!" = "Please remove it from your function call."))
  lifecycle::deprecate_stop(when = "0.6.0",
                            what = "scale_colour_e61(subtitle_max_char)",
                            details = c("!" = "Please remove it from your function call."))
  lifecycle::deprecate_stop(when = "0.6.0",
                            what = "scale_colour_e61(footnote_max_char)",
                            details = c("!" = "Please remove it from your function call."))


  # check the title and subtitle are strings
  sapply(list(title, subtitle), function(x){
    if (!is.null(x) && !is.character(x)){
      stop("title and subtitle must be a string.")
    }
  })

  # Track whether a label has been wrapped
  wrap_title_trk <- FALSE
  wrap_subtitle_trk <- FALSE
  wrap_caption_trk <- FALSE

  # Turn off text wrapping if FALSE is the argument
  if (isFALSE(title_wrap)) title_wrap <- 9999
  if (isFALSE(subtitle_wrap)) subtitle_wrap <- 9999
  if (isFALSE(footnote_wrap)) footnote_wrap <- 9999

  # For each label check whether to wrap the title
  if(!is.null(title_wrap)){

    # Check the title and title_wrap have been correctly supplied
    if (!is.numeric(title_wrap) || title_wrap < 0){
      stop("title_wrap must be a positive integer.")
    }

    # Wrap the title text
    title_text <- paste(strwrap(title, width = title_wrap), collapse = "\n")

    wrap_title_trk <- TRUE

  } else {
    title_text <- paste(strwrap(title, width = 120), collapse = "\n")
  }

  if(!is.null(subtitle_wrap)){

    # Check the subtitle and subtitle_wrap have been correctly supplied
    if (!is.numeric(subtitle_wrap) || subtitle_wrap < 0){
      stop("subtitle_wrap must be a positive integer.")
    }

    if (!is.null(subtitle) && !is.character(subtitle)){
      stop("subtitle must be a string.")
    }

    # Wrap the subtitle text
    subtitle_text <- paste(strwrap(subtitle, width = subtitle_wrap), collapse = "\n")

    wrap_subtitle_trk <- TRUE

  } else {
    subtitle_text <- paste(strwrap(subtitle, width = 120), collapse = "\n")
  }

  if(!is.null(footnote_wrap)){

    # Check the subtitle and subtitle_wrap have been correctly supplied
    if (!is.numeric(footnote_wrap) || footnote_wrap < 0){
      stop("footnote_wrap must be a positive integer.")
    }

    # Wrap the subtitle text
    caption_text <- caption_wrap(footnotes, sources, max_char = footnote_wrap)
    wrap_caption_trk <- TRUE

  } else {
    caption_text <- caption_wrap(footnotes, sources, max_char = 120)
  }

  if(wrap_title_trk) attr(title_text, "title_wrap") <- TRUE
  if(wrap_subtitle_trk) attr(subtitle_text, "subtitle_wrap") <- TRUE
  if(wrap_caption_trk) attr(caption_text, "caption_wrap") <- TRUE

  # add to a ggplot object and return
  label <-
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      caption = caption_text,
      x = x,
      y = y,
      ...
    )

  return(label)
}

#' Caption text wrapper
#'
#' This is an internal function that supplies the functionality to wrap title
#' text manually.
#'
#' @noRd
caption_wrap <- function(
    footnotes = NULL,
    sources = NULL,
    max_char = 120,
    caption_wrap = TRUE
  ){

  # Footnotes
  if (!is.null(footnotes)) {

    # Sense check inputs
    if (!is.vector(footnotes) || !is.character(footnotes))
      stop("footnotes must be a vector of strings.")

    # Stops footnote text from spilling over the RHS of graphs if they are lengthy
    if(caption_wrap){
      footnotes <-
        sapply(
          footnotes,
          function(x) paste(strwrap(x, width = max_char), collapse = "\n")
        )
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
    if(caption_wrap){
      sources <- paste(strwrap(sources, width = max_char), collapse = "\n")
    }
  }

  # Put the footer text together
  caption <- paste0(c(footnotes, sources), collapse = "\n")
  if (caption == "") caption <- NULL # Return NULL caption if blank

  return(caption)
}
