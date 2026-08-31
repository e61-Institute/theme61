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
#'   transforming the `footnotes` and `sources` arguments into nicely formatted
#'   text that goes into the `caption` argument in ggplot2's `labs()` function.
#'   Thus, if you are using `footnotes` or `sources`, do not supply a `caption`
#'   argument as well.
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
#' @param y_top Logical. If `TRUE` (default), the y-axis title is placed
#'   underneath the subtitle. If `FALSE`, the y-axis label remains on the side
#'   of the graph.
#' @param title_wrap,subtitle_wrap,footnote_wrap Numeric or logical. Set the
#'   maximum number of characters per line in the title, subtitle and footer
#'   text. Set to `FALSE` if you want to turn off text wrapping. The default is
#'   usually appropriate for the default graph dimensions in [save_e61].
#' @param ... Additional optional arguments passed to [labs][ggplot2::labs].
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
                     ytitle_wrap = NULL,
                     x = NULL,
                     y = NULL,
                     y_top = TRUE,
                     ...
) {

  # check various titles are strings
  str_chk <- list(title, subtitle, footnotes, sources, x, y)

  for (i in str_chk) {
    if (!is.null(i) && !is.character(i)) stop(i, " must be a string.")
  }

  # theme61.iterate_mode: theme_e61() isn't applied automatically in this
  # mode, so the ggtext::element_markdown() that normally renders the HTML/
  # markdown tags below isn't either - skip generating them so the graph
  # doesn't show literal "<span>"/"<br>" text in the subtitle/y-axis title.
  iterate_mode <- isTRUE(getOption("theme61.iterate_mode", FALSE))

  # Turn off text wrapping if FALSE is the argument
  if (isFALSE(title_wrap)) title_wrap <- 9999
  if (isFALSE(subtitle_wrap)) subtitle_wrap <- 9999
  if (isFALSE(footnote_wrap)) footnote_wrap <- 9999
  if (isFALSE(ytitle_wrap)) ytitle_wrap <- 9999

  # Title ----
  # Keep title as NULL so ggplot2 does not reserve space for an empty title
  # grob (an empty string title still takes up vertical space)
  if (is.null(title)) {
    title_text <- NULL
    wrap_title_trk <- FALSE
  } else {
    w <- resolve_wrap(title, title_wrap, "title", default_width = 120)
    title_text <- w$text
    wrap_title_trk <- w$wrapped
  }

  # Subtitle ----
  w <- resolve_wrap(subtitle, subtitle_wrap, "subtitle", default_width = 120)
  subtitle_text <- w$text
  wrap_subtitle_trk <- w$wrapped

  # Y-axis title ----
  # In iterate_mode, the y-axis title below won't be merged into the
  # (markdown-rendered) subtitle, so join with a plain newline instead of
  # "<br>" - it stays a normal, non-markdown axis title.
  wrap_ytitle_trk <- FALSE
  if (y_top && !is.null(ytitle_wrap)) {
    w <- resolve_wrap(y, ytitle_wrap, "ytitle", collapse = if (iterate_mode) "\n" else "<br>")
    y <- w$text
    wrap_ytitle_trk <- w$wrapped
  }

  # Footnotes ----
  validate_wrap(footnote_wrap, "footnote")
  caption_text <- caption_wrap(footnotes, sources, max_char = footnote_wrap %||% 120)
  wrap_caption_trk <- !is.null(footnote_wrap)

  if(wrap_title_trk) attr(title_text, "title_wrap") <- TRUE
  if(wrap_subtitle_trk) attr(subtitle_text, "subtitle_wrap") <- TRUE
  if(wrap_ytitle_trk) attr(y, "ytitle_wrap") <- TRUE
  if(wrap_caption_trk) attr(caption_text, "caption_wrap") <- TRUE

  # Add the y-axis text once the subtitle has been processed
  primary_size <- getOption("theme61.base_size", default = 10) * 1
  secondary_size <- getOption("theme61.base_size", default = 10) * 0.9

  # Set y = "" to NULL because it just breaks code later
  if (!is.null(y) && y == "") y <- NULL

  # Skip the HTML/markdown subtitle styling in iterate_mode - it relies on
  # ggtext::element_markdown(), which theme_e61() isn't applying. Leave
  # subtitle_text as plain text and y as a normal y-axis title instead
  # (rendered wherever ggplot2 would normally put it).
  if (!iterate_mode) {
    if (y_top) {

      if (is.null(y)) {
        subtitle_text <- glue::glue(
          "<span style='font-size:{primary_size}pt'>{subtitle_text}</span>"
        )
      } else if (subtitle_text == "") {
        # No subtitle: show the y-axis title on its own line rather than
        # prefixing it with an empty line, which would still reserve a
        # blank line's worth of height above it.
        subtitle_text <- glue::glue(
          "<span style='font-size:{secondary_size}pt'>{y}</span>"
        )

        y <- NULL
      } else {
        subtitle_text <- glue::glue(
          "<span style='font-size:{primary_size}pt'>{subtitle_text}</span><br><span style='font-size:{secondary_size}pt'>{y}</span>"
        )

        y <- NULL
      }
    } else {
      subtitle_text <- glue::glue("<span style='font-size:{primary_size}pt'>{subtitle_text}</span>")
    }
  }

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

#' Validate a `*_wrap` argument: must be NULL or a non-negative number.
#' Shared by all four wrap arguments in labs_e61() (title_wrap, subtitle_wrap,
#' ytitle_wrap, footnote_wrap).
#' @noRd
validate_wrap <- function(wrap, label) {
  if (!is.null(wrap) && (!is.numeric(wrap) || wrap < 0)) {
    stop(label, "_wrap must be a positive integer.")
  }
  invisible(wrap)
}

#' Validate and apply a `*_wrap` argument to `text` via strwrap(), falling
#' back to `default_width` if `wrap` is NULL (or leaving `text` untouched if
#' there's no default either). Returns the (possibly wrapped) text and
#' whether an explicit wrap was applied - used to set labs_e61()'s `*_wrap`
#' tracking attributes downstream. Shared by the title/subtitle/y-axis-title
#' blocks in labs_e61().
#' @noRd
resolve_wrap <- function(text, wrap, label, default_width = NULL, collapse = "\n") {

  validate_wrap(wrap, label)

  width <- if (is.null(wrap)) default_width else wrap

  if (is.null(width)) return(list(text = text, wrapped = FALSE))

  list(text = paste(strwrap(text, width = width), collapse = collapse), wrapped = !is.null(wrap))
}
