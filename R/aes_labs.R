#' Resolve the final text for a title/subtitle/caption label: pass through
#' unchanged if already manually wrapped (tracked via the `*_wrap` attribute
#' set by labs_e61()), otherwise look up its rendered grob to get the actual
#' font size and rescale it to fit plot_width.
#' text_type - One of "title", "subtitle", "caption" - also the grob/label name.
#' @noRd
resolve_label_text <- function(plot, grobs, layout_names, text_type, plot_width){

  wrap_attr <- paste0(text_type, "_wrap")
  raw_text <- plot@labels[[text_type]]

  if(!is.null(attr(raw_text, wrap_attr))){
    return(raw_text)
  }

  grob <- grobs[[which(layout_names == text_type)]]

  if(is.null(grob$children) || length(grob$children) == 0){
    return(NULL)
  }

  font_size <- if(text_type == "subtitle"){
    tryCatch(grob$children[[1]]$children[[1]]$gp$fontsize, error = function(e) 10)
  } else {
    grob$children[[1]]$gp$fontsize
  }

  rescale_text(text = raw_text, text_type = text_type, font_size = font_size, plot_width = plot_width)
}

#' Get aesthetic titles, subtitles and footnotes.
#' plot - Plot object to adjust.
#' plot_width - Numeric. Width of the plot.
#' @noRd
update_labs <- function(plot, plot_width){

  p <- t61_ggplotGrob_quiet_na(plot)

  title_text <- resolve_label_text(plot, p$grobs, p$layout$name, "title", plot_width)

  # set the title to element blank if it is not required - otherwise it leaves a useless space
  if(is.null(title_text) || title_text == ""){
    plot <- plot + theme(plot.title = element_blank())
  }

  subtitle_text <- resolve_label_text(plot, p$grobs, p$layout$name, "subtitle", plot_width)

  # set the subtitle to element blank if it is not required - otherwise it leaves a useless space
  if(is.null(subtitle_text) || subtitle_text == ""){
    plot <- plot + theme(plot.subtitle = element_blank())
  }

  caption_text <- resolve_label_text(plot, p$grobs, p$layout$name, "caption", plot_width)

  # Update the x-axis label spacing if there is no x-axis label ----

  if(is.null(plot@labels$x) || plot@labels$x == ""){
    plot <- plot + theme(axis.title.x = element_blank())
  }

  # Only set the labels this function computes - re-setting every other
  # label (x/y/colour/fill/...) to itself is a no-op at best and drops any
  # label not listed at worst.
  plot_new <- plot +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      caption = caption_text
    )

  return(plot_new)
}

#' Format text based on font size, text type and plot width
#' text - The text to be rescaled (adding line breaks in the right places etc.)
#' text_type - Is the text for a title, subtitle or caption (footnotes and sources)
#' font_size - Numeric. Size of the font of the text.
#' plot_width - Numeric. Width of the plot.
#' @noRd
rescale_text <- function(text, text_type, font_size, plot_width){

  if(length(plot_width) == 0) stop("Plot width is length 0.")

  # algo for titles
  if(text_type == "title") {

    text <- rescale_title_text(text, font_size, plot_width)

    # algo for subtitles
  } else if (text_type == "subtitle") {

    parts <- attr(text, "t61_subtitle", exact = TRUE)

    # Subtitles built by labs_e61() carry their pieces with them, so re-wrap
    # those and render the markup again. A subtitle set any other way is
    # already whatever the user asked for, so leave it alone rather than
    # guessing at its structure.
    if (!is.null(parts)) {
      text <- render_subtitle_markup(rewrap_subtitle_parts(parts, font_size, plot_width))
    }

    # algo for footnotes
  } else if(text_type == "caption"){

    text <- rescale_caption_text(text, font_size, plot_width)
  }

  return(text)
}

#' Format text based on font size, text type and plot width for multi plots
#' text - The text to be rescaled (adding line breaks in the right places etc.)
#' text_type - Is the text for a title, subtitle or caption (footnotes and sources)
#' font_size - Numeric. Size of the font of the text.
#' plot_width - Numeric. Width of the plot.
#' @noRd
rescale_text_multi <- function(text, text_type, font_size, plot_width){

  if(length(plot_width) == 0) stop("Plot width is length 0.")

  # algo for titles
  if(text_type == "title") {

    text <- rescale_title_text(text, font_size, plot_width)

    # algo for titles
  } else if(text_type == "subtitle") {

      text <- stringr::str_replace_all(text, "\\\n", " ")

      text <- get_lines(text, font_size, plot_width)

      text <- paste(text$collapsed_text, collapse = "\n")

    # algo for footnotes
  } else if(text_type == "caption"){

    text <- rescale_caption_text(text, font_size, plot_width)
  }

  return(text)
}

#' Wrap a title string into font-fitted lines. Shared by rescale_text() and
#' rescale_text_multi() - the title algorithm is identical for both.
#' @noRd
rescale_title_text <- function(text, font_size, plot_width){
  text <- stringr::str_replace_all(text, "\\\n", " ")
  text <- get_lines(text, font_size, plot_width, font_face = 2)
  paste(text$collapsed_text, collapse = "\n")
}

#' Wrap `text` into lines that fit plot_width at the given font size, joined
#' by `collapse`. Any line breaks already in the text are treated as spaces
#' so the text is re-flowed from scratch.
#' @noRd
wrap_to_width <- function(text, font_size, plot_width, collapse = "<br>"){

  text <- stringr::str_replace_all(text, "[\r\n]", " ")

  paste(get_lines(text, font_size, plot_width)$collapsed_text, collapse = collapse)
}

#' Re-wrap the structured subtitle parts (see render_subtitle_markup()) to the
#' final plot width. Text the user wrapped explicitly, via subtitle_wrap or
#' ytitle_wrap, is left exactly as they wrapped it.
#' @noRd
rewrap_subtitle_parts <- function(parts, font_size, plot_width){

  has_ytitle <- !is.null(parts$ytitle)

  # With a subtitle above it, the y-axis title is the smaller of the two
  # sizes in the row, and font_size is the measured size of the larger one.
  # On its own it is rendered alone, so font_size is already its own size.
  ytitle_font_size <-
    if (has_ytitle && parts$subtitle != "") font_size * 0.9 else font_size

  # "<br>" is a line break in this row, so re-flow across it rather than
  # leaving it glued to a word
  reflow <- function(text) gsub("<br>", " ", text, fixed = TRUE)

  if (!isTRUE(parts$subtitle_wrapped)) {
    parts$subtitle <- wrap_to_width(reflow(parts$subtitle), font_size, plot_width)
  }

  if (has_ytitle && !isTRUE(parts$ytitle_wrapped)) {
    parts$ytitle <- wrap_to_width(reflow(parts$ytitle), ytitle_font_size, plot_width)
  }

  parts
}

#' Wrap the structured footnotes/sources (see caption_wrap()) to plot_width
#' and assemble the caption text.
#' @noRd
rescale_caption_parts <- function(parts, font_size, plot_width){

  footnote_text <- NULL

  # Numbering happens here, after blank footnotes are dropped, so the
  # asterisks always run *, **, *** without gaps
  footnotes <- number_footnotes(parts$footnotes %||% character(0))

  if (length(footnotes) > 0) {
    footnote_text <- paste(
      vapply(footnotes, wrap_to_width, character(1), USE.NAMES = FALSE,
             font_size = font_size, plot_width = plot_width, collapse = "\n"),
      collapse = "\n"
    )
  }

  source_text <- NULL

  if (length(parts$sources) > 0) {
    source_text <-
      wrap_to_width(format_sources(parts$sources), font_size, plot_width, collapse = "\n")
  }

  text <- paste0(c(footnote_text, source_text), collapse = "\n")

  if (text == "") NULL else text
}

#' Wrap a caption to plot_width. Captions built by caption_wrap() carry their
#' footnotes and sources with them, so those are re-wrapped from the structured
#' values. A caption set any other way is wrapped as-is, one line at a time, so
#' its own line structure is kept. Shared by rescale_text() and
#' rescale_text_multi() - the caption algorithm is identical for both.
#' @noRd
rescale_caption_text <- function(text, font_size, plot_width){

  parts <- attr(text, "t61_caption", exact = TRUE)

  if (!is.null(parts)) {
    return(rescale_caption_parts(parts, font_size, plot_width))
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  paste(
    vapply(lines, wrap_to_width, character(1), USE.NAMES = FALSE,
           font_size = font_size, plot_width = plot_width, collapse = "\n"),
    collapse = "\n"
  )
}


#' Calculate break text up into aesthetically sized lines
#' text - String. Text to be measured.
#' font_size - Numeric. Size of the font of the text.
#' plot_width - Numeric. Width of the plot.
#' font_face - Numeric. Face of the font (1 = normal, 2 = bold)
#' @noRd
get_lines <- function(text, font_size, plot_width, font_face = 1){

  # split text into words and calculate the length of each word. Word widths
  # include a trailing space, but the last word on a line never renders one
  # (nothing follows it), so space_width is subtracted from each line's
  # running total below to match what's actually drawn. Measuring the words
  # and the lone space together in one call keeps this to a single
  # throwaway measurement device instead of two.
  words <- split_text_into_words(text)
  n <- nrow(words)

  widths <- get_text_width(c(paste0(words$word, " "), " "), font_size, font_face)
  words[, word_width := utils::head(widths, n)]
  space_width <- utils::tail(widths, 1)

  # Running total from the very start of the text - a line's own cumulative
  # width is this minus its running total as of the previous line's end
  # (`offset`), computed once rather than re-summed from scratch per line.
  cum <- cumsum(words$word_width)

  line <- integer(n)
  line_no <- 1L
  start <- 1L
  offset <- 0

  for (i in seq_len(n)) {

    local_width <- (cum[i] - offset - space_width) / plot_width

    # start a new line, unless this is the first word on the line (always
    # keep at least one word per line, even if it alone exceeds plot_width)
    if (local_width > 1 && i > start) {
      line_no <- line_no + 1L
      start <- i
      offset <- cum[i - 1L]
    }

    line[i] <- line_no
  }

  words[, line := line]

  # combine lines
  words[, .(collapsed_text = paste(word, collapse = " ")), by = line]
}

#' Calculate the width of text in ggplot titles, subtitles and footnotes
#'
#' Measured on a throwaway svglite device using the real render font, rather
#' than base R's built-in metric tables, which don't know about "pt-sans"
#' and measure it inaccurately.
#' text - String. Text to be measured.
#' font_size - Numeric. Size of the font of the text.
#' font_face - Numeric. Face of the font (1 = normal, 2 = bold)
#' @noRd
get_text_width <- function(text, font_size = 10, font_face = 1) {

  if (length(text) == 0) return(numeric(0))

  family <- "pt-sans"
  face <- if (font_face == 2) "bold" else "plain"

  measure_device({
    grid::pushViewport(grid::viewport(
      gp = grid::gpar(fontfamily = family, fontsize = font_size, fontface = face)
    ))

    ret <- grid::convertWidth(grid::stringWidth(text), "cm", valueOnly = TRUE)

    grid::popViewport()

    ret
  })
}

#' Calculate the height of text in ggplot titles, subtitles and footnotes
#'
#' See get_text_width() for why this measures using a real graphics device
#' rather than base R's built-in font metric tables.
#' text - String. Text to be measured.
#' font_size - Numeric. Size of the font of the text.
#' @noRd
get_text_height <- function(text, font_size = 10) {

  if (length(text) == 0) return(numeric(0))

  family <- "pt-sans"

  measure_device({
    grid::pushViewport(grid::viewport(
      gp = grid::gpar(fontfamily = family, fontsize = font_size)
    ))

    ret <- grid::convertHeight(grid::stringHeight(text), "cm", valueOnly = TRUE)

    grid::popViewport()

    ret
  })
}

#' Run `expr` on a throwaway svglite device, so text is measured with real
#' font metrics rather than base R's built-in tables.
#' @noRd
measure_device <- function(expr) {

  measure_file <- tempfile(fileext = ".svg")
  on.exit(unlink(measure_file), add = TRUE)

  device <- t61_open_device(measure_file, width = 10, height = 10)
  on.exit(t61_release_device(device), add = TRUE)

  expr
}

#' Split a character string into its individual words
#' text - Text to be split into individual words.
#' @noRd
split_text_into_words <- function(text) {

  words <- strsplit(text, "\\s+")[[1]]

  # if we have been pased an empty string, return an empty string
  if(length(words) == 0){

    data.table::data.table(word = "", text = text)

  } else {
    data.table::data.table(word = words, text = text)
  }
}

#' Update the size of plot labels
#' @noRd
update_plot_label <- function(plot, chart_type, base_size){

  for (i in seq_along(plot@layers)){

    # 1 - check whether it has geom_text or geom_label arguments (this is what plot labels are)
    layer_class <- class(plot@layers[[i]]$geom)

    if("GeomText" %in% layer_class || "GeomLabel" %in% layer_class){

      # 2 - check whether it is an plot_label that can be adjusted
      label <- plot@layers[[i]]$aes_params$label

      # 3 - check that it has the adjustment attribute (set on the layer
      # object itself by .build_plot_label_layer(), not on the label mapping)
      if(!is.null(attr(plot@layers[[i]], "adj_plot_label"))){

        # 4 - update the size - this will depend on the chart width and base text size
        plot@layers[[i]]$aes_params$size <- 3.5 * base_size / 10
      }
    }
  }

  return(plot)
}

#' Update plot margins when new base size is provided
#'
#' Elements the current theme has already blanked are left out - a margin
#' does nothing for an element that won't render, and setting one would
#' silently undo the user's theme() call.
#' @noRd
update_margins <- function(current_theme, base_size, legend_title) {

  half_line <- base_size / 2

  margin_args <- list(
    axis.text.x = element_text(margin = margin(t = base_size / 4, unit = "pt")),
    axis.text.x.top = element_text(margin = margin(b = base_size / 5)),
    axis.text.y = element_text(margin = margin(r = base_size / 5)),
    axis.text.y.right = element_text(margin = margin(l = base_size / 5)),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.ticks.length.x = unit(half_line / 2, "pt"), # Puts ticks inside graph
    axis.title.x = element_text(margin = margin(t = half_line / 2)),
    axis.title.x.top = element_text(margin = margin(b = half_line / 2)),
    axis.title.y = element_text(margin = margin(r = half_line / 2)),
    axis.title.y.right = element_text(margin = margin(l = half_line / 2)),
    legend.spacing = unit(half_line, "pt"),
    legend.margin = margin(),
    legend.text = element_text(margin = margin(l = 0, r = base_size / 4, unit = "pt")),
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.spacing = unit(half_line, "pt"),
    strip.text = element_text(
      margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),
    plot.title = element_text(margin = margin(b = half_line)),
    plot.subtitle = ggtext::element_markdown(
      margin = margin(
        t = 0, r = 0, b = base_size * .5, l = 0,
        unit = "pt"
      )
    )
  )

  is_blanked <- vapply(names(margin_args), function(el) {
    inherits(current_theme[[el]], "element_blank")
  }, logical(1))

  ret <- do.call(theme, margin_args[!is_blanked])

  # adjust borders to the legend title if there is one
  if (!"element_blank" %in% class(legend_title)) {
    ret <- ret %+replace%
      theme(legend.title = element_text(size = rel(1),
                                        margin = margin(l = 0,
                                                        r = base_size / 4, unit = "pt")))
  }

  return(ret)
}
