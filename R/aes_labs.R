#' Get aesthetic titles, subtitles and footnotes.
#' plot - Plot object to adjust.
#' plot_width - Numeric. Width of the plot.
#' @noRd
update_labs <- function(plot, plot_width){

  p <- ggplotGrob(plot)

  # Title ----

  # First check whether the title has already been manually wrapped
  if(is.null(attr(plot@labels$title, "title_wrap"))){

    # check if the title exists
    title_grob <- p$grobs[[which(p$layout$name == "title")]]
    title_text <- NULL

    if(!is.null(title_grob$children)){
      title_size <- title_grob$children[[1]]$gp$fontsize

      title_text <-
        rescale_text(
          text = plot@labels$title,
          text_type = "title",
          font_size = title_size,
          plot_width = plot_width
        )
    }
  } else {
    title_text <- plot@labels$title
  }

  # set the title to element blank if it is not required - otherwise it leaves a useless space
  if(is.null(title_text) || title_text == ""){
    plot <- plot + theme(plot.title = element_blank())
  }

  # Subtitle ----
  if(is.null(attr(plot@labels$subtitle, "subtitle_wrap"))){
    subtitle_grob <- p$grobs[[which(p$layout$name == "subtitle")]]
    subtitle_text <- NULL

    if(!is.null(subtitle_grob$children) && length(subtitle_grob$children) > 0){
      subtitle_size <- tryCatch(
        subtitle_grob$children[[1]]$children[[1]]$gp$fontsize,
        error = function(e) 10
      )

      subtitle_text <-
        rescale_text(
          text = plot@labels$subtitle,
          text_type = "subtitle",
          font_size = subtitle_size,
          plot_width = plot_width
        )
    }
  } else {
    subtitle_text <- plot@labels$subtitle
  }

  # set the subtitle to element blank if it is not required - otherwise it leaves a useless space
  if(is.null(subtitle_text) || subtitle_text == ""){
    plot <- plot + theme(plot.subtitle = element_blank())
  }

  # Footnotes ----

  if(is.null(attr(plot@labels$caption, "caption_wrap"))){
    footnote_grob <- p$grobs[[which(p$layout$name == "caption")]]
    caption_text <- NULL

    if(!is.null(footnote_grob$children)){

      footnote_size <- footnote_grob$children[[1]]$gp$fontsize
      caption_text <-
        rescale_text(
          text = plot@labels$caption,
          text_type = "caption",
          font_size = footnote_size,
          plot_width = plot_width
        )
    }
  } else {
    caption_text <- plot@labels$caption
  }

  # Update the x-axis label spacing if there is no x-axis label ----

  if(is.null(plot@labels$x) || plot@labels$x == ""){
    plot <- plot + theme(axis.title.x = element_blank())
  }

  # add a new labs function to override the old one
  plot_new <- plot +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      caption = caption_text,
      x = plot@labels$x,
      y = plot@labels$y,
      colour = plot@labels$colour,
      fill = plot@labels$fill
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

    text <- stringr::str_replace_all(text, "\\\n", " ")

    text <- get_lines(text, font_size, plot_width, font_face = 2)

    text <- paste(text$collapsed_text, collapse = "\n")

    # algo for subtitles
  } else if (text_type == "subtitle") {

    # Check if the y-axis title is present
    has_y_title <- if (stringr::str_detect(text, ".*<br>.*")) TRUE else FALSE

    # Strip the HTML elements and split the subtitle from the y-axis title
    if (has_y_title) {
      regex_in <- "(<.*>)(.*)<\\/span><br>(<.*>)(.*)<\\/span>"
      regex_out <- "\\1___\\2___\\3___\\4"

    } else {
      regex_in <- "(<.*>)(.*)<\\/span>"
      regex_out <- "\\1___\\2"
    }

    sub_list <- gsub(regex_in, regex_out, text) |>
      strsplit("___", fixed = T) |> unlist()

    if (length(sub_list) > 1 && has_y_title) {
      sub_text <- sub_list[[2]]
      y_text <- sub_list[[4]]

    } else if (length(sub_list) > 1 && !has_y_title) {
      sub_text <- sub_list[[2]]

    } else if (length(sub_list) == 1) {
      sub_text <- ""
    }

    ## Parse the subtitle text
    sub_text <- stringr::str_replace_all(sub_text, "\\\n", " ")

    sub_text <- get_lines(sub_text, font_size, plot_width)

    sub_text <- paste(sub_text$collapsed_text, collapse = "<br>")

    ## Parse the y-axis title text
    if (has_y_title) {

      y_text <- stringr::str_replace_all(y_text, "\\\n", " ")

      # Note we need to scale down the font size for y-axis titles as it is part
      # of the subtitle text which has a larger font size
      y_text <- get_lines(y_text, font_size * 0.9, plot_width)

      y_text <- paste(y_text$collapsed_text, collapse = "<br>")
    }

    ## Recombine them and restore the HTML
    if (sub_text != "" && has_y_title) {
      text <- paste0(sub_list[[1]], sub_text,
                     "</span><br>",
                     sub_list[[3]], y_text,
                     "</span>")

    } else if (sub_text != "" && !has_y_title) {
      text <- paste0(sub_list[[1]], sub_text, "</span>")

    } else if (sub_text == "" && has_y_title) {
      text <- paste0(sub_list[[3]], y_text, "</span>")

    }

    # algo for footnotes
  } else if(text_type == "caption"){

    footnote_text <- stringr::str_replace_all(text, "\\\n\\*", " new_footnote\\*")
    footnote_text <- stringr::str_replace_all(footnote_text, "\\\n", " ")
    footnote_text <- stringr::str_remove(footnote_text, pattern = "^\\* ")

    sources <-
      stringr::str_extract(footnote_text, "(?<=Sources{0,1}\\:).*$") |>
      stringr::str_split(";") |>
      unlist() |>
      stringr::str_squish()

    # remove sources - if we have them
    if(stringr::str_detect(footnote_text, "Source")){
      footnote_text <- stringr::str_extract(footnote_text, "^.*(?=Source.*:.+)")

    } else {
      footnote_text <- footnote_text
    }

    # split footnotes up if there are multiple and drop those with length 0
    footnote_text <- stringr::str_split(footnote_text, "new_footnote\\*+\\s*")

    footnote_text <- lapply(footnote_text, stringr::str_remove_all, pattern = "new_footnote")

    text_lengths <- lapply(footnote_text, get_text_width, font_size = font_size)

    footnote_data <- data.table::data.table(footnote_text = unlist(footnote_text), text_width = unlist(text_lengths))

    footnote_data <- footnote_data |>
      _[text_width != 0] |>
      _[, footnote_text := stringr::str_replace_all(footnote_text, "[\r\n]" , " ")]

    # number footnotes and then split into words
    footnote_data[, footnote_num := 1:.N]

    if(nrow(footnote_data) > 0){

      # split into words to calculate line lengths
      text_lines <- list()

      for(i in 1:nrow(footnote_data)){

        # Get lines and make sure to add the *s
        text_lines[[i]] <- get_lines(
          paste(strrep("*", i), footnote_data$footnote_text[i]),
          font_size,
          plot_width
        )

        text_lines[[i]][, footnote_num := i]
      }

      text_lines <- data.table::rbindlist(text_lines)

      # combine text into a caption along with the sources
      footnote_data <-
        text_lines[, .(footnote = paste(collapsed_text, collapse = "\n")), by = footnote_num]

      footnote_data <- footnote_data[, .(footnotes = paste(footnote, collapse = "\n"))]

      footnote_text <- footnote_data$footnotes[1]

      # Otherwise we didn't have any footnotes to begin with, so set as an empty string
    } else {
      footnote_text <- NULL
    }

    # Check whether we have sources to add and how many
    if(any(is.na(sources)) || is.null(sources)){
      if(is.null(footnote_text)){
        text <- NULL

      } else {
        text <- footnote_text
      }

      # we have sources - check how many
    } else {

      # Add the sources label and collapse
      if(length(sources) > 1) {
        sources <- paste0(sources, collapse = "; ")

        sources <- paste0("Sources: ", sources)

      } else if(length(sources) == 1){
        sources <- paste0(sources, collapse = "; ")

        sources <- paste0("Source: ", sources)
      }

      # Make sure the sources don't extend over the width of the plot
      sources <- get_lines(sources, font_size, plot_width)

      sources <- paste0(sources$collapsed_text, collapse = "\n")

      # Add the rest of the footnote text
      if(is.null(footnote_text)){
        text <- sources

      } else {
        text <- paste0(footnote_text, "\n", sources)
      }

    }
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

    text <- stringr::str_replace_all(text, "\\\n", " ")

    text <- get_lines(text, font_size, plot_width, font_face = 2) # set font face to bold

    text <- paste(text$collapsed_text, collapse = "\n")

    # algo for titles
  } else if(text_type == "subtitle") {

      text <- stringr::str_replace_all(text, "\\\n", " ")

      text <- get_lines(text, font_size, plot_width)

      text <- paste(text$collapsed_text, collapse = "\n")

    # algo for footnotes
  } else if(text_type == "caption"){

    footnote_text <- stringr::str_replace_all(text, "\\\n\\*", " new_footnote\\*")
    footnote_text <- stringr::str_replace_all(footnote_text, "\\\n", " ")
    footnote_text <- stringr::str_remove(footnote_text, pattern = "^\\* ")

    sources <-
      stringr::str_extract(footnote_text, "(?<=Sources{0,1}\\:).*$") |>
      stringr::str_split(";") |>
      unlist() |>
      stringr::str_squish()

    # remove sources - if we have them
    if(stringr::str_detect(footnote_text, "Source")){
      footnote_text <- stringr::str_extract(footnote_text, "^.*(?=Source.*:.+)")

    } else {
      footnote_text <- footnote_text
    }

    # split footnotes up if there are multiple and drop those with length 0
    footnote_text <- stringr::str_split(footnote_text, "new_footnote\\*+\\s*")

    footnote_text <- lapply(footnote_text, stringr::str_remove_all, pattern = "new_footnote")

    text_lengths <- lapply(footnote_text, get_text_width, font_size = font_size)

    footnote_data <- data.table::data.table(footnote_text = unlist(footnote_text), text_width = unlist(text_lengths))

    footnote_data <- footnote_data |>
      _[text_width != 0] |>
      _[, footnote_text := stringr::str_replace_all(footnote_text, "[\r\n]" , " ")]

    # number footnotes and then split into words
    footnote_data[, footnote_num := 1:.N]

    if(nrow(footnote_data) > 0){

      # split into words to calculate line lengths
      text_lines <- list()

      for(i in 1:nrow(footnote_data)){

        # Get lines and make sure to add the *s
        text_lines[[i]] <- get_lines(
          paste(strrep("*", i), footnote_data$footnote_text[i]),
          font_size,
          plot_width
        )

        text_lines[[i]][, footnote_num := i]
      }

      text_lines <- data.table::rbindlist(text_lines)

      # combine text into a caption along with the sources
      footnote_data <-
        text_lines[, .(footnote = paste(collapsed_text, collapse = "\n")), by = footnote_num]

      footnote_data <- footnote_data[, .(footnotes = paste(footnote, collapse = "\n"))]

      footnote_text <- footnote_data$footnotes[1]

      # Otherwise we didn't have any footnotes to begin with, so set as an empty string
    } else {
      footnote_text <- NULL
    }

    # Check whether we have sources to add and how many
    if(any(is.na(sources)) || is.null(sources)){
      if(is.null(footnote_text)){
        text <- NULL

      } else {
        text <- footnote_text
      }

      # we have sources - check how many
    } else {

      # Add the sources label and collapse
      if(length(sources) > 1) {
        sources <- paste0(sources, collapse = "; ")

        sources <- paste0("Sources: ", sources)

      } else if(length(sources) == 1){
        sources <- paste0(sources, collapse = "; ")

        sources <- paste0("Source: ", sources)
      }

      # Make sure the sources don't extend over the width of the plot
      sources <- get_lines(sources, font_size, plot_width)

      sources <- paste0(sources$collapsed_text, collapse = "\n")

      # Add the rest of the footnote text
      if(is.null(footnote_text)){
        text <- sources

      } else {
        text <- paste0(footnote_text, "\n", sources)
      }

    }
  }

  return(text)
}


#' Calculate break text up into aesthetically sized lines
#' text - String. Text to be measured.
#' font_size - Numeric. Size of the font of the text.
#' plot_width - Numeric. Width of the plot.
#' font_face - Numeric. Face of the font (1 = normal, 2 = bold)
#' @noRd
get_lines <- function(text, font_size, plot_width, font_face = 1){

  # split text into words and calculate the length of each word
  words <- split_text_into_words(text)
  words[, word_width := get_text_width(paste0(word, " "), font_size, font_face)]

  # word_width includes a trailing space, but the last word on a line never
  # renders one (nothing follows it), so subtract one space width from the
  # running total below to match what's actually drawn.
  space_width <- get_text_width(" ", font_size, font_face)

  # assign words to different lines based on the cumulative length
  words[, cumsum_word_width := (cumsum(word_width) - space_width) / plot_width]

  check_lines <- T
  i <- 1
  text_lines <- list()

  while(check_lines){

    # check whether we can create a line (i.e. some words are under the limit), otherwise take the first word and try again
    temp_line <- words[cumsum_word_width <= 1]

    if(nrow(temp_line) == 0){

      text_lines[[i]] <- words[1][, line := i]

      words <- words[-1]

    } else {
      text_lines[[i]] <- words[cumsum_word_width <= 1][, line := i]

      words <- words[cumsum_word_width > 1]

    }

    words <- words[, cumsum_word_width := (cumsum(word_width) - space_width) / plot_width]

    i <- i + 1

    if(nrow(words) == 0) break
  }

  text_lines <- data.table::rbindlist(text_lines)

  # combine lines
  text_lines <- text_lines[, .(collapsed_text = paste(word, collapse = " ")), by = line]

  return(text_lines)
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

  family <- if (is_testing()) "sans" else "pt-sans"
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

  family <- if (is_testing()) "sans" else "pt-sans"

  measure_device({
    grid::pushViewport(grid::viewport(
      gp = grid::gpar(fontfamily = family, fontsize = font_size)
    ))

    ret <- grid::convertHeight(grid::stringHeight(text), "cm", valueOnly = TRUE)

    grid::popViewport()

    ret
  })
}

#' Run `expr` with a throwaway svglite device as the active graphics device,
#' so text can be measured with real font metrics. The device is opened and
#' closed around `expr`, which restores whatever device (if any) was active
#' beforehand as a side effect of `dev.off()`, so this never disturbs a
#' device the user has open.
#' @noRd
measure_device <- function(expr) {

  measure_file <- tempfile(fileext = ".svg")
  on.exit(unlink(measure_file))

  svglite::svglite(measure_file, width = 10, height = 10)
  on.exit(grDevices::dev.off(), add = TRUE)

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

      label_size <- plot@layers[[i]]$aes_params$size

      # 3 - check that it has the adjustment attribute
      if(!is.null(attr(label, "adj_plot_label"))){

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
