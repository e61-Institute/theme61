#' Get aesthetic titles, subtitles and footnotes.
#' plot - Plot object to adjust.
#' plot_width - Numeric. Width of the plot.
#' @noRd
update_labs <- function(plot, is_mpanel, plot_width){

  p <- ggplot2::ggplotGrob(plot)

  # Title ----

  # check if the title exists
  title_grob <- p$grobs[[which(p$layout$name == "title")]]
  title_text <- NULL

  if(!is.null(title_grob$children)){
    title_size <- title_grob$children[[1]]$gp$fontsize

    title_text <-
      rescale_text(
        text = plot$labels$title,
        text_type = "title",
        font_size = title_size,
        plot_width = plot_width
      )
  }

  # Subtitle ----

  subtitle_grob <- p$grobs[[which(p$layout$name == "subtitle")]]
  subtitle_text <- NULL

  if(!is.null(subtitle_grob$children)){
    subtitle_size <- subtitle_grob$children[[1]]$gp$fontsize
    subtitle_text <-
      rescale_text(
        text = plot$labels$subtitle,
        text_type = "subtitle",
        font_size = subtitle_size,
        plot_width = plot_width
      )
  }

  # Footnotes ----

  footnote_grob <- p$grobs[[which(p$layout$name == "caption")]]
  caption_text <- NULL

  if(!is.null(footnote_grob$children)){

    footnote_size <- footnote_grob$children[[1]]$gp$fontsize
    caption_text <-
      rescale_text(
        text = plot$labels$caption,
        text_type = "caption",
        font_size = footnote_size,
        plot_width = plot_width
      )
  }

  # add a new labs function to override the old one
  plot_new <- plot +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      caption = caption_text,
      x = plot$labels$x,
      y = plot$labels$y,
      colour = plot$labels$colour,
      fill = plot$labels$fill
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

  # one rule for titles and subtitles
  if(text_type %in% c("title", "subtitle")){

    text <- stringr::str_replace_all(text, "\\\n", " ")

    text <- get_lines(text, font_size, plot_width)

    text <- text %>%
      dplyr::summarise(text = paste(collapsed_text, collapse = "\n")) %>%
      dplyr::pull(text)

  # another rule for footnotes
  } else if(text_type == "caption"){

    footnote_text <- stringr::str_replace_all(text, "\\\n", " ")

    sources <-
      stringr::str_extract(footnote_text, "(?<=Sources{0,1}\\:).*$") %>%
      stringr::str_split(";") %>%
      unlist() %>%
      stringr::str_squish()

    # remove sources
    footnote_text <- stringr::str_extract(footnote_text, "^.*(?=Source.*:.+)")

    # split footnotes up if there are multiple and drop those with length 0
    footnote_text <- stringr::str_split(footnote_text, "\\*+\\s*")

    text_lengths <- lapply(footnote_text, get_text_width, font_size = font_size)

    footnote_data <- data.frame(footnote_text = unlist(footnote_text), text_width = unlist(text_lengths))

    footnote_data <- footnote_data %>%
      dplyr::filter(text_width != 0) %>%
      dplyr::mutate(footnote_text = stringr::str_replace_all(footnote_text, "[\r\n]" , " "))

    # number footnotes and then split into words
    footnote_data <- footnote_data %>% dplyr::mutate(footnote_num = dplyr::row_number())

    if(nrow(footnote_data) > 0){

      # split into words to calculate line lengths
      text_lines <- list()

      for(i in 1:nrow(footnote_data)){

        text_lines[[i]] <- get_lines(footnote_data$footnote_text[i], font_size, plot_width)

        text_lines[[i]] <- text_lines[[i]] %>% dplyr::mutate(footnote_num = i)
      }

      text_lines <- text_lines %>% dplyr::bind_rows()

      # combine text into a caption along with the sources
      footnote_data <- text_lines %>%
        dplyr::group_by(footnote_num) %>%
        dplyr::summarise(footnote = paste(collapsed_text, collapse = "\n"))

      footnote_data <- footnote_data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(footnote = paste(strrep("*", as.numeric(footnote_num)), footnote)) %>%
        dplyr::summarise(footnotes = paste(footnote, collapse = "\n"))

      footnote_text <- footnote_data$footnotes[1]

    # Otherwise we didn't have any footnotes to begin with, so set as an empty string
    } else {
      footnote_text <- NULL
    }

    # Check whether we have sources to add and how many
    if(length(sources) > 1) {

      if(is.null(footnote_text)){
        text <- paste("Sources:", paste(sources, collapse = "; "))

      } else {
        text <- paste(footnote_text, "\nSources:", paste(sources, collapse = "; "))
      }

    } else if(length(sources) == 1){

      if(is.null(footnote_text)){
        text <- paste("\nSource:", sources)

      } else {
        text <- paste(footnote_text, "\nSource:", sources)
      }

    # No sources
    } else {
      if(is.null(footnote_text)){
        text <- NULL

      } else {
        text <- footnote_text
      }
    }
  }

  return(text)
}

#' Calculate break text up into aesthetically sized lines
#' text - String. Text to be measured.
#' font_size - Numeric. Size of the font of the text.
#' plot_width - Numeric. Width of the plot.
#' @noRd
get_lines <- function(text, font_size, plot_width){

  # split text into words and calculate the length of each word
  words <- split_text_into_words(text)

  words <- words %>% dplyr::mutate(word_width = get_text_width(paste0(word, " "), font_size))

  # assign words to different lines based on the cumulative length
  words <- words %>% dplyr::mutate(cumsum_word_width = cumsum(word_width) / plot_width)

  check_lines <- T
  i <- 1
  text_lines <- list()

  while(check_lines){

    text_lines[[i]] <- words %>%
      dplyr::filter(cumsum_word_width <= 1) %>%
      dplyr::mutate(line = i)

    words <- words %>% dplyr::filter(cumsum_word_width > 1)

    words <- words %>% dplyr::mutate(cumsum_word_width = cumsum(word_width) / plot_width)

    i <- i + 1

    if(nrow(words) == 0) break
  }

  text_lines <- text_lines %>% dplyr::bind_rows()

  # combine lines
  text_lines <- text_lines %>%
    dplyr::group_by(line) %>%
    dplyr::summarise(collapsed_text = paste(word, collapse = " "))

  return(text_lines)
}

#' Calculate the width of text in ggplot titles, subtitles and footnotes
#' text - String. Text to be measured.
#' font_size - Numeric. Size of the font of the text.
#' @noRd
get_text_width <- function(text, font_size = 10) {

  R.devices::devEval("nulldev", {
    par(family = "sans", ps = font_size)
    ret <- graphics::strwidth(text, units = "inches") * 2.54
  })

  return(ret)
}

#' Calculate the height of text in ggplot titles, subtitles and footnotes
#' text - String. Text to be measured.
#' font_size - Numeric. Size of the font of the text.
#' @noRd
get_text_height <- function(text, font_size = 10) {

  R.devices::devEval("nulldev", {
    par(family = "sans", ps = font_size)
    ret <- graphics::strheight(text, units = "inches") * 2.54
  })

  return(ret)
}

#' Split a character string into it's individual words
#' text - Text to be split into individual words.
#' @noRd
split_text_into_words <- function(text) {
  words <- strsplit(text, "\\s+")[[1]]
  data.frame(word = words, text = text, stringsAsFactors = FALSE)
}

#' Update y-axis label spacing so that they are aesthetic
#' plot - Plot object to adjust.
#' @noRd
update_y_axis_labels <- function(plot){

  # get the minimum and maximum y-axis values
  min_y <- 0
  max_y <- 0
  chart_data <- ggplot2::ggplot_build(plot)$data

  for(i in seq_along(chart_data)){

    y_data <- chart_data[[i]]$y

    # skip if not numeric
    if(!is.numeric(y_data)) next

    temp_max_y <- chart_data[[i]]$y %>% max(na.rm = T)
    temp_min_y <- chart_data[[i]]$y %>% min(na.rm = T)

    if(is.finite(min_y) & temp_min_y < min_y) min_y <- temp_min_y
    if(is.finite(max_y) & temp_max_y > max_y) max_y <- temp_max_y
  }

  sig_fig_max_y <- stringr::str_extract(max_y, "^[^\\.]*") %>% nchar()
  sig_fig_min_y <- stringr::str_extract(min_y, "^[^\\.]*") %>% nchar()

  if(is.na(sig_fig_max_y)) sig_fig_max_y <- 0
  if(is.na(sig_fig_min_y)) sig_fig_min_y <- 0

  # Update the theme adjustment
  sig_fig <- max(c(sig_fig_max_y, sig_fig_min_y), na.rm = T)

  if(sig_fig == 1){
    adj <- -5

  } else if(sig_fig >= 2){
    adj <- -5 + -5 * (sig_fig - 1)
  }

  plot <- plot +
    ggplot2::theme(
      axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(l = 5, r = adj), vjust = 1, angle = 0),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = adj, r = 5), vjust = 1, angle = 0)
    )

  return(plot)
}
