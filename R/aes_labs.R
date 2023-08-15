#' Get aesthetic titles, subtitles and footnotes.
#' @param plot Plot object to adjust.
#' @param plot_width Numeric. Width of the plot.
#' @rdname e61_aes_labs
#' @export
update_labs <- function(plot, plot_width){

  p <- ggplot2::ggplotGrob(plot)

  # Title ----

  title_size <- p$grobs[[which(p$layout$name == "title")]]$children[[1]]$gp$fontsize
  title_text <- stringr::str_replace_all(plot$labels$title, "\\\n", " ")

  title_text <- get_lines(title_text, title_size, plot_width)

  title_text <- title_text %>%
    dplyr::summarise(title = paste(collapsed_text, collapse = "\n")) %>%
    pull(title)


  # Subtitle ----

  subtitle_size <- p$grobs[[which(p$layout$name == "subtitle")]]$children[[1]]$gp$fontsize
  subtitle_text <- stringr::str_replace_all(plot$labels$subtitle, "\\\n", " ")

  subtitle_text <- get_lines(subtitle_text, subtitle_size, plot_width)

  subtitle_text <- subtitle_text %>%
    dplyr::summarise(subtitle = paste(collapsed_text, collapse = "\n")) %>%
    pull(subtitle)


  # Footnotes ----

  footnote_size <- p$grobs[[which(p$layout$name == "caption")]]$children[[1]]$gp$fontsize
  footnote_text <- stringr::str_replace_all(plot$labels$caption, "\\\n", " ")

  sources <-
    stringr::str_extract(footnote_text, "(?<=Sources{0,1}\\:).*$") %>%
    stringr:: str_split(";") %>%
    unlist() %>%
    stringr::str_squish()

  # remove sources
  footnote_text <- stringr::str_extract(footnote_text, "^.*(?=Source.*:.+)")

  # split footnotes up if there are multiple and drop those with length 0
  footnote_text <- stringr::str_split(footnote_text, "\\*+\\s*")

  text_lengths <- lapply(footnote_text, get_text_width, font_size = footnote_size)

  footnote_data <- data.frame(footnote_text = unlist(footnote_text), text_width = unlist(text_lengths))

  footnote_data <- footnote_data %>%
    dplyr::filter(text_width != 0) %>%
    dplyr::mutate(footnote_text = stringr::str_replace_all(footnote_text, "[\r\n]" , " "))

  # number footnotes and then split into words
  footnote_data <- footnote_data %>% dplyr::mutate(footnote_num = row_number())

  # split into words to calculate line lengths
  text_lines <- list()

  for(i in 1:nrow(footnote_data)){

    text_lines[[i]] <- get_lines(footnote_data$footnote_text[i], footnote_size, plot_width)

    text_lines[[i]] %<>% dplyr::mutate(footnote_num = i)
  }

  text_lines %<>% dplyr::bind_rows()

  # combine text into a caption along with the sources
  footnote_data <- text_lines %>%
    dplyr::group_by(footnote_num) %>%
    dplyr::summarise(footnote = paste(collapsed_text, collapse = "\n"))

  footnote_data %<>%
    dplyr::ungroup() %>%
    dplyr::mutate(footnote = paste(rep("*", footnote_num), footnote)) %>%
    dplyr::summarise(footnotes = paste(footnote, collapse = "\n"))

  footnote_text <- footnote_data$footnotes[1]

  if(length(sources) > 1) {

    caption_text <- paste(footnote_text, "\nSources:", paste(sources, collapse = "; "))

  } else {

    caption_text <- paste(footnote_text, "\nSource:", sources)
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

#' Calculate break text up into aesthetically sized lines
#' @param text String. Text to be measured.
#' @param font_size Numeric. Size of the font of the text.
#' @param plot_width Numeric. Width of the plot.
#' @rdname e61_aes_labs
#' @export
get_lines <- function(text, font_size, plot_width){

  # add a little more width for aesthetic reasons - 1.1 for two or more digits y-axis, 1.075 for 1 digit
  plot_width <- plot_width * 1.1
  # plot_width <- plot_width * 1.1
  # plot_width <- plot_width * 1.08

  # split text into words and calculate the length of each word
  words <- split_text_into_words(text)

  words %<>% dplyr::mutate(word_width = get_text_width(paste0(word, " "), font_size))

  # assign words to different lines based on the cumulative length
  words %<>% dplyr::mutate(cumsum_word_width = cumsum(word_width) / plot_width)

  check_lines <- T
  i <- 1
  text_lines <- list()

  while(check_lines){

    text_lines[[i]] <- words %>%
      dplyr::filter(cumsum_word_width <= 1) %>%
      dplyr::mutate(line = i)

    words %<>% dplyr::filter(cumsum_word_width > 1)

    words %<>% dplyr::mutate(cumsum_word_width = cumsum(word_width) / plot_width)

    i <- i + 1

    if(nrow(words) == 0) break
  }

  text_lines %<>% dplyr::bind_rows()

  # combine lines
  text_lines <- text_lines %>%
    dplyr::group_by(line) %>%
    dplyr::summarise(collapsed_text = paste(word, collapse = " "))

  return(text_lines)
}

#' Calculate the width of text in ggplot titles, subtitles and footnotes
#' @param text String. Text to be measured.
#' @param font_size Numeric. Size of the font of the text.
#' @rdname e61_aes_labs
#' @export
get_text_width <- function(text, font_size = 10) {

  R.devices::devEval("nulldev", {
    par(family = "sans", ps = font_size)
    ret <- strwidth(text, units = "inches") * 2.54
  })

  return(ret)
}

#' Split a character string into it's individual words
#' @param text Text to be split into individual words.
#' @rdname e61_aes_labs
#' @export
split_text_into_words <- function(text) {
  words <- strsplit(text, "\\s+")[[1]]
  data.frame(word = words, text = text, stringsAsFactors = FALSE)
}
