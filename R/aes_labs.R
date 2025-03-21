#' Get aesthetic titles, subtitles and footnotes.
#' plot - Plot object to adjust.
#' plot_width - Numeric. Width of the plot.
#' @noRd
update_labs <- function(plot, plot_width){

  p <- ggplotGrob(plot)


  # Title ----

  # First check whether the title has already been manually wrapped
  if(is.null(attr(plot$labels$title, "title_wrap"))){

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
  } else {
    title_text <- plot$labels$title
  }

  # set the title to element blank if it is not required - otherwise it leaves a useless space
  if(is.null(title_text) || title_text == ""){
    plot <- plot + theme(plot.title = element_blank())
  }

  # Subtitle ----
  if(is.null(attr(plot$labels$subtitle, "subtitle_wrap"))){
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
  } else {
    subtitle_text <- plot$labels$subtitle
  }

  # set the title to element blank if it is not required - otherwise it leaves a useless space
  if(is.null(subtitle_text) || subtitle_text == ""){
    plot <- plot + theme(plot.subtitle = element_blank())
  }

  # Footnotes ----

  if(is.null(attr(plot$labels$caption, "caption_wrap"))){
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
  } else {
    caption_text <- plot$labels$caption
  }

  # Update the x-axis label spacing if there is no x-axis label ----

  if(is.null(plot$labels$x) || plot$labels$x == ""){
    plot <- plot + theme(axis.title.x = element_blank())
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

    text <- paste(text$collapsed_text, collapse = "\n")

    # make sure we don't have only one word hanging on the last line
    if(stringr::str_detect(text, "\\\n\\S+$")){

      last_two_words <-
        stringr::str_extract(text, "\\S+\\\n\\S+$") |>
        stringr::str_replace_all("\\\n", " ")

      text <- text |>
        stringr::str_remove("\\S+\\\n\\S+$") |>
        paste("\n", last_two_words)
    }

  # another rule for footnotes
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

        text_lines[[i]] <- get_lines(footnote_data$footnote_text[i], font_size, plot_width)

        text_lines[[i]][, footnote_num := i]
      }

      text_lines <- data.table::rbindlist(text_lines)

      # combine text into a caption along with the sources
      footnote_data <-
        text_lines[, .(footnote = paste(collapsed_text, collapse = "\n")), by = footnote_num]

      footnote_data[, footnote := paste(strrep("*", as.numeric(footnote_num)), footnote)]
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
  words[, word_width := get_text_width(paste0(word, " "), font_size)]

  # assign words to different lines based on the cumulative length
  words[, cumsum_word_width := cumsum(word_width) / plot_width]

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

    words <- words[, cumsum_word_width := cumsum(word_width) / plot_width]

    i <- i + 1

    if(nrow(words) == 0) break
  }

  text_lines <- data.table::rbindlist(text_lines)

  # combine lines
  text_lines <- text_lines[, .(collapsed_text = paste(word, collapse = " ")), by = line]

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

#' Update y-axis label spacing so that they are aesthetic
#' plot - Plot object to adjust.
#' y_lab_max_size - for multi-panels the max size of the y-axis labels
#' panel_width - width of the panels in the chart
#' @noRd
update_y_axis_labels <- function(plot,
                                 max_break_width = NULL,
                                 y_lab_max_size = NULL,
                                 any_neg_break = FALSE,
                                 any_dec_break = FALSE) {

  # get the difference in the label size
  y_font_size <- get_font_size(plot, elem = "axis.text.y", parent = "axis.text")

  # add the spacing of one character for the other margin
  spacing <- y_font_size / 5

  # if the y lab max is null, then we have a single panel plot and don't need to worry about the max label size
  if(is.null(y_lab_max_size)){

    adj_width <- get_y_break_width(plot)

    if(adj_width > 20) spacing <- spacing + 1.2 * y_font_size

  # otherwise we have a multi panel and need to take into account the width of the widest break label
  } else {

    # This is old code that used to be necessary but it no longer is as the way ggplot calculates margin
    # spacing has changed
    # # get the maximum offset and compare to the
    # y_lab_max_size <- y_lab_max_size * .pt * 10

    # max_offset <- pmax(y_lab_max_size, max_break_width)
    adj_width <- max_break_width

    # increase the adjustment if there are decimal places and negatives
    if(any_neg_break) adj_width <- adj_width + 2

    if(any_dec_break) adj_width <- adj_width + 2

    if(max_break_width > 20) adj_width <- adj_width - 4

  }

  plot <- plot +
    theme(
      axis.title.y.left = element_text(margin = margin(l = spacing, r = - adj_width), vjust = 1, hjust = 1, angle = 0),
      axis.title.y.right = element_text(margin = margin(l = - adj_width, r = spacing), vjust = 1, hjust = 0, angle = 0)
    )

  return(plot)
}

#' Get the y-axis breaks for a chart
#' plot - Plot object to adjust.
#' @noRd
get_y_breaks <- function(plot){

  # get the break text size
  p_build <- ggplot_build(plot)

  break_text_size <- get_font_size(plot, elem = "axis.text.y", parent = "axis.text")

  # check what the y-axis breaks are - this will show up if the user has specified the breaks
  breaks <- p_build$layout$panel_scales_y[[1]]$labels

  if(is.null(breaks) || length(breaks) == 0)
    breaks <- p_build$layout$panel_scales_y[[1]]$breaks

  # if there are no breaks use the scale y-continuous function to find them instead
  if(is.null(breaks) || length(breaks) == 0 || is.function(breaks)){

    # save the existing limits - if there are any
    y_scale_lims <- layer_scales(plot)$y$limits

    # check if we already have a proper e61 scale - no need to redo the work and get it wrong
    if(length(y_scale_lims) == 3){

      aes_lims <- y_scale_lims

      # otherwise we'll need to re calculate what the aesthetic limits will look like
    } else {

      # check whether the chart is a bar chart or not
      is_bar <- is_barchart(plot)

      # if there are existing limits - use those first
      if(!is.null(y_scale_lims)){

        min_y <- y_scale_lims[1]
        max_y <- y_scale_lims[2]

        # check whether the tick mark with the given limits is null, if it is we'll need to calculate all three from scratch
        tick <- get_aes_ticks(min_y, max_y)

        if(is.null(tick)){
          aes_lims <- unlist(get_aes_limits(min_y, max_y, from_zero = is_bar))

        } else {
          aes_lims <- c(min_y, max_y, tick)
        }

        # otherwise have a look at the data
      } else {

        # this looks at the underlying chart data and returns the min and the max y values
        minmax <- get_y_minmax(plot)

        min_y <- minmax[[1]]
        max_y <- minmax[[2]]

        # get aesthetic limits for the y-axis - if it is a bar chart, then include zero
        aes_lims <- unlist(get_aes_limits(min_y, max_y, from_zero = is_bar))
      }
    }

    # check whether the y-title is at the top - then we want the max break to be smaller
    y_angle <- plot$theme$axis.title.y.left$angle
    y_vjust <- plot$theme$axis.title.y.left$vjust

    if(length(y_angle) != 0 && length(y_vjust) != 0 && y_angle == 0 && y_vjust == 1) {

      # then define the breaks
      breaks <- seq(aes_lims[[1]], aes_lims[[2]] - aes_lims[[3]], aes_lims[[3]])
    } else {

      # then define the breaks
      breaks <- seq(aes_lims[[1]], aes_lims[[2]], aes_lims[[3]])
    }
  }

  return(breaks)
}

#' Get the width of y-axis break labels
#' Plot - Plot object to adjust.
#' @noRd
get_y_break_width <- function(plot){

  breaks <- get_y_breaks(plot)
  break_text_size <- get_font_size(plot, elem = "axis.text.y", parent = "axis.text")

  # take the absolute value of the breaks if required
  neg_breaks <- ifelse(breaks < 0, "-", "")

  breaks <- abs(breaks)

  # get the width of the breaks - find the maximum width
  break_text_widths <- get_text_width(breaks, font_size = break_text_size)

  # add the minus sign width
  neg_breaks_width <- get_text_width(neg_breaks, font_size = break_text_size) * 0.5

  break_text_widths <- break_text_widths + neg_breaks_width

  max_break_width <- max(break_text_widths) # this is in cm

  break_width_pt <- .pt * max_break_width * 10 # so convert to points

  return(break_width_pt)
}

#' Check whether the y-breaks have negatives or decimal places - these break multi facet plots
#' plot - Plot object to adjust.
#' @noRd
check_y_break_type <- function(plot){

  breaks <- get_y_breaks(plot)

  # Check whether there are any negative breaks
  has_neg <- any(breaks < 0, na.rm = T)

  # Check whether there are any breaks with decimal places in them
  has_dec <- any(stringr::str_detect(breaks, "\\."), na.rm = T)

  return(list("any_neg" = has_neg, "any_dec" = has_dec))
}

#' Get the font size of text from a particular aspect of a ggplot
#' plot - Plot object to adjust.
#' elem - the element you want to get the size of
#' parent - the parent element of the element you want to get the size of (used as a fall back option)
#' @noRd
get_font_size <- function(plot, elem = "text", parent = "text"){

  # get the text sizes of the elements we're interested in and the main text size of the plot
  main_text_size <- plot$theme$text$size
  elem_text_size <- plot$theme[[elem]]$size
  parent_text_size <- plot$theme[[parent]]$size

  # if the element does not have a text size, look at the parent
  if(is.null(elem_text_size)){

    # check whether it is a relative text size of numeric
    if(class(parent_text_size) == "rel"){
      text_size <- eval(parse(text = paste(parent_text_size, " * ", main_text_size)))

    } else if (class(parent_text_size) == "numeric")(
      text_size <- parent_text_size
    )

    # if we have the element text size, then use that
  } else {

    if(class(elem_text_size) == "rel" && class(parent_text_size) == "rel"){
      text_size <- eval(parse(text = paste(parent_text_size, " * ", elem_text_size, " * ", main_text_size)))

    } else if (class(elem_text_size) == "numeric")(
      text_size <- elem_text_size

    ) else if (class(elem_text_size) == "rel" && class(parent_text_size) == "numeric")(
      text_size <- eval(parse(text = paste(elem_text_size, " * ", parent_text_size)))
    )
  }

  if(is.null(text_size) || !is.numeric(text_size) || is.na(text_size))
    text_size <- main_text_size

  return(text_size)
}

#' Update the size of plot labels
#' @noRd
update_plot_label <- function(plot, chart_type, base_size){

  for (i in seq_along(plot$layers)){

    # 1 - check whether it has geom_text or geom_label arguments (this is what plot labels are)
    layer_class <- class(plot$layers[[i]]$geom)

    if("GeomText" %in% layer_class || "GeomLabel" %in% layer_class){

      # 2 - check whether it is an plot_label that can be adjusted
      label <- plot$layers[[i]]$aes_params$label

      label_size <- plot$layers[[i]]$aes_params$size

      # 3 - check that it has the adjustment attribute
      if(!is.null(attr(label, "adj_plot_label"))){

        # 4 - update the size - this will depend on the chart width and base text size
        plot$layers[[i]]$aes_params$size <- 3.5 * base_size / 10
      }
    }
  }

  return(plot)
}

#' Update plot margins when new base size is provided
#' @noRd
update_margins <- function(base_size, legend_title) {

  half_line <- base_size / 2

  ret <-
    theme(
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
      plot.subtitle = element_text(
        margin = margin(
          t = 0, r = 0, b = base_size * .5, l = 0,
          unit = "pt"
        )
      )
    )

  # adjust borders to the legend title if there is one
  if (!"element_blank" %in% class(legend_title)) {
    ret <- ret %+replace%
      theme(legend.title = element_text(size = rel(1),
                                        margin = margin(l = 0,
                                                        r = base_size / 4, unit = "pt")))
  }

  return(ret)
}
