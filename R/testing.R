# Title:
# Purpose:
# Author: Jack Buckley
# Date:
# -------------------------------------------------------------------------

# install and load packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  magrittr,
  datapasta,
  readxl,
  janitor,
  lubridate,
  ggthemes,
  viridis,
  tictoc,
  readtext,
  hildareadR,
  bit64,
  broom,
  scales,
  crayon,
  gtable,
  grid
)

# remove all currently loaded files
rm(list = ls())


# Test functions ----------------------------------------------------------

save_e61 <- function(filename,
                      plot = ggplot2::last_plot(),
                      setwidth = 8.5,
                      maxheight = NULL, # maximum height of the chart
                      format = c("svg", "pdf", "eps", "png"),
                      save_data = FALSE,
                      resize = NULL,
                      pointsize = 12,
                      res = 72,
                      test = !isTRUE(getOption("test_save"))
) {

  # Get the number of panel rows and columns --------------------------------

  plot_build <- ggplot2::ggplot_build(plot)

  # Check if the graph was generated with mpanel_e61 by checking for attributes added to mpanels
  if(!is.null(attr(plot, "panel_rows"))){

    n_panel_cols = attr(plot, "panel_cols")
    n_panel_rows = attr(plot, "panel_rows")

  # Get facet dimensions if applicable
  } else if (length(plot$facet$params) != 0) {

    n_panel_cols <- max(plot_build$layout$layout$COL)
    n_panel_rows <- max(plot_build$layout$layout$ROW)

  # The default is just 1 row and 1 column
  } else {
    n_panel_cols <- 1
    n_panel_rows <- 1
  }


  # Set width -------------------------------------------------------------

  # check whether the user has supplied a given width first (i.e. different to the default 8.5cm)
  if(setwidth == 8.5) {

    # When coord_flip() is used to make a plot horizontal, the default dims are too small
    if (isTRUE("CoordFlip" %in% class(ggplot2::ggplot_build(plot)$layout$coord))) {

      # TODO fix this so that it also updates
      width <- 17

    } else {

      width <- 8.5 * n_panel_cols
    }

    # update the size of the text used for titles, footnotes, axes etc.
    p <- ggplotGrob(plot)

    # allow charts to be the width of the panels
    # TODO update to allow the user to select either the width of the panels or the entire chart width (i.e. include axes)
    known_wd <- sum(grid::convertWidth(p$widths, "cm", valueOnly = TRUE))
    tot_panel_width <- width - known_wd

    plot <- update_labs(chart = plot, chart_width = tot_panel_width)

  } else {

    width <- setwidth
  }


  # Height adjustments ------------------------------------------------------

  # Step 1 - Get the amount of free height and width we have to play with (what is not already used up by the set elements)
  p <- ggplotGrob(plot)

  units <- "cm"

  known_ht <- sum(grid::convertHeight(p$heights, units, valueOnly = TRUE))
  known_wd <- sum(grid::convertWidth(p$widths, units, valueOnly = TRUE))

  # calculate the free width and height we have to play with
  free_ht <- if(!is.null(maxheight)) maxheight - known_ht else 100 - known_ht
  free_wd <- width - known_wd

  # Step 2 - Find the number of panels (these have null rows and heights because they are flexible)
  null_rowhts <- as.numeric(p$heights[grid::unitType(p$heights) == "null"])
  null_colwds <- as.numeric(p$widths[grid::unitType(p$widths) == "null"])
  panel_asps <- (
    matrix(null_rowhts, ncol = 1)
    %*% matrix(1 / null_colwds, nrow = 1))

  # check that aspect ratios are consistent
  # stop("Panel aspect ratios must be consistent")

  # Step 3 - Divide the free width by the number of columns (panels) we have
  panel_width <- free_wd / n_panel_cols # width of each panel
  panel_height <- panel_width * max(panel_asps[1,]) # height of each panel (width * aspect ratio)

  height <- known_ht + panel_height * n_panel_rows

  # print(str_c("Width: ", width, " and Height: ", height))

  # Update y-axis limits if they have not been set

  ggsave(filename = filename, width = cm_to_in(width), height = cm_to_in(height))
}

# get the current width of the title, subtitle and footnotes
update_labs <- function(chart, chart_width){

  # chart <- plot
  p <- ggplotGrob(chart)

  # Extract the maximum title character length - length in cm of chart / length in cm of title * nchar(title)
  title_size <- p$grobs[[which(p$layout$name == "title")]]$children[[1]]$gp$fontsize
  title_text <- str_replace_all(chart$labels$title, "\\\n", " ")

  title_width <- get_text_width(title_text, font_size = title_size)
  title_max_char <- floor(chart_width / title_width * nchar(title_text))

  title_text <- paste(strwrap(title_text, width = title_max_char), collapse = "\n")

  # subtitle
  subtitle_size <- p$grobs[[which(p$layout$name == "subtitle")]]$children[[1]]$gp$fontsize
  subtitle_text <- str_replace_all(chart$labels$subtitle, "\\\n", " ")

  subtitle_width <- get_text_width(subtitle_text, font_size = subtitle_size)
  subtitle_max_char <- floor(chart_width / subtitle_width * nchar(subtitle_text))

  subtitle_text <- paste(strwrap(subtitle_text, width = subtitle_max_char), collapse = "\n")

  # footnotes
  footnote_size <- p$grobs[[which(p$layout$name == "caption")]]$children[[1]]$gp$fontsize
  footnote_text <- str_replace_all(chart$labels$caption, "\\\n", " ")

  sources <- str_extract(footnote_text, "(?<=Sources{0,1}\\:).*$") %>% str_split(";") %>% unlist() %>% str_squish()

  # remove sources
  footnote_text <- str_extract(footnote_text, "^.*(?=Source.*:.+)")

  # split up the footnotes if we have multiple
  footnote_text <- str_split(footnote_text, "\\*+\\s*")

  footnote_text_lengths <- lapply(footnote_text, get_text_width, font_size = footnote_size)

  footnote_data <- data.table(text = unlist(footnote_text), text_width = unlist(footnote_text_lengths))

  # update footnote text before saving
  footnote_data <- footnote_data[text_width != 0]

  footnote_data[, text := str_replace_all(text, "[\r\n]" , " ")]

  # number footnotes and then split into words
  footnote_data[, footnote_num := .I]

  footnote_words <- footnote_data %>%
    rowwise() %>%
    do(split_text_into_words(.$text)) %>%
    ungroup()

  footnote_data %<>% left_join(footnote_words, by = "text") %>% select(footnote_num, word)

  footnote_data %<>% mutate(word_width = get_text_width(paste0(word, " "), font_size = footnote_size))

  # assign words to different lines
  footnote_data %<>% group_by(footnote_num) %>% mutate(cum_word_width = cumsum(word_width))

  footnote_data %<>% mutate(line = ceiling(cum_word_width / chart_width))

  # combine lines
  footnote_data %<>%
    group_by(footnote_num, line) %>%
    summarise(collapsed_text = paste(word, collapse = " "))

  # combine text into a caption along with the sources
  footnote_data %<>%
    group_by(footnote_num) %>%
    summarise(footnote = paste(collapsed_text, collapse = "\n"))

  footnote_data %<>%
    ungroup() %>%
    mutate(footnote = paste(rep("*", footnote_num), footnote)) %>%
    summarise(footnotes = paste(footnote, collapse = "\n"))

  footnote_text <- footnote_data$footnotes[1]

  if(length(sources) > 1) {

    caption_text <- paste(footnote_text, "\nSources:", paste(sources, collapse = "; "))

  } else {

    caption_text <- paste(footnote_text, "\nSource:", sources)
  }

  # add a new labs function to override the old one
  chart_new <- chart +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      caption = caption_text,
      x = chart$labels$x,
      y = chart$labels$y,
      colour = chart$labels$colour,
      fill = chart$labels$fill
    )

  return(chart_new)
}

# calculate the width of the text
get_text_width <- function(txt, font_size = 10) {

  R.devices::devEval("nulldev", {
    par(family = "sans", ps = font_size)
    ret <- strwidth(txt, units = "inches") * 2.54
  })

  return(ret)
}

split_text_into_words <- function(text) {
  words <- strsplit(text, "\\s+")[[1]]
  data.frame(word = words, text = text, stringsAsFactors = FALSE)
}


# Use the cars data set to make some charts -------------------------------

data <- USPersonalExpenditure
data <- as.data.table(data, keep.rownames = TRUE)
data <- melt(data, id.vars = "rn", variable.name = "year", value.name = "value")
setnames(data, "rn", "category")
data[, category := factor(category)]
data[, year := as.numeric(as.character(year))]

data_3 <- data[category %in% c("Food and Tobacco", "Household Operation", "Medical and Health")]


# Making the charts -------------------------------------------------------

# Test 1 - regular chart
(plot <-
   ggplot(data, aes(x = year, y = value, colour = category)) +
   geom_line() +
   theme_e61() +
   scale_y_continuous_e61() +
   labs_e61(
     title = "Hello world!",
     subtitle = "Adding a subtitle for the hell of it",
     footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
     sources = c("e61", "ABS")
   ))

t1 <- plot

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test1_regular.svg"
save_e61(t1, filename = f_name)

# Test 2 - expand footnotes, title, subtitle to at least two lines
(t2 <- plot +
    labs_e61(
      title = "This title spans over multiple lines, WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW",
      subtitle = "Adding a subtitle for the hell of it. And now I'm making it really really really really disaterously long",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole. Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum .",
      sources = c("e61", "ABS")
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test2_long_titles.svg"
save_e61(t2, filename = f_name)

# Test 3 - increase the size of titles, subtitles and footnotes
(t3 <- plot +
    theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 18),
      plot.caption = element_text(size = 15)
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test3_large_titles.svg"
save_e61(t3, filename = f_name)

# Test 4 - mpanel chart

# Test 5 - facet_wrap
(t5 <- plot + facet_wrap(vars(category)))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test5_facet_wrap.svg"
save_e61(t5, filename = f_name)

# Test 6 - facet_wrap + long titles
(t6 <- plot +
    labs_e61(
      title = "This title spans over multiple lines, WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW",
      subtitle = "Adding a subtitle for the hell of it. And now I'm making it really really really really disaterously long",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole. Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum.",
      sources = c("e61", "ABS")
    ) +
    facet_wrap(vars(category)))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test6_facet_wrap.svg"
save_e61(t6, filename = f_name)

# Test 7 - facet_wrap + long titles
(t7 <- plot +
    scale_y_continuous_e61(limits = c(0, 100, 20)) +
    labs_e61(
      title = "This title spans over multiple lines, WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW",
      subtitle = "Adding a subtitle for the hell of it. And now I'm making it really really really really disaterously long",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole. Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum.",
      sources = c("e61", "ABS")
    ) +
    facet_wrap(vars(category)))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test7_scales.svg"
save_e61(t7, filename = f_name)

# retrieve info from the y-axis scaling function
(plot <- ggplot(data, aes(x = year, y = value, colour = category)) +
    geom_line() +
    theme_e61() +
    scale_x_continuous_e61(limits = c(1940, 1950)) +
    scale_y_continuous_e61(limits = c(0, 100, 20)) +
    labs_e61(
      title = "Hello world!",
      subtitle = "Adding a subtitle for the hell of it",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
      sources = c("e61", "ABS")
    ))

# Returns the order of the first scale function used - how do we determine this
y_scale_lims <- layer_scales(plot)$y$limits

max_y <- ggplot_build(plot)$data[[1]]$y %>% max()
min_y <- ggplot_build(plot)$data[[1]]$y %>% min()

# determine the scale limits to set for the y-axis

# first case - both numbers above 0
if(min_y >= 0){

  # default to including 0 for now - let the user manually override if they don't want to do that
  min_y_new <- 0

  ase_nums <- get_aes_num(max_y)

  max_y_new <- ase_nums[[1]]
  band_width <- ase_nums[[1]]

  plot <- plot + scale_y_continuous_e61(limits = c(min_y_new, max_y_new, band_width))

# second case - both values below 0
} else if(max_y <= 0){

  # default to including 0 for now - let the user manually override if they don't want to do that
  max_y_new <- 0

  ase_nums <- get_aes_num(min_y)

  min_y_new <- ase_nums[[1]]
  band_width <- ase_nums[[1]]

  plot <- plot + scale_y_continuous_e61(limits = c(min_y_new, max_y_new, band_width))

# third case - min < 0 and max > 0 - tricky one...
} else {


  # find the nice value in the skewed direction first - this will inform the spacing
  if(abs(min_y) > max_y) {

    ase_nums <- get_aes_num(min_y)

    fct <- ase_nums[]

  } else {

    ase_nums <- get_aes_num(max_y)


  }
}


# Functions for getting aesthetic numbers and spacing ---------------------

# Get an asethetic number for setting y-axis scaling defaults
get_aes_num <- function(
    y_val, # value to find the aesthetic value for
    set_val = NULL, # restrict the search to only values which are factors of this number
    zero_overlap = F # boolean value to say whether the two value overlap 0
  ) {

  # define aesthetic y-axis max/min labels - normalise these values using the y_val value order of magnitude
  aes_y_points <- data.table(points = c(10, 12, 15, 16, 20, 25, 30, 40, 50, 60, 75, 80, 100))

  if(zero_overlap) aes_y_points <- aes_y_points[!points %in% c(12, 16)]

  # set the adjustment factor based on whether we are looking at a minimum or maximum value
  if (y_val > 0) adj <- 1 else adj <- -1

  # check for the set variable and if we need to take it into account
  if(!is.null(set_val)) {

    set_val_order_mag <- ceiling(log10(set_val))

    set_val_adj <- set_val / 10 ^ (set_val_order_mag - 2)

    aes_y_points <- aes_y_points[points %% set_val_adj == 0]
  }

  order_mag <- ceiling(log10(adj * y_val))

  aes_y_points[, points_adj := adj * points]

  # choose an aesthetic finishing point if the max is close to one
  aes_y_points[, points_diff := points_adj - (y_val / 10 ^ (order_mag - 2))]

  if (y_val > 0) {
    aes_y_points <- aes_y_points[points_diff > 0][points_diff == min(points_diff)]

  } else {
    aes_y_points <- aes_y_points[points_diff < 0][points_diff == max(points_diff)]
  }

  # take the smallest value that is greater than 0
  y_aes_adj <- adj * aes_y_points$points[1]
  y_aes <- aes_y_points$points[1]

  return(y_aes_adj * 10 ^ (order_mag - 2))
}

# Testing basic function
expect_equal(get_aes_num(87), 100)
expect_equal(get_aes_num(101), 120)
expect_equal(get_aes_num(987), 1000)
expect_equal(get_aes_num(32), 40)
expect_equal(get_aes_num(0.029), 0.03)
expect_equal(get_aes_num(0.29), 0.3)

expect_equal(get_aes_num(-87), -100)
expect_equal(get_aes_num(-101), -120)
expect_equal(get_aes_num(-987), -1000)
expect_equal(get_aes_num(-0.029), -0.03)
expect_equal(get_aes_num(-0.29), -0.3)

# Testing zero overlap functionality
expect_equal(get_aes_num(y_val = -0.09, set_val = 0.1, zero_overlap = T), -0.1)
expect_equal(get_aes_num(y_val = -5, set_val = 2, zero_overlap = T), -6)
expect_equal(get_aes_num(y_val = -7.5, set_val = 2.5, zero_overlap = T), -10)
expect_equal(get_aes_num(y_val = 7.5, set_val = 2.5, zero_overlap = T), 10)
expect_equal(get_aes_num(y_val = -7.4, set_val = 2.5, zero_overlap = T), -7.5)

# Aesthetic tick spacing
get_aes_ticks <- function(bot_val, top_val){

  # check the argments are correctly ordered, otherwise adjust
  if(top_val < bot_val) {
    temp_bot <- bot_val
    bot_val <- top_val
    top_val <- temp_bot
  }

  # differences between points by the number of ticks to include
  five_point <- c(10, 15, 25, 50, 75, 100)
  four_point <- c(12, 16, 20, 40, 60, 80)
  three_point <- c(30)

  # the maximum size of the tick spacing
  max_size <- 100

  # determine the difference between the two points
  if (bot_val <= 0 & top_val >= 0){
    diff <- top_val + abs(bot_val)

    if(bot_val != 0 & top_val != 0){
      max_size <- min(c(top_val, abs(bot_val)))
    }

  } else if (bot_val < 0 & top_val < 0) {

    diff <- abs(bot_val) - abs(top_val)

  } else {
    diff <- top_val - bot_val
  }

  # if the difference is a factor of 5, use five ticks
  if (diff %in% five_point) {
    band_size <- diff / 5

    # if a factor of 4
  } else if (diff %in% four_point) {
    band_size <- diff / 4

    # else use a factor of 3
  } else {
    band_size <- diff / 3
  }

  if(band_size > max_size) band_size <- max_size

  return(band_size)
}

# Test function
expect_equal(get_aes_ticks(0, 50), 10)
expect_equal(get_aes_ticks(0, 60), 15)
expect_equal(get_aes_ticks(20, 60), 10)
expect_equal(get_aes_ticks(20, 30), 2)
expect_equal(get_aes_ticks(-20, 30), 10)
expect_equal(get_aes_ticks(-10, 30), 10)
expect_equal(get_aes_ticks(-15, 30), 15)
expect_equal(get_aes_ticks(-30, 30), 15)
expect_equal(get_aes_ticks(-5, 30), 5)
expect_equal(get_aes_ticks(-5, 5), 2)
expect_equal(get_aes_ticks(-50, -20), 10)
expect_equal(get_aes_ticks(-100, -20), 20)
expect_equal(get_aes_ticks(20, 100), 20)
expect_equal(get_aes_ticks(25, 100), 15)
expect_equal(get_aes_ticks(40, 100), 15)
