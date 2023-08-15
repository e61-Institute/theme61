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
      title = "This title spans over multiple lines, WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW",
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
(t5 <- ggplot(data, aes(x = year, y = value, colour = category)) +
    facet_wrap(vars(category)) +
    geom_line() +
    labs_e61(
      title = "Hello world!",
      subtitle = "Adding a subtitle for the hell of it",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
      sources = c("e61", "ABS")
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test5_facet_wrap.svg"
save_e61(t5, filename = f_name)

# Test 6 - facet_wrap + long titles
(t6 <- plot +
    labs_e61(
      title = "This title spans over multiple lines, WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW WOW",
      subtitle = "Adding a subtitle for the hell of it. And now I'm making it really really really really disaterously long. Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole. Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum.",
      sources = c("e61", "ABS")
    ) +
    facet_wrap(vars(category)))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test6_facet_wrap.svg"
save_e61(t6, filename = f_name)

# Test 7 - different limits
(t7 <-
    data %>% filter(category == "Food and Tobacco") %>%
    ggplot(aes(x = year, y = value, colour = category)) +
    geom_line() +
    labs_e61(
      title = "Hello world!",
      subtitle = "Adding a subtitle for the hell of it",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
      sources = c("e61", "ABS")
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test7_lims.svg"
save_e61(t7, filename = f_name)

# Test 8 - different limits, but as a bar chart
(t8 <-
    data %>% filter(category == "Food and Tobacco") %>%
    ggplot(aes(x = year, y = value, fill = category)) +
    geom_col() +
    labs_e61(
      title = "Hello world!",
      subtitle = "Adding a subtitle for the hell of it",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
      sources = c("e61", "ABS")
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test8_lims.svg"
save_e61(t8, filename = f_name)


# Test auto y-axis limit setting ------------------------------------------

# Test 9 - different limits
(t9 <-
    data %>% filter(category == "Private Education") %>%
    ggplot(aes(x = year, y = value, fill = category)) +
    geom_col() +
    labs_e61(
      title = "Hello world!",
      subtitle = "Adding a subtitle for the hell of it",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
      sources = c("e61", "ABS")
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test9_lims.svg"
save_e61(t9, filename = f_name)

# Test 10 - different y-axis scale
(t10 <-
    data %>% filter(category == "Private Education") %>%
    ggplot(aes(x = year, y = value, fill = category)) +
    geom_col() +
    labs_e61(
      title = "Hello world!",
      subtitle = "Adding a subtitle for the hell of it",
      y = "$",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
      sources = c("e61", "ABS")
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test10_lims.svg"
save_e61(t10, filename = f_name)

# Test 11 - overlapping 0
(t11 <-
    ggplot(data, aes(x = year, y = value - 30, colour = category)) +
    add_baseline() +
    geom_line() +
    labs_e61(
      title = "Hello world!",
      subtitle = "Adding a subtitle for the hell of it",
      y = "$000",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
      sources = c("e61", "ABS")
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test11_zero_overlap.svg"
save_e61(t11, filename = f_name)

# Test 12 - large numbers
(t12 <-
    ggplot(data, aes(x = year, y = 10 * value, colour = category)) +
    add_baseline() +
    geom_line() +
    labs_e61(
      title = "Hello world!",
      subtitle = "Adding a subtitle for the hell of it",
      y = "$000",
      footnotes = "Here follows an exert from the last e61 newsletter: It's been a busy fortnight at e61! So busy in fact, the newsletter authors decided to lazily summarise it via dot points. This fortnight we said farewell to two keen analysts of social policy at e61, Jiaqi and Nicole.",
      sources = c("e61", "ABS")
    ))

f_name <- "C:/Users/JackBuckley/OneDrive - e61 Institute Ltd/Desktop/Test charts/test12_large_nums.svg"
save_e61(t12, filename = f_name)
