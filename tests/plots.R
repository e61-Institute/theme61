library(theme61)
library(ggplot2)


mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
})

p1 <- ggplot(mtcars2) +
  geom_point(aes(x = wt, y = mpg, colour = gear)) +
  labs(
    title = "Fuel economy declines as weight increases",
    subtitle = "(1973-74)",
    caption = "Data from the 1974 Motor Trend US magazine.",
    tag = "Figure 1",
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Gears"
  )

sysfonts::font_add_google("Quattrocento Sans", "Quattrocento Sans")
showtext::showtext_auto()
p1 +
  theme_e61() +
  e61_colour_manual(n = 3) +
  add_e61_logo(x1 = 5, x2 = 5.5, y1 = 30, y2 = 35)

p1 +
  facet_grid(vs ~ am) +
  theme_e61(panel_borders = TRUE, legend = "bottom") +
  e61_colour_manual(n = 3) +
  scale_y_continuous_e61() +
  scale_x_continuous_e61()
