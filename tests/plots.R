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


p1 +
  theme_e61(legend = "bottom", legend_title = TRUE) + scale_y_continuous_e61() +
  e61_colour_manual(n = 3) +
  add_e61_logo()

p1 +
  facet_grid(vs ~ am) +
  theme_e61(panel_borders = TRUE, legend = "bottom",base_family = "Quattrocento Sans", legend_title = TRUE) +
  e61_colour_manual(n = 3) +
  scale_y_continuous_e61() +
  scale_x_continuous_e61()


g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)),linetype="blank", alpha=0.8) +
  labs(title="Density plot",
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       tag = "Figure 1",
       y="Density",
       fill="# Cylinders")+
  theme_e61(base_family = "Quattrocento Sans",legend = "bottom")+
  scale_y_continuous_e61() +
  scale_x_continuous_e61() +
  e61_fill_manual(n = 4) +
  e61_colour_manual(n = 4) +
  add_e61_logo()

library(lubridate)
df <- economics_long[economics_long$variable %in% c("psavert", "uempmed"), ]

df <- df[lubridate::year(df$date) %in% c(1967:1981), ]

# labels and breaks for X axis text
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)

# plot
ggplot(df, aes(x=date)) +
  geom_line(aes(y=value, col=variable),size=1) +
  labs(title="Time Series of Returns Percentage",
       subtitle="Drawn from Long Data format",
       caption="Source: Economics",
       tag = "Figure 1",
       y="Percent %",
       color=NULL) +  # title and caption
  scale_x_date(date_breaks = "3 years",date_labels = "%Y")+
  theme_e61(legend = "bottom")+
    e61_colour_manual(n = 2,labels = c("Market Returns", "Unemployment")) +
    add_e61_logo()

