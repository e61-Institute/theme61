# Builds the colour palette diagram
inputs <- CJ(row = seq(1, 12), col = seq(0, 8))
inputs[, value := .I][, value := as.factor(value)]
inputs[, row := factor(row, labels = rev(c("Blue dark", "Blue light", "Teal dark", "Teal light", "Sky light", "Sky dark", "Coral dark", "Coral light", "Orange light", "Orange dark", "Maroon dark", "Maroon light")))]

prep_pal <- function(search) {
  unname(unlist(mget(apropos(search), inherits = TRUE)))
}

col_pal <- lapply(c("e61_bluedark", "e61_bluelight", "e61_tealdark", "e61_teallight", "e61_skylight", "e61_skydark", "e61_coraldark", "e61_corallight", "e61_orangelight", "e61_orangedark", "e61_maroondark", "e61_maroonlight"), prep_pal)
col_pal <- rev(col_pal)
col_pal <- unlist(col_pal)

p <- ggplot(inputs, aes(x = col, y = row, fill = value)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_manual(values = col_pal) +
  scale_x_continuous_e61(n.breaks = 9) +
  labs_e61(title = "The e61 Institute Colour Palette")

save_e61(
  plot = p,
  filename = here::here("man/figures/g-palette-colours.svg"),
  dim = list(height = 6),
  auto_scale = FALSE
)

# Builds the state colour diagram
data <- data.frame(
  call = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "AUS"),
  label = c("NSW/Sydney", "VIC/Melbourne", "QLD/Brisbane", "SA/Adelaide", "WA/Perth", "TAS/Hobart", "NT/Darwin", "ACT/Canberra", "Australia"),
  y = rep(1, 9)
)

p <- ggplot(data, aes(x = call, y = y, fill = call)) +
  geom_col() +
  geom_text(aes(label = label), y = 0.05, hjust = 0, colour = "white") +
  coord_flip() +
  scale_y_continuous_e61(c(0, 1, 1), expand_bottom = 0, expand_top = 0) +
  scale_fill_e61_aus() +
  labs_e61(title = "The e61 Institute States and Territories Colour Palette", y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

save_e61(
  plot = p,
  filename = here::here("man/figures/g-palette-state-colours.svg"),
  auto_scale = FALSE
)
