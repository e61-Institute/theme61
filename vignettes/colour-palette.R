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
