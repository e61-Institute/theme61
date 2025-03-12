# Set up a minimal ggplot object that works with theme61::ggplot and theme61::save_e61
minimal_plot <-
  ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
  geom_point()

minimal_plot_label <-
  ggplot(data.frame(x = c(0, 3), y = c(0, 3)), aes(x, y)) +
  geom_point()

