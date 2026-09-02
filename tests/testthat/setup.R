# Set up a minimal ggplot object that works with theme61::ggplot and theme61::save_e61
minimal_plot <-
  ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
  geom_point()

minimal_plot_label <-
  ggplot(data.frame(x = c(0, 3), y = c(0, 3)), aes(x, y)) +
  geom_point()


# ggplot2's ggplot_build()/ggplotGrob() open the session's default device when
# none is open, and leave it current. That made the suite order-dependent: the
# first file to trigger it changed the device every later file measured on.
# Opening one up front means nothing has to, so device state stays constant.
grDevices::pdf(NULL)
withr::defer(grDevices::dev.off(), testthat::teardown_env())

# Then hold that line: a test that opens or closes a device now fails loudly
# here rather than silently changing unrelated snapshots later on.
testthat::set_state_inspector(function() {
  list(devices = grDevices::dev.list())
})
