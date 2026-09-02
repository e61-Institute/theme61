# Set up a minimal ggplot object that works with theme61::ggplot and theme61::save_e61
minimal_plot <-
  ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
  geom_point()

minimal_plot_label <-
  ggplot(data.frame(x = c(0, 3), y = c(0, 3)), aes(x, y)) +
  geom_point()


# ggplot2 opens the session default device when none is open and leaves it
# current, making the suite order-dependent. Open one up front so nothing has to.
grDevices::pdf(NULL)
withr::defer(grDevices::dev.off(), testthat::teardown_env())

# Catch any test that still opens or closes one: a leak silently changes the
# text metrics every later test measures with.
testthat::set_state_inspector(function() {
  list(devices = grDevices::dev.list())
})
