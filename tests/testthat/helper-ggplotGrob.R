# ggplot2::ggplotGrob() called directly (outside save_e61()'s own pipeline,
# which already muffles this via t61_with_device()) warns "font family
# 'pt-sans' not found in PostScript font database": grid's font-metric
# fallback doesn't know about sysfonts-registered families on some devices,
# but showtext still renders pt-sans correctly wherever it's actually drawn.
# Tests that just need the built gtable (not testing this warning itself)
# should call this instead of ggplot2::ggplotGrob() directly.
# t61_with_device() muffles the warning and stops ggplotGrob() leaving the
# session default device current for later tests to measure on.
quiet_ggplotGrob <- function(plot) {
  t61_with_device(ggplot2::ggplotGrob(plot))
}
