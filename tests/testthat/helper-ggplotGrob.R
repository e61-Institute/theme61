# ggplot2::ggplotGrob() called directly (outside save_e61()'s own pipeline,
# which already muffles this via t61_with_device()) warns "font family
# 'pt-sans' not found in PostScript font database": grid's font-metric
# fallback doesn't know about sysfonts-registered families on some devices,
# but showtext still renders pt-sans correctly wherever it's actually drawn.
# Tests that just need the built gtable (not testing this warning itself)
# should call this instead of ggplot2::ggplotGrob() directly.
quiet_ggplotGrob <- function(plot) {
  withCallingHandlers(
    ggplot2::ggplotGrob(plot),
    warning = function(w) {
      if (grepl("not found in PostScript font database", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
