# Set various options in the theme61 package

To see the list of available options, just run the function in the
console with no arguments i.e. `set_t61_options()`.

## Usage

``` r
set_t61_options(opt = NULL)
```

## Arguments

- opt:

  A named list of options to set. See Details for available options.

## Details

The following options are available to set:

- `theme61.auto_label`: If TRUE (default),
  [`plot_label()`](https://e61-institute.github.io/theme61/reference/plot_label.md)
  text without an explicit `x`/`y` gets automatically positioned by
  [`save_e61()`](https://e61-institute.github.io/theme61/reference/save_e61.md)
  (see
  [`?plot_label`](https://e61-institute.github.io/theme61/reference/plot_label.md)).
  Set to FALSE to turn automatic positioning off entirely and restore
  the previous behaviour, where `x`/`y` are always required
  ([`plot_label()`](https://e61-institute.github.io/theme61/reference/plot_label.md)
  errors immediately if you omit them, regardless of `auto_position`) –
  no auto-positioning work is attempted, so there's no performance cost
  from the feature at all.

- `theme61.auto_theme`: If TRUE (default),
  [`theme_e61()`](https://e61-institute.github.io/theme61/reference/theme_e61.md)
  is automatically applied whenever you call
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html). Set
  to FALSE to turn this off and apply your own theme instead.

- `theme61.autolabel_fallback_msg`: If TRUE (default), every time an
  auto-positioned
  [`plot_label()`](https://e61-institute.github.io/theme61/reference/plot_label.md)
  text settles for a fallback position – rather than a real,
  collision-checked placement – a message names the label and the reason
  (e.g. not yet supported under
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  with an area/pointbar series, or no good spot was found). Set to FALSE
  to turn it off.

- `theme61.autolabel_fast_msg`: Controls the reminder that
  auto-positioned
  [`plot_label()`](https://e61-institute.github.io/theme61/reference/plot_label.md)
  text shown in the Viewer pane preview (or any
  `save_e61(fast_labels = TRUE)` call) uses a quick placement heuristic,
  not the real collision-avoiding search – labels may overlap there even
  when
  [`save_e61()`](https://e61-institute.github.io/theme61/reference/save_e61.md)
  would place them cleanly. By default (unset), it appears at most once
  every 30 minutes. Set to TRUE to show it every time, or FALSE to turn
  it off entirely.

- `theme61.base_size`: The base font size for graphs. This is 10 by
  default.

- `theme61.default_save_format`: The default file save format if format
  is not specified in
  [save_e61](https://e61-institute.github.io/theme61/reference/save_e61.md)
  and the file extension is not provided in `filename`. Unset by
  default, in which case
  [save_e61](https://e61-institute.github.io/theme61/reference/save_e61.md)'s
  own default (all supported formats: svg, pdf, eps, png, jpg) is used.
  Set via
  [`set_format()`](https://e61-institute.github.io/theme61/reference/set_format.md)
  (or this function) to restrict the default(s); clear with
  [`unset_format()`](https://e61-institute.github.io/theme61/reference/set_format.md)
  to go back to saving every format.

- `theme61.disable_spellcheck`: If TRUE,
  [save_e61](https://e61-institute.github.io/theme61/reference/save_e61.md)'s
  spell-checker is skipped entirely, regardless of its `spell_check`
  argument. This is FALSE by default.

- `theme61.iterate_mode`: If TRUE, all of theme61's automatic styling
  and Viewer pane preview rendering is skipped, so graphs print to the
  Plots pane with plain ggplot2 defaults as fast as possible. This is
  FALSE by default. Masked functions
  ([`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html),
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html),
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
  [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html))
  also stop redirecting to their theme61 equivalents and pass straight
  through to the underlying ggplot2 function instead. Any theme61
  functions you call explicitly (e.g.
  [`scale_colour_e61()`](https://e61-institute.github.io/theme61/reference/scale_e61.md),
  [`theme_e61()`](https://e61-institute.github.io/theme61/reference/theme_e61.md),
  [`labs_e61()`](https://e61-institute.github.io/theme61/reference/labs_e61.md))
  still apply as normal, since they become part of the plot object
  regardless of this option.

- `theme61.max_discrete_colours`: The maximum number of levels a
  discrete colour aesthetic can have before automatic colour scale
  injection errors out instead of applying
  [`scale_colour_e61()`](https://e61-institute.github.io/theme61/reference/scale_e61.md).
  This is 12 by default.

- `theme61.max_discrete_fills`: The maximum number of levels a discrete
  fill aesthetic can have before automatic fill scale injection errors
  out instead of applying
  [`scale_fill_e61()`](https://e61-institute.github.io/theme61/reference/scale_e61.md).
  This is 12 by default.

- `theme61.open_in_browser`: If TRUE, graphs will also open in the
  browser in addition to the Viewer pane. This is FALSE by default.

- `theme61.preview_on_print`: If TRUE (default), graphs will be
  automatically previewed in the Viewer pane when printed to the
  console.

- `theme61.sec_axis_msg`: Controls the reminder from
  [sec_rescale_inv](https://e61-institute.github.io/theme61/reference/dual_y_axis.md)
  that rescaled secondary axis changes need the graph code run twice to
  take effect. By default (unset), it appears at most once every 30
  minutes. Set to TRUE to show it every time, or FALSE to turn it off
  entirely.

## Environment variables

A few behaviours run once, when theme61 is loaded (e.g. by
[`library(theme61)`](https://github.com/e61-institute/theme61)), before
any [`options()`](https://rdrr.io/r/base/options.html) call in your
script would take effect. These can't be controlled by
`set_t61_options()` - instead, set the corresponding environment
variable to `"1"` *before* theme61 is loaded, e.g. in your `.Renviron`
file or CI configuration:

- `THEME61_DISABLE_FONT_DOWNLOAD`: Skips registering the bundled PT Sans
  font with sysfonts/showtext. Useful if you don't want showtext enabled
  for the session at all. Enabled (font registration happens) by
  default.

- `THEME61_DISABLE_GEOM_DEFAULTS`: Skips overwriting ggplot2's
  session-wide geom colour/fill defaults (e.g.
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)'s
  default colour). Enabled (defaults are overwritten) by default.

- `THEME61_DISABLE_VERSION_CHECK`: Skips the startup check against
  GitHub for a newer theme61 release. Enabled (the check runs) by
  default.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the default save format to "png"
set_t61_options(list(theme61.default_save_format = "png"))

# Environment variables must be set before library(theme61) is called,
# e.g. at the top of your script or in .Renviron:
Sys.setenv(THEME61_DISABLE_VERSION_CHECK = "1")
library(theme61)
} # }
```
