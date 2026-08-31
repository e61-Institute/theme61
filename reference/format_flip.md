# Applies changes to the theme for horizontal bar graphs

Horizontal bar graphs made with
[`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
require some changes to the
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) in order
to look proper. theme61 detects
[`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
automatically and applies these changes for you (without overriding any
element you've customised away from the
[`theme_e61()`](https://e61-institute.github.io/theme61/reference/theme_e61.md)
default), so in most cases you don't need to call this function
yourself. It's still exported for the one thing auto-detection can't do:
adjusting `x_adj`, or for manual use outside the normal save/print
pipeline.

## Usage

``` r
format_flip(x_adj = 0, current_theme = NULL)
```

## Arguments

- x_adj:

  Numeric. Adjusts the vertical position of the x-axis title, the
  default works for most graphs. A negative value moves the title up, a
  positive value moves the title down.

- current_theme:

  The plot's current theme, used internally to skip any element the user
  has already customised away from the
  [`theme_e61()`](https://e61-institute.github.io/theme61/reference/theme_e61.md)
  default. Leave as `NULL` for normal manual use.

## Value

ggplot object
