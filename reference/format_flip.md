# Applies changes to the theme for horizontal bar graphs

Horizontal bar graphs made with
[`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
require some changes to the
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) in order
to look proper. This function wraps those changes up in a convenient
function that should be appended at the end of the graph code, after
theming functions such as
[`theme_e61()`](https://e61-institute.github.io/theme61/reference/theme_e61.md)
have been called.

## Usage

``` r
format_flip(x_adj = 0)
```

## Arguments

- x_adj:

  Numeric. Adjusts the vertical position of the x-axis title, the
  default works for most graphs. A negative value moves the title up, a
  positive value moves the title down.

## Value

ggplot object
