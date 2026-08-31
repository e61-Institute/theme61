# Highlight one or more groups in a colour or fill scale

Colours the specified group(s) using the e61 palette and greys out every
other level of the mapped `colour`/`fill` variable. This is useful for
drawing attention to one series (or a handful of series) in a chart
without needing a legend, in keeping with the e61 house style of
labelling directly on the graph (see
[`plot_label()`](https://e61-institute.github.io/theme61/reference/plot_label.md)).

## Usage

``` r
scale_colour_e61_highlight(highlight, unhighlighted = e61_greylight3, ...)

scale_fill_e61_highlight(highlight, unhighlighted = e61_greylight3, ...)
```

## Arguments

- highlight:

  Character (or coercible) vector. The value(s) of the mapped
  `colour`/`fill` variable to highlight in colour. Every other level
  present in the data is greyed out.

- unhighlighted:

  Character. Hex code or e61 colour to use for the non-highlighted
  levels. Defaults to `e61_greylight3`.

- ...:

  Arguments passed on to
  [`ggplot2::scale_colour_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)

  `values`

  :   a set of aesthetic values to map data values to. The values will
      be matched in order (usually alphabetical) with the limits of the
      scale, or with `breaks` if provided. If this is a named vector,
      then the values will be matched based on the names instead. Data
      values that don't match will be given `na.value`.

  `aesthetics`

  :   Character string or vector of character strings listing the
      name(s) of the aesthetic(s) that this scale works with. This can
      be useful, for example, to apply colour settings to the `colour`
      and `fill` aesthetics at the same time, via
      `aesthetics = c("colour", "fill")`.

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (the scale limits)

      - A character vector of breaks

      - A function that takes the limits as input and returns breaks as
        output

  `na.value`

  :   The aesthetic value to use for missing (`NA`) values

## Value

Object to add to a ggplot (via `+`).

## Examples

``` r
library(ggplot2)

df <- data.frame(
  year = rep(2020:2023, 3),
  region = rep(c("NSW", "VIC", "QLD"), each = 4),
  value = c(4, 5, 6, 7, 3, 3, 4, 4, 2, 2, 3, 3)
)

ggplot(df, aes(year, value, colour = region)) +
  geom_line() +
  scale_colour_e61_highlight(highlight = "NSW")
```
