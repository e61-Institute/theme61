# Colour and fill scales using colours from the e61 palette

The colour and fill scales are designed for discrete scales. If the data
are continuous, we recommend binning the data as this often makes it
easier to distinguish between values than a continuous scale. If a
continuous scale is desired, the `discrete` argument can be set to
`FALSE`.

## Usage

``` r
scale_colour_e61(
  reverse = FALSE,
  discrete = TRUE,
  aesthetics = "colour",
  palette = c("light", "dark", "diverging", "grey"),
  ...
)

scale_fill_e61(
  reverse = FALSE,
  discrete = TRUE,
  aesthetics = "fill",
  palette = c("light", "dark", "diverging", "grey"),
  ...
)
```

## Arguments

- reverse:

  Logical. TRUE reverses the colour order. Defaults to FALSE.

- discrete:

  Logical. Indicate whether to use a discrete scale. Defaults to TRUE.

- palette:

  Character. The specific e61 palette for continuous scales. Must be
  supplied if a continuous scale is used.

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

ggplot2 object

## Examples

``` r
ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
   geom_point() +
   scale_colour_e61() +
   theme_e61()
```
