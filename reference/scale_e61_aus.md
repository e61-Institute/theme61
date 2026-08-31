# A consistent set of colours for Australian states and territories for graphing

A consistent set of colours for Australian states and territories for
graphing

## Usage

``` r
scale_colour_e61_aus(...)

scale_fill_e61_aus(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`ggplot2::scale_colour_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html),
  [`ggplot2::scale_fill_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)

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

graph_data <- data.frame(
  state = c("AUS", "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
  value = runif(9)
  )

ggplot(graph_data, aes(x = state, y = value, fill = state)) +
  geom_col() +
  scale_fill_e61_aus()
```
