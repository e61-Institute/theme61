# Add graph titles and footers in the e61 style

Provides support for well-formatted titles and footer text with minimal
user adjustment needed.

## Usage

``` r
labs_e61(
  title = NULL,
  subtitle = NULL,
  footnotes = NULL,
  sources = NULL,
  title_wrap = NULL,
  subtitle_wrap = NULL,
  footnote_wrap = NULL,
  ytitle_wrap = NULL,
  x = NULL,
  y = NULL,
  y_top = TRUE,
  ...
)
```

## Arguments

- title:

  The text for the title.

- subtitle:

  The text for the subtitle.

- footnotes:

  A vector of footnote text strings. Each new string will be prepended
  with \*, \*\*, \*\*\*, etc. Note you'll need to include the asterisks
  in the title/subtitle yourself. Please be sensible with the number of
  separate points you include in the graph.

- sources:

  String vector providing the names of sources for the graph.

- title_wrap, subtitle_wrap, footnote_wrap:

  Numeric or logical. Set the maximum number of characters per line in
  the title, subtitle and footer text. Set to `FALSE` if you want to
  turn off text wrapping. The default is usually appropriate for the
  default graph dimensions in
  [save_e61](https://e61-institute.github.io/theme61/reference/save_e61.md).

- x, y:

  String to set the x- and y-axis titles. Note that the x-axis title is
  blank (NULL) by default.

- y_top:

  Logical. If `TRUE` (default), the y-axis title is placed underneath
  the subtitle. If `FALSE`, the y-axis label remains on the side of the
  graph.

- ...:

  Additional optional arguments passed to
  [labs](https://ggplot2.tidyverse.org/reference/labs.html).

## Details

You should use vectors in the footnotes and sources to take advantage of
the formatting features of this function.

The arguments in the function allow you to make changes to the text
formatting if required.

The primary purpose of this function is to correctly format footer text
without requiring the user to guess where to put manual line breaks for
long footnotes or put in "Sources:" themselves. It does this by
transforming the `footnotes` and `sources` arguments into nicely
formatted text that goes into the `caption` argument in ggplot2's
[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) function.
Thus, if you are using `footnotes` or `sources`, do not supply a
`caption` argument as well.

## Examples

``` r
  ggplot() +
  theme_e61() +
  labs_e61(
    title = "Graph title*",
    subtitle = "Graph subtitle**",
    sources = c("A source", "Company name", "Better source"),
    footnotes = c("Footnote 1", "Footnote 2")
    )
```
