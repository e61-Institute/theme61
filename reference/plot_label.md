# Add on-graph labels to graphs

Add text labels for lines, columns or other elements directly onto the
graph plot. This is preferred over using legends.

## Usage

``` r
plot_label(
  label,
  x,
  y,
  colour = NA,
  size = 3.5,
  hjust = 0,
  geom = c("text", "label"),
  angle = 0,
  panel = NULL,
  facet_name = lifecycle::deprecated(),
  facet_value = lifecycle::deprecated()
)

plab(
  label,
  x,
  y,
  colour = NA,
  size = 3.5,
  hjust = 0,
  geom = c("text", "label"),
  angle = 0,
  panel = NULL,
  facet_name = lifecycle::deprecated(),
  facet_value = lifecycle::deprecated()
)
```

## Arguments

- label:

  String vector. Label text to be displayed.

- x:

  Numeric or string vector. X-axis positions of the label text.

- y:

  Numeric or string vector. Y-axis positions of the label text.

- colour:

  (optional) Vector of colour names or strings. Default uses the e61
  palette.

- size:

  (optional) Integer. Size of the text, the default size should be
  appropriate in most cases.

- hjust:

  (optional) A numeric value from 0-1. Adjusts the alignment of the
  text. 0 left-aligns (default), 0.5 centre-aligns and 1 right-aligns.

- geom:

  (optional) String. Either "text" (default) or "label". "label" adds a
  white box around the text which could be useful sometimes.

- angle:

  (optional) Numeric. Rotate the labels. Defaults to 0 which is normal
  left-to-right text.

- panel:

  Optional named list. If the plot is facetted, you can restrict the
  label(s) to a specific panel by supplying the facetting variable(s) as
  a named list, see the Details for the syntax.

- facet_name, facet_value:

  **\[deprecated\]**

## Value

Object to add to a ggplot (via `+`).

## Details

The syntax for getting labels to appear on certain facet panels is as
follows.

For facet wraps, supply a named list with the facetting variable name(s)
and the facet value(s) you want the labels to appear on. For example, to
get labels to appear only on panel `1`, use `panel = list(grp = "1")`.
If you have 2 labels that you want to appear on panels `1` and `2`, use
`panel = list(grp = c("1", "2"))`.

For facet grids, you need to supply both the x- and y-dimension facet
variables to get the plot labels to appear correctly. For example, if
your facet variables are `r` and `c`, use
`panel = list(r = "A", c = "1")` to get the labels to appear on the
panel at row `A` and column `1`. If you have 2 labels you want to appear
on panel `A1` and `B2`, use
`panel = list(r = c("A", "B"), c = c("1", "2"))`.
