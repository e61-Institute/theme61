# Add on-graph labels to graphs

Add text labels for lines, columns or other elements directly onto the
graph plot. This is preferred over using legends.

## Usage

``` r
plot_label(
  label = NULL,
  x = NULL,
  y = NULL,
  colour = NA,
  size = 3.5,
  hjust = 0,
  geom = c("text", "label"),
  angle = 0,
  panel = NULL,
  auto_position = TRUE,
  print_position = FALSE
)

plab(
  label = NULL,
  x = NULL,
  y = NULL,
  colour = NA,
  size = 3.5,
  hjust = 0,
  geom = c("text", "label"),
  angle = 0,
  panel = NULL,
  auto_position = TRUE,
  print_position = FALSE
)
```

## Arguments

- label:

  (optional, see Details) String vector. Label text to be displayed. If
  omitted, derived from a discrete `colour`/`fill` scale on the plot, if
  there is one.

- x:

  (optional, see Details) Numeric or string vector. X-axis positions of
  the label text. If supplied, this exact position is always used.

- y:

  (optional, see Details) Numeric or string vector. Y-axis positions of
  the label text. See `x`.

- colour:

  (optional, see Details) Vector of colour names or strings. Defaults to
  a discrete `colour`/`fill` scale on the plot, if there is one, else
  the e61 palette.

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
  left-to-right text. See Details for how this interacts with `x`/`y`.

- panel:

  Optional named list. If the plot is facetted, you can restrict the
  label(s) to a specific panel by supplying the facetting variable(s) as
  a named list, see Details for the syntax.

- auto_position:

  Logical. If TRUE (default),
  [`save_e61()`](https://e61-institute.github.io/theme61/reference/save_e61.md)
  will try to automatically reposition the label to a nearby,
  non-overlapping spot on the chart. See Details for exactly when this
  applies and how positions are chosen. Set to FALSE to always use the
  exact `x`/`y` you supply – `x` and `y` are then required.

- print_position:

  Logical. If TRUE, print the plot's final (auto-positioned) label
  `label`/`x`/`y` to the console, as copy-pasteable `plot_label()`
  arguments, whenever the plot is displayed – no need to call
  [`save_e61()`](https://e61-institute.github.io/theme61/reference/save_e61.md)
  first. Useful for grabbing the chosen positions once so you can pin
  them (or hand-tweak just one or two) instead of auto-positioning every
  time. Defaults to FALSE.

- facet_name, facet_value:

  **\[deprecated\]**

## Value

Object to add to a ggplot (via `+`).

## Details

### Default label text and colour

If the plot maps `colour`/`fill` to a discrete variable (checked in that
order), `label` and `colour` can be derived from that scale instead of
the e61 palette. This covers an explicit
[`scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)/
[`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
(or a theme61 wrapper that constructs one, e.g.
[`scale_colour_e61_aus()`](https://e61-institute.github.io/theme61/reference/scale_e61_aus.md)),
an algorithmic discrete scale like
[`scale_colour_e61()`](https://e61-institute.github.io/theme61/reference/scale_e61.md)/[`scale_colour_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html),
and simply relying on theme61's own default scale by supplying no scale
at all – once the plot is built, all of these resolve to the same thing,
a fixed mapping from each level to its assigned colour:

- If you omit `label` entirely, it defaults to that scale's own levels
  (i.e. what a legend would show), in their resolved order – so
  `plot_label()` with no arguments at all labels every series using its
  exact data value and assigned colour.

- If you supply `label` but omit `colour`, each label's colour is taken
  from the scale in the same order – this assumes `label` is written in
  the same order as the scale's levels. If `label` has more entries than
  the scale has levels for, this is skipped and the e61 palette is used
  instead (rather than guessing a partial match).

- An explicit `colour` always wins outright over any of the above.

A continuous `colour`/`fill` (e.g.
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html))
doesn't count – there's no fixed set of "levels" to derive labels from.

### Automatic positioning

When `auto_position = TRUE` (the default),
[`save_e61()`](https://e61-institute.github.io/theme61/reference/save_e61.md)
tries to move the label to a nearby, non-overlapping spot – but only for
single-panel (unfacetted) charts where the label's colour matches a
line/point/column/area/[`geom_pointbar()`](https://e61-institute.github.io/theme61/reference/geom_pointbar.md)
series in the plot (`colour` for lines, points and
[`geom_pointbar()`](https://e61-institute.github.io/theme61/reference/geom_pointbar.md),
`fill` for columns and areas), and only for unrotated text
(`angle = 0`). For an area series, the label is placed fully inside the
band where there's room, recoloured to contrast with the fill, or
outside it (in the fill's own colour) where the band is too narrow. For
a
[`geom_pointbar()`](https://e61-institute.github.io/theme61/reference/geom_pointbar.md)
series, the buffer accounts for the full error-bar extent, not just the
point.

If you supply `x`/`y`, that position is always used exactly as given –
the placement algorithm never runs for that label. If you don't, the
fallback order is: (1) a good spot found by the placement algorithm; (2)
any collision-free spot on the chart (i.e. empty space), even if it's
not a particularly good one; (3) the centre of the panel, so the label
stays visible rather than vanishing.

A facetted plot, or rotated text (`angle != 0`), has no automatic
positioning to fall back on, so `x`/`y` are required in those cases (as
they are whenever `auto_position = FALSE`).

Set `theme61.auto_label = FALSE` (see
[`set_t61_options()`](https://e61-institute.github.io/theme61/reference/set_t61_options.md))
to turn automatic positioning off globally – `x`/`y` are then always
required, the same as `auto_position = FALSE`, and no auto-positioning
work is attempted at all (no performance cost from the feature).

### Facet targeting

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
