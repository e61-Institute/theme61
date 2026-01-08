# Adds a thin black line

Adds a thin black line (by default at y = 0). This is useful shorthand
for when you would otherwise have to use `geom_hline`, which by default
adds a black line that is too thick.

## Usage

``` r
add_baseline(y = 0, colour = "black", linewidth = 0.25)
```

## Arguments

- y:

  Numeric. The y-intercept of the line. Defaults to 0. You can set it to
  100 if you have an index on your y-axis, for example. Or any other
  value if you're feeling chaotic.

- colour:

  String. The line is black by default but you can change it if you need
  another colour.

- linewidth:

  Numeric. Set the thickness of the line. Default (0.25) should be
  appropriate for default graph scaling.

## Value

ggplot object

## Details

Some examples of when you want a baseline include whenever the y-axis on
your graph includes 0 (for units and percentages) or 100 (for indices
that start at 100).
