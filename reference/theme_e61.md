# theme61 theme

Returns a ggplot2 theme with e61 Institute styling applied.

`set_base_size` sets the base size for the theme to be used in
`theme_e61()`. This needs to be set outside of the function because it
is called by other functions as part of the graph rendering process.

## Usage

``` r
theme_e61(
  legend = c("none", "bottom", "top", "left", "right", "inside"),
  legend_position = NULL,
  legend_title = FALSE,
  aspect_ratio = 0.75,
  background = "white",
  base_family = "pt-sans",
  base_line_size = points_to_mm(0.75),
  base_rect_size = points_to_mm(1)
)

set_base_size(base_size)
```

## Arguments

- legend:

  Character. Legend position, "none" (default) hides the legend.

- legend_position:

  A numeric vector of length two setting the placement of legends that
  have the "inside" position. Takes values between 0 and 1.

- legend_title:

  Logical. Include legend title? Defaults to FALSE.

- aspect_ratio:

  Numeric. Sets the aspect ratio of the graph panel.

- background:

  Character. Default is "white". For all graphs that you save, you
  should control the background colour using the `bg_colour` argument in
  `save_e61`, not here.

- base_family:

  Character. Chart font family. Default for notes is PT Sans.

- base_line_size:

  Numeric. Default line width.

- base_rect_size:

  Numeric. Default rect width.

- base_size:

  Numeric. Graph font size. Default is 10.

## Value

`theme_e61` returns a ggplot2 object.

`set_base_size` is used for its side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  theme_e61()
} # }

if (FALSE) { # \dontrun{
library(sf)

sa3_shp <- strayr::read_absmap("sa32016")

sydney_map <- filter(sa3_shp, gcc_code_2016 == "1GSYD")

# Spatial styling (blank axes/gridlines) is applied automatically on
# save/print, so theme_e61() alone is enough here too.
ggplot(data = sydney_map) +
  geom_sf(aes(fill = sa3_code_2016), colour = "black") +
  theme_e61()
} # }
```
