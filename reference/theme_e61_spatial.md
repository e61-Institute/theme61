# e61 themed spatial maps options

Applies the e61 theme to ggplot spatial maps to adjust graph appearance.
If you are looking to change the appearance of titles or labels, check
the arguments in
[`labs_e61()`](https://e61-institute.github.io/theme61/reference/labs_e61.md),
which are probably what you are looking for.

## Usage

``` r
theme_e61_spatial(
  legend = c("none", "bottom", "top", "left", "right", "inside"),
  legend_position = NULL,
  legend_title = FALSE,
  base_family = "pt-sans",
  aspect_ratio = NULL
)
```

## Arguments

- legend:

  Character. Legend position, "none" (default) hides the legend.

- legend_position:

  A numeric vector of length two setting the placement of legends that
  have the "inside" position. Takes values between 0 and 1.

- legend_title:

  Logical. Include legend title? Defaults to FALSE.

- base_family:

  Character. Chart font family. Default for notes is PT Sans.

- aspect_ratio:

  Numeric. Sets the aspect ratio of the graph panel.

## Value

`theme_e61_spatial` returns a ggplot2 object.

## See also

Other map functions:
[`add_map_e61()`](https://e61-institute.github.io/theme61/reference/add_map_e61.md),
[`crop_aus_coord()`](https://e61-institute.github.io/theme61/reference/crop_aus_coord.md),
[`setup_stadia_maps()`](https://e61-institute.github.io/theme61/reference/setup_stadia_maps.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

sa3_shp <- strayr::read_absmap("sa32016")

sydney_map <- filter(sa3_shp, gcc_code_2016 == "1GSYD")

ggplot(data = sydney_map) +
  geom_sf(aes(fill = sa3_code_2016), colour = "black") +
  theme_e61_spatial()
} # }
```
