# e61 theme for spatial maps

**\[deprecated\]**

## Usage

``` r
theme_e61_spatial(
  legend = c("none", "bottom", "top", "left", "right", "inside"),
  legend_position = NULL,
  legend_title = FALSE,
  base_family = "pt-sans",
  aspect_ratio = NULL,
  background = "white",
  base_line_size = points_to_mm(0.75),
  base_rect_size = points_to_mm(1)
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

- background:

  Character. Default is "white". For all graphs that you save, you
  should control the background colour using the `bg_colour` argument in
  `save_e61`, not here.

- base_line_size:

  Numeric. Default line width.

- base_rect_size:

  Numeric. Default rect width.

## Details

Map-specific axis/gridline styling is now applied automatically on
save/print based on whether a plot contains a spatial layer - use
[`theme_e61()`](https://e61-institute.github.io/theme61/reference/theme_e61.md)
for both regular and spatial plots.
