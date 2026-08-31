# Crops maps of Australia to exclude outlying territories

The ABS default shapefiles for Australia include outlying islands that
are of limited interest in any maps we produce. This function provides
sensible co-ordinates via
[`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
that crops out those islands in a map.

## Usage

``` r
crop_aus_coord()
```

## See also

Other map functions:
[`add_map_e61()`](https://e61-institute.github.io/theme61/reference/add_map_e61.md),
[`setup_stadia_maps()`](https://e61-institute.github.io/theme61/reference/setup_stadia_maps.md)

## Examples

``` r
  if (FALSE) { # \dontrun{
    ggplot(strayr::read_absmap("aus2021")) +
      geom_sf() +
      crop_aus_coord()
  } # }
```
