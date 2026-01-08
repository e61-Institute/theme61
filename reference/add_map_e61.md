# Add an underlying base map to ggplot maps

Wrapper around
[`ggmap::get_stadiamap()`](https://rdrr.io/pkg/ggmap/man/get_stadiamap.html)
that makes it easy to insert a base map underneath a ggplot-based map.
The default map type (Stamen's Toner Lite map) is designed to be
unintrusive so the focus remains on the data being visualised.

## Usage

``` r
add_map_e61(
  bbox = c(top = -33.757742, right = 151.492882, bottom = -34.024779, left = 150.839539),
  adjust = 0,
  maptype = "stamen_toner_lite"
)
```

## Arguments

- bbox:

  A named vector of numeric coordinates defining the extent of your map,
  e.g `c(top = -33, right = 151, bottom = -34, left = 150)`. Defaults to
  bounds for Greater Sydney.

- adjust:

  Numeric. Adjust default zoom value.

- maptype:

  Character. Select a valid map type from
  [`ggmap::get_stadiamap()`](https://rdrr.io/pkg/ggmap/man/get_stadiamap.html)'s
  `maptype` argument. The default is set to `stamen_toner_lite`.

## Value

A ggmap object

## See also

Other map functions:
[`crop_aus_coord()`](https://e61-institute.github.io/theme61/reference/crop_aus_coord.md),
[`setup_stadia_maps()`](https://e61-institute.github.io/theme61/reference/setup_stadia_maps.md),
[`theme_e61_spatial()`](https://e61-institute.github.io/theme61/reference/theme_e61_spatial.md)

## Examples

``` r
library(sf)
#> Error in library(sf): there is no package called ‘sf’
sa3 <- strayr::read_absmap("sa32021")
#> Error in loadNamespace(x): there is no package called ‘strayr’
sa3 <- sa3[sa3$gcc_code_2021 == "1GSYD", ] # let's just look at Sydney
#> Error: object 'sa3' not found

 ggplot(sa3) +
 add_map_e61(bbox = c(
   left = min(sa3$cent_long),
   bottom = min(sa3$cent_lat),
   right = max(sa3$cent_long),
   top = max(sa3$cent_lat))
   ) +
 geom_point(aes(x = cent_long, y = cent_lat)) + #plot points
 theme_e61_spatial()
#> Error: object 'sa3' not found
```
