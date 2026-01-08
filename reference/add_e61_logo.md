# Add e61 logo to graph

Add e61 logo to graph

## Usage

``` r
add_e61_logo(x = 0.9, y = 0.9, size = 0.1)
```

## Arguments

- x, y:

  Numeric. Set the x and y position of the logo. Value needs to be
  between 0 and 1.

- size:

  Numeric. Set the height and width of the logo.

## Value

ggplot2 object

## Examples

``` r
library(ggplot2)
#> 
#> Attaching package: ‘ggplot2’
#> The following objects are masked from ‘package:theme61’:
#> 
#>     facet_grid, facet_wrap, ggplot, ggsave, labs
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
 geom_point() +
 add_e61_logo()

```
