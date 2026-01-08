# An add-in to easily create plots with ggplot2

Select data to be used and map variables to aesthetics to produce a
chart, customise common elements and get code to reproduce the chart.

## Usage

``` r
e61_chart_maker(
  data = NULL,
  controls = c("labs", "parameters", "appearance", "filters", "code"),
  viewer = getOption(x = "esquisse.viewer", default = "dialog")
)
```

## Arguments

- data:

  A data.frame, you can pass a data.frame explicitly to the function, or
  choose one in global environment after loading.

- controls:

  Character. Controls menu to be displayed. Use `NULL` to hide all
  menus.

- viewer:

  Character. Where to display the gadget: "dialog", "pane" or "browser"
  (see [`shiny::viewer()`](https://rdrr.io/pkg/shiny/man/viewer.html)).

## Value

You can view code used to produce the chart, copy it or insert it in
current script.

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch e61_chart_maker in RStudio :
e61_chart_maker(iris)

# Launch e61_chart_maker in without data specified :
e61_chart_maker()
} # }
```
