#' e61 themed graph options
#'
#' @title An add-in to easily create plots with ggplot2
#'
#' @description Select data to be used and map variables to aesthetics to produce a chart,
#'  customize common elements and get code to reproduce the chart.
#'
#' @param data a `data.frame`, you can pass a `data.frame` explicitly to the function,
#' or choose one in global environment after loading.
#' @param controls Controls menu to be displayed. Use `NULL` to hide all menus.
#' @param viewer Where to display the gadget: `"dialog"`,
#'  `"pane"` or `"browser"` (see \code{\link[shiny]{viewer}}).
#'
#' @return `NULL`. You can view code used to produce the chart, copy it or insert it in current script.
#' @export
#'
#' @importFrom shiny dialogViewer browserViewer runGadget paneViewer reactiveValues
#'
#' @examples
#' # Launch e61_chart_maker in RStudio :
#' e61_chart_maker(iris)
#'
#' # Launch e61_chart_maker in without data specified :
#' e61_chart_maker()

e61_chart_maker <-
  function(
    data = NULL,
    controls = c("labs", "parameters", "appearance", "filters", "code"),
    viewer = getOption(x = "esquisse.viewer", default = "dialog")){

    esquisse::esquisser(data, controls, viewer)
}
