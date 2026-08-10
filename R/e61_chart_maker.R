#' An add-in to easily create plots with ggplot2
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Select data to be used and map variables to aesthetics to produce a chart,
#' customise common elements and get code to reproduce the chart. This is a
#' thin wrapper around `esquisse::esquisser()` - call that directly instead.
#'
#' @param data A data.frame, you can pass a data.frame explicitly to the
#'   function, or choose one in global environment after loading.
#' @param controls Character. Controls menu to be displayed. Use `NULL` to hide
#'   all menus.
#' @param viewer Character. Where to display the gadget: "dialog", "pane" or
#'   "browser" (see [shiny::viewer()]).
#'
#' @return You can view code used to produce the chart, copy it or insert it in
#'   current script.
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch e61_chart_maker in RStudio :
#' e61_chart_maker(iris)
#'
#' # Launch e61_chart_maker in without data specified :
#' e61_chart_maker()
#' }
e61_chart_maker <-
  function(
    data = NULL,
    controls = c("labs", "parameters", "appearance", "filters", "code"),
    viewer = getOption(x = "esquisse.viewer", default = "dialog")
    ){

    lifecycle::deprecate_warn(
      "0.8.0", "e61_chart_maker()",
      details = "This gadget is a thin wrapper around the `esquisse` package - call `esquisse::esquisser()` directly instead. e61_chart_maker() will be removed in a future release."
    )

    # check whether the package has been installed, otherwise prompt users to
    # install it
    if(!require(esquisse)){
      remotes::install_github(
        repo = "JackBuckley/e61-chart-maker",
        force = TRUE,
        replace = TRUE,
        update = "always"
      )
    }

    esquisse::esquisser(data, controls, viewer)
}
