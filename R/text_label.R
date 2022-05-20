#' Better in-graph text labels
#'
#' A wrapper around \code{\link[ggplot2]{annotate}} that makes it easier to add
#' in-graph text labels on graphs.
#'
#' @param text A vector of text labels to show on the graph.
#' @param x A vector of numbers to position the text along the x-axis.
#' @param y A vector of numbers to position the text along the y-axis.
#' @param colour The colour can be generated in a few ways: \itemize{
#'   \item{Set to NULL (default) to automatically set the colours per the
#'     \code{e61_palette} colour sequence.}
#'   \item{Provide a vector of hex codes or colour names as a string.}
#'   \item{Provide a vector of object names in the e61 colour palette.}
#'   }
#' @param size A numeric value to adjust the size of text relative to the parent
#'   element, see \code{\link[ggplot2]{rel}} for what that means. The default
#'   value should be appropriate for most graphs.
#' @examples
#' data <- data.frame(x = LETTERS[1:3], y = seq(1, 3, 1))
#' g1 <- ggplot(data, aes(x = x, y = y, fill = x)) +
#'  geom_col() +
#'  theme_e61() +
#'  scale_y_continuous_e61(limits = c(0, 4)) +
#'  e61_fill_manual(n = 3)
#'
#'  # Using the automatically generated colours (default)
#'  # This should work whenever you use e61_fill/colour_manual()
#'  g1 + text_label(
#'    text = data$x,
#'    x = c(1, 2, 3),
#'    y = c(1.5, 2.5, 3.5))
#'
#'  # Specify your own colours
#'  g1 + text_label(
#'    text = data$x,
#'    x = c(1, 2, 3),
#'    y = c(1.5, 2.5, 3.5),
#'    colour = c("black", "green", "blue"))
#'
#'  # Specify the e61 colour objects, which then call the corresponding hex codes
#'  g1 + text_label(
#'    text = data$x,
#'    x = c(1, 2, 3),
#'    y = c(1.5, 2.5, 3.5),
#'    colour = c(e61_bluedark, e61_tealdark, e61_skydark))
#' @export

text_label <- function(text, x, y, colour = NULL, size = 5) {

  # Ensure the vectors are all the same length
  if (length(text) != length(x) || length(x) != length(y))
    stop("The number of elements in text, x and y must be the same.")

  if (!is.null(colour)) {
    if (length(text) != length(colour))
      stop("The number of colours provided must equal the number of text labels.")
  }

  # Converts the colour number to the respective colour in the e61 palette
  if (is.null(colour)) {

    colour <- e61_palette(length(text))

  }

  ret_val <-
    ggplot2::annotate(
      "text",
      x = x,
      y = y,
      label = text,
      colour = colour,
      size = ggplot2::rel(size)
    )

  return(ret_val)
}
