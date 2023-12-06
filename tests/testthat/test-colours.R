test_that("Check colour aesthetics", {
  skip("For interactive use only")

  # generate new palettes with gen_palette()

  set.seed(42)

  # Column graph with 4 colours
  d1 <- data.table::data.table(
    x = 1:4,
    y = seq(5, 25, length.out = 4)
  )

  p <- ggplot(d1, aes(x, y, fill = factor(x))) +
    geom_col()

  save_e61(tempfile(fileext = ".svg"), p)

  # Column graph with as many colours as you want
  make_graph <- function(n) {
    d1 <- data.table::data.table(
      x = 1:n,
      y = seq(5, 25, length.out = n)
    )

    p <- ggplot(d1, aes(x, y, fill = factor(x))) +
      geom_col()

    save_e61(tempfile(fileext = ".svg"), p)
  }

  # Run through all the colours
  for (i in 1:12) {
    make_graph(i)
    Sys.sleep(1)
  }

  # Line graph with 4 colours
  d2 <- data.table::data.table(x = 1:10)

  d2[, `:=`(y1 = 0.25*x + 10 + rnorm(10),
            y2 = 0.5*x + 8 + rnorm(10),
            y3 = x + 5 + rnorm(10),
            y4 = 1.5*x + rnorm(10)
            )]
  d2 <- data.table::melt(d2, id.vars = "x")

  p <- ggplot(d2, aes(x, y = value, colour = variable)) +
    geom_line() +
    scale_colour_manual(values = c(e61_skylight, e61_tealdark, e61_maroonlight, e61_orangedark))

  save_e61(tempfile(fileext = ".svg"), p)

})
