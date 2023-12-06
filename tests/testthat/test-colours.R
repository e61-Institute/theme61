test_that("Check colour aesthetics", {
  skip("For interactive use only")

  set.seed(42)

  # Column graph with 4 colours
  d1 <- data.table::data.table(
    x = 1:4,
    y = seq(5, 25, length.out = 4)
  )

  p1 <- ggplot(d1, aes(x, y, fill = factor(x))) +
    geom_col()

  save_e61(tempfile(fileext = ".svg"), p1)

  # Line graph with 4 colours
  d2 <- data.table::data.table(x = 1:10)

  d2[, `:=`(y1 = 0.25*x + 10 + rnorm(10),
            y2 = 0.5*x + 8 + rnorm(10),
            y3 = x + 5 + rnorm(10),
            y4 = 1.5*x + rnorm(10)
            )]
  d2 <- data.table::melt(d2, id.vars = "x")

  p2 <- ggplot(d2, aes(x, y = value, colour = variable)) +
    geom_line()

  save_e61(tempfile(fileext = ".svg"), p2)

  # ggplot(d1, aes(x, y, fill = factor(x))) +
  #   geom_col() +
  #   scale_fill_manual(values = c(e61_bluedark, "#10485e", e61_bluelight, "#196f91"))
  #
  # ggplot(d1, aes(x, y, fill = factor(x))) +
  #   geom_col() +
  #   scale_fill_manual(values = c(e61_tealdark, e61_teallight, "#2aaeb8", e61_skydark))
  #
  # "#10485e"
  # "#196f91"
  #
  # "#2aaeb8"
  # "#428489"
  #
  # ggplot(d1, aes(x, y, fill = factor(x))) +
  #   geom_col() +
  #   scale_fill_manual(values = c(e61_skylight, e61_skydark, e61_bluedark, e61_bluelight))
  #
  # gen_palette(c("#10485e", "#196f91", "#ed7f00", "#ffc537"))

})
