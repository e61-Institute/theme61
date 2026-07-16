# Line-of-sight: is a candidate label position "visibly connected" to the
# series it labels, i.e. does a straight line between them cross any other
# ink? Used to prefer candidates a reader can visually trace back to their
# series even when an arrow/connector isn't drawn.

#' Integer pixel coordinates along the straight line from (r0,c0) to
#' (r1,c1), using Bresenham's algorithm (the symmetric all-octants form,
#' see e.g. Wikipedia's "Bresenham's line algorithm"). Returned in walk
#' order from one endpoint to the other.
#' @noRd
t61_bresenham_points <- function(r0, c0, r1, c1) {
  x0 <- round(c0); y0 <- round(r0)
  x1 <- round(c1); y1 <- round(r1)

  dx <- abs(x1 - x0); sx <- if (x0 < x1) 1 else -1
  dy <- -abs(y1 - y0); sy <- if (y0 < y1) 1 else -1
  err <- dx + dy

  n_max <- dx - dy + 2 # upper bound on path length; avoids repeated reallocation
  xs <- integer(n_max)
  ys <- integer(n_max)

  x <- x0; y <- y0
  i <- 0L
  repeat {
    i <- i + 1L
    xs[i] <- x
    ys[i] <- y
    if (x == x1 && y == y1) break
    e2 <- 2 * err
    if (e2 >= dy) { err <- err + dy; x <- x + sx }
    if (e2 <= dx) { err <- err + dx; y <- y + sy }
  }

  cbind(row = ys[seq_len(i)], col = xs[seq_len(i)])
}

#' Is there a clear line of sight between two pixel positions in the
#' occupancy mask? The first/last `skip_ends` pixels of the path are
#' ignored, since they sit right on the label's own box and the series'
#' own ink -- without that, every path would trivially "collide" at both
#' ends.
#' @noRd
t61_line_of_sight <- function(occupancy, r0, c0, r1, c1, skip_ends = 2) {
  pts <- t61_bresenham_points(r0, c0, r1, c1)
  n <- nrow(pts)

  if (n <= 2 * skip_ends) return(TRUE) # path too short to meaningfully test

  interior <- pts[(skip_ends + 1):(n - skip_ends), , drop = FALSE]

  nr <- nrow(occupancy); nc <- ncol(occupancy)
  in_bounds <- interior[, "row"] >= 1 & interior[, "row"] <= nr &
    interior[, "col"] >= 1 & interior[, "col"] <= nc
  interior <- interior[in_bounds, , drop = FALSE]

  if (nrow(interior) == 0) return(TRUE)

  !any(occupancy[cbind(interior[, "row"], interior[, "col"])])
}
