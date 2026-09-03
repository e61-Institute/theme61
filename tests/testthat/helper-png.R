# Dimensions of a PNG on disk, without depending on magick (a Suggests
# package): reading as a nativeRaster gives dim() = c(height, width).
png_info <- function(path) {
  d <- dim(png::readPNG(path, native = TRUE))
  list(width = d[[2]], height = d[[1]])
}
