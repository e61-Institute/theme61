# Converts SVG to a bitmap file

Converts an SVG file to a bitmap file, currently supports JPEG and PNG.

## Usage

``` r
svg_to_bitmap(file_in, file_out = NULL, res = 1, delete = FALSE)
```

## Arguments

- file_in:

  File path to the SVG image to convert.

- file_out:

  File path to the PNG or JPEG. image to save. Default saves a file with
  the same name and location (except for the file extension).

- res:

  Numeric. Increase the dimensions of the saved PNG or JPEG. E.g.
  `res = 2` doubles the dimensions of the saved graph.

- delete:

  Logical. Delete the original SVG file? (defaults to FALSE).

## Value

Invisibly returns the file path to the PNG image
