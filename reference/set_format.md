# Sets the default file save format if format is not specified

This function sets the file save format if `format` is not specified in
`save_e61` and the file extension is not provided in `filename`.

This function clears the default file save format specified in
`set_format`.

## Usage

``` r
set_format(format)

unset_format()
```

## Arguments

- format:

  A string vector of file formats to save as. Accepts "svg", "pdf",
  "eps", "png", "jpg". For example `c("svg", "pdf")` will save 2 files
  with the same name to the same location to SVG and PDF formats. If the
  file format is specified in `filename` or by the `set_format` option,
  then this argument is ignored.

## Value

This function is used for its side effects.

This function is used for its side effects.
