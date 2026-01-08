# Format axes in the e61 style

These functions format the x and y axes to be consistent with e61
styling. This includes removing white space at the beginning and end of
each axis.

## Usage

``` r
scale_y_continuous_e61(
  limits = NULL,
  sec_axis = ggplot2::dup_axis(),
  rescale_sec = FALSE,
  expand_bottom = 0,
  expand_top = 0,
  add_space = FALSE,
  ...
)

scale_x_continuous_e61(
  limits = NULL,
  expand_left = 0.05,
  expand_right = 0.05,
  hide_first_last = FALSE,
  ...
)
```

## Arguments

- limits:

  One of:

  - A numeric vector of length three providing the limits of the scale
    and the increment between each axis tick, e.g. `c(0, 25, 5)` will
    set the axis to range from 0 to 25, with increments of 5 per tick.

  - A numeric vector of length two providing the minimum and maximum
    limits of the scale. The break increments will be automatically
    chosen.

  - `NULL` to use the default scale range.

- sec_axis:

  Logical. Defaults to duplicating the y-axis so it shows on the left
  and right. Set to FALSE to hide the secondary axis.

- rescale_sec:

  Logical. Set this to TRUE if you are using a rescaled secondary axis,
  otherwise leave it as FALSE (default). To add a rescaled secondary
  axis, see the documentation for
  [sec_rescale](https://e61-institute.github.io/theme61/reference/dual_y_axis.md).

- expand_bottom, expand_top:

  Numeric. Add extra space between data points and the top/bottom of the
  graph. See
  [expansion](https://ggplot2.tidyverse.org/reference/expansion.html)
  for details.

- add_space:

  Logical. This argument is for internal theme61 purposes only. It is
  recommended that as a user you do not include it in your function
  call. Defaults to FALSE to ensure that we only add the extra white
  space above the chart when we are saving it.

- ...:

  Arguments passed on to
  [`ggplot2::scale_y_continuous`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

  `name`

  :   The name of the scale. Used as the axis or legend title. If
      [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
      the default, the name of the scale is taken from the first mapping
      used for that aesthetic. If `NULL`, the legend title will be
      omitted.

  `oob`

  :   One of:

      - Function that handles limits outside of the scale limits (out of
        bounds). Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

      - The default
        ([`scales::censor()`](https://scales.r-lib.org/reference/oob.html))
        replaces out of bounds values with `NA`.

      - [`scales::squish()`](https://scales.r-lib.org/reference/oob.html)
        for squishing out of bounds values into range.

      - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
        for squishing infinite values into range.

  `na.value`

  :   Missing values will be replaced with this value.

  `trans`

  :   **\[deprecated\]** Deprecated in favour of `transform`.

  `guide`

  :   A function used to create a guide or its name. See
      [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html)
      for more information.

- expand_left, expand_right:

  Numeric. Add extra space between data points and the left/right of the
  graph. See
  [expansion](https://ggplot2.tidyverse.org/reference/expansion.html)
  for details.

- hide_first_last:

  Logical. Defaults to FALSE. Hides the first and last x-axis labels to
  avoid overlapping with the bottom of the y-axis.
