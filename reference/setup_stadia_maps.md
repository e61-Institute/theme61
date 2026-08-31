# Setup Stadia Maps API

Using Stadia Maps (formerly Stamen Maps) tiles requires setting up a
free API key and registering it in your R session, this helper function
helps you set up your session accordingly.

## Usage

``` r
setup_stadia_maps(api_key = NULL, update_ggmap = NA)
```

## Arguments

- api_key:

  Character. Your Stadia Maps API key. If supplied, the "have you
  registered a key?"/"paste your key" prompts are skipped entirely and
  the key is registered directly via
  [`ggmap::register_stadiamaps()`](https://rdrr.io/pkg/ggmap/man/register_stadiamaps.html).
  Defaults to `NULL`, which preserves the original interactive prompting
  behaviour when run at a console.

- update_ggmap:

  Logical. Whether to update `ggmap` (via
  `remotes::install_github("dkahle/ggmap")`) when the installed version
  does not support Stadia Maps tiles. `TRUE` updates, `FALSE` skips the
  update, and the default `NA` preserves the original interactive prompt
  asking the user to enter 'Y'/'N'.

## Details

To get an API key, you must sign up at
<https://client.stadiamaps.com/signup/>Stadia Maps sign up.

When run at an interactive console with no arguments, this function
behaves as before: it asks (via
[`readline()`](https://rdrr.io/r/base/readline.html)) whether `ggmap`
needs updating and whether you already have an API key, prompting you to
paste it in if so.

In a non-interactive context (e.g. a script run with `Rscript`, a CI
job, a Quarto/Rmd render, or a `testthat` test),
[`readline()`](https://rdrr.io/r/base/readline.html) cannot be used, so
both prompts must instead be answered up-front via `api_key` and
`update_ggmap`. If either of these is left unanswered (`NULL`/`NA`) in a
non-interactive session, this function fails fast with an informative
error instead of hanging or erroring obscurely on a blocked
[`readline()`](https://rdrr.io/r/base/readline.html) call.

## See also

Other map functions:
[`add_map_e61()`](https://e61-institute.github.io/theme61/reference/add_map_e61.md),
[`crop_aus_coord()`](https://e61-institute.github.io/theme61/reference/crop_aus_coord.md)
