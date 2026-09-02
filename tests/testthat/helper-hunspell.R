# Some CI runners (macOS, Windows) don't have the en_AU hunspell dictionary
# installed, so check_spelling() degrades to a no-op there (see
# R/save-helpers.R). Tests that actually exercise spell-checking need it, so
# skip them gracefully instead of failing where it's absent.
has_en_au_dictionary <- function() {
  !is.null(tryCatch(hunspell::dictionary("en_AU"), error = function(e) NULL))
}

skip_if_no_en_au_dictionary <- function() {
  testthat::skip_if_not(has_en_au_dictionary(), "en_AU hunspell dictionary not installed")
}
