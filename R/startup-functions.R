#' Checks theme61 version
#'
#' Compares the version of theme61 currently installed with the latest version
#' on Github.
#'
#' This function runs when the package is loaded and throws a warning to the
#' user if the package is out-of-date.
#'
#' @param test Logical. For testing the interactive prompt.
#' @noRd
#' @importFrom gh gh
check_pkg_ver <- function(test = FALSE) {

  # Checks Github for latest version of theme61
  releases <- tryCatch({
    gh("GET /repos/{owner}/{repo}/releases",
           owner = "e61-institute",
           repo = "theme61",
           .max_wait = 5)
    },
    # Early return if there is a network error for any reason
    error = function(e) {
      packageStartupMessage("R could not check if your version of theme61 is up-to-date.")
      NULL
    }
    )

  if (is.null(releases) || length(releases) == 0) {
    return(invisible(NULL))
  }

  latest_v <- releases[[1]][["tag_name"]]
  latest_v <- gsub("v", "", latest_v, fixed = TRUE)

  # Get the latest version of the local installation
  inst_v <- packageVersion("theme61")

  # Set up a test with fake package versions that trigger the prompt
  if (test) {
    inst_v <- "0.8.2"
    latest_v <- "0.9.0"
  }

  # Prompts to update the package if it is out-of-date
  if (inst_v < latest_v) {

    # Non-interactive sessions would spin the readline() prompt forever.
    if (!.t61_interactive()) {
      cli::cli_alert_info(
        "A newer version of theme61 is available. Run remotes::install_github(\"e61-institute/theme61\") to update.")
      return(invisible(NULL))
    }

    resp <- ""
    while (!resp %in% c("Y", "N")) {
      cli::cli_alert_warning(
        "Your version of theme61 is out-of-date. Enter 'Y' to update or 'N' to ignore.",
        wrap = TRUE)

      resp <- .t61_readline()
    }

    if (resp == "Y" && !test)
      remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")
  }

  invisible(NULL)

}

#' Register the bundled PT Sans font with sysfonts/showtext and systemfonts
#'
#' Registers from the ttf files shipped in inst/extdata/fonts/pt-sans
#' (SIL OFL license) instead of downloading from Google Fonts, so it can't
#' fail from being offline or rate-limited.
#'
#' Registered with both sysfonts (for showtext, which does the actual
#' on-screen/on-device rendering) and systemfonts (which is what svglite
#' consults for glyph metrics in get_text_width()/get_text_height()'s
#' title-wrapping calculations). Without the systemfonts registration,
#' "pt-sans" isn't a real installed font name on most systems, so metric
#' lookups silently fall back to whatever font that name happens to
#' resolve to there -- a different, usually wider or narrower, substitute
#' on each OS -- which is why title wrapping used to look fine on one
#' machine and too conservative on another.
#' @noRd
.t61_init_fonts <- function() {
  # Hard opt-out (e.g. don't want showtext enabled for this session at all)
  if (identical(Sys.getenv("THEME61_DISABLE_FONT_DOWNLOAD", unset = ""), "1")) {
    return(invisible(NULL))
  }

  if (!requireNamespace("sysfonts", quietly = TRUE) ||
      !requireNamespace("showtext", quietly = TRUE)) {
    return(invisible(NULL))
  }

  font_dir <- system.file("extdata", "fonts", "pt-sans", package = "theme61")

  # Idempotent regardless of prior registration state, so this always runs
  # (cheap either way) rather than needing an "already done" check.
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    try(
      systemfonts::register_font(
        name = "pt-sans",
        plain = file.path(font_dir, "PTSans-Regular.ttf"),
        bold = file.path(font_dir, "PTSans-Bold.ttf"),
        italic = file.path(font_dir, "PTSans-Italic.ttf"),
        bolditalic = file.path(font_dir, "PTSans-BoldItalic.ttf")
      ),
      silent = TRUE
    )
  }

  # Already registered with sysfonts (e.g. a previous library(theme61) call
  # this session)
  fams <- try(sysfonts::font_families(), silent = TRUE)
  if (!inherits(fams, "try-error") && "pt-sans" %in% fams) {
    try(showtext::showtext_auto(), silent = TRUE)
    return(invisible(NULL))
  }

  # Never allow font registration to abort startup
  tryCatch(
    {
      sysfonts::font_add(
        family = "pt-sans",
        regular = file.path(font_dir, "PTSans-Regular.ttf"),
        bold = file.path(font_dir, "PTSans-Bold.ttf"),
        italic = file.path(font_dir, "PTSans-Italic.ttf"),
        bolditalic = file.path(font_dir, "PTSans-BoldItalic.ttf")
      )
      showtext::showtext_auto()
      invisible(NULL)
    },
    error = function(e) invisible(NULL)
  )
}

# Needed to make sure tests work
.t61_readline <- base::readline
.t61_interactive <- base::interactive
