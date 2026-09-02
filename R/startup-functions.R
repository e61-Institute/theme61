#' The tag of the most recent theme61 release on GitHub, as a
#' `package_version`, or NULL if it can't be determined.
#'
#' Hits the REST API directly rather than via gh: one unauthenticated
#' request for one field doesn't justify gh's dependency chain (httr2,
#' openssl, gitcreds and friends). The cost is gh's credential handling,
#' so this shares GitHub's 60-requests/hour/IP unauthenticated budget --
#' acceptable for a check whose failure mode is already "say nothing".
#'
#' `per_page=1` keeps the payload to a single release, and the first
#' `tag_name` in the response is the most recent one (the API orders
#' releases newest-first), matching what the gh version read.
#' @noRd
t61_latest_release <- function() {

  url <- "https://api.github.com/repos/e61-institute/theme61/releases?per_page=1"

  # download.file() honours options(timeout), which defaults to 60s -- far
  # too long to block a package load on an unreachable network.
  old_timeout <- options(timeout = 5)
  on.exit(options(old_timeout), add = TRUE)

  destfile <- tempfile(fileext = ".json")
  on.exit(unlink(destfile), add = TRUE)

  ok <- tryCatch({
    suppressWarnings(
      .t61_download_file(url, destfile = destfile, quiet = TRUE, mode = "wb")
    )
    TRUE
  },
  # Any network problem at all: say so once and move on
  error = function(e) {
    packageStartupMessage("R could not check if your version of theme61 is up-to-date.")
    FALSE
  })

  if (!ok || !file.exists(destfile)) return(NULL)

  json <- tryCatch(
    readChar(destfile, file.size(destfile), useBytes = TRUE),
    error = function(e) NULL
  )
  if (is.null(json)) return(NULL)

  tag <- regmatches(json, regexpr('"tag_name"\\s*:\\s*"[^"]*"', json))
  if (length(tag) == 0) return(NULL)

  tag <- sub('^"tag_name"\\s*:\\s*"', "", tag)
  tag <- sub('"$', "", tag)
  tag <- sub("^v", "", tag)

  # Guard the comparison in check_pkg_ver(): unlike gh's parsed JSON, this
  # is scraped text, so a rate-limit body or an unexpected tag format must
  # not turn a startup check into a startup error.
  if (!grepl("^[0-9]+([.-][0-9]+)*$", tag)) return(NULL)

  tryCatch(package_version(tag), error = function(e) NULL)
}

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
check_pkg_ver <- function(test = FALSE) {

  latest_v <- t61_latest_release()

  if (is.null(latest_v)) {
    return(invisible(NULL))
  }

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

    if (resp == "Y" && !test) t61_self_update()
  }

  invisible(NULL)

}

#' Update theme61 from GitHub.
#'
#' `dependencies = NA` (remotes' default) rather than TRUE: TRUE also
#' installs Suggests, which for theme61 means pulling sf, strayr's readr/
#' readxl chain and the vignette toolchain onto every analyst's machine on
#' every update, none of which the package needs to draw a graph.
#' @noRd
t61_self_update <- function() {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    cli::cli_alert_warning(
      'The {.pkg remotes} package is needed to update theme61. Run {.run install.packages("remotes")} and try again.')
    return(invisible(NULL))
  }

  remotes::install_github("e61-institute/theme61", dependencies = NA, upgrade = "always")
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

# Needed to make sure tests work. Wrappers rather than direct copies of the
# base/utils functions: copying the binding pulls their .Internal() calls into
# this namespace, which R CMD check flags as theme61 calling .Internal itself.
.t61_readline <- function(...) base::readline(...)
.t61_interactive <- function(...) base::interactive(...)
.t61_download_file <- function(...) utils::download.file(...)
