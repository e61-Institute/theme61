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

#' Checks if the internet is working
#'
#' @noRd
.t61_has_internet <- function() {
  if (requireNamespace("curl", quietly = TRUE)) {
    return(isTRUE(curl::has_internet()))
  }

  old_timeout <- getOption("timeout")
  options(timeout = 1)
  on.exit(options(timeout = old_timeout), add = TRUE)

  con <- try(utils::url("https://cloud.r-project.org", open = "rb"), silent = TRUE)
  if (inherits(con, "try-error")) return(FALSE)
  try(close(con), silent = TRUE)
  TRUE
}

#' Initialize theme61 fonts
#'
#' @noRd
.t61_init_fonts <- function() {
  # Hard opt-out (CI / airgapped machines)
  if (identical(Sys.getenv("THEME61_DISABLE_FONT_DOWNLOAD", unset = ""), "1")) {
    return(invisible(NULL))
  }

  if (!requireNamespace("sysfonts", quietly = TRUE) ||
      !requireNamespace("showtext", quietly = TRUE)) {
    return(invisible(NULL))
  }

  # If already registered, just enable showtext (for the svglite/pdf/eps
  # paths) and additionally make sure systemfonts/ragg know about it too
  # (for the ragg png/jpg path), then exit
  fams <- try(sysfonts::font_families(), silent = TRUE)
  if (!inherits(fams, "try-error") && "pt-sans" %in% fams) {
    try(showtext::showtext_auto(), silent = TRUE)
    try(.t61_register_systemfonts(), silent = TRUE)
    return(invisible(NULL))
  }

  # Skip entirely if offline / DNS broken
  if (requireNamespace("curl", quietly = TRUE)) {
    if (!isTRUE(curl::has_internet())) {
      return(invisible(NULL))
    }
  }

  # Final guard: never allow font download to abort startup
  tryCatch(
    {
      sysfonts::font_add_google("PT Sans", "pt-sans")
      showtext::showtext_auto()
      # ggplot2 4.0's device-agnostic text rendering, and ragg-based
      # devices (see save_graph()'s png/jpg path), resolve fonts via
      # systemfonts rather than showtext's device-patching shim, so also
      # register the font file sysfonts just downloaded with
      # systemfonts. Wrapped separately/defensively -- this is purely
      # additive and must never break the showtext-based fallback above
      # if it fails.
      try(.t61_register_systemfonts(), silent = TRUE)
      invisible(NULL)
    },
    error   = function(e) invisible(NULL),
    warning = function(w) invisible(NULL)
  )
}

#' Register a sysfonts-downloaded font family with systemfonts, so
#' ragg-based devices (which resolve fonts via systemfonts, not showtext)
#' can find it natively.
#'
#' `sysfonts::font_add_google()` has no public accessor for the local file
#' paths it downloads. Internally (see `sysfonts:::font_add_google` /
#' `sysfonts:::download_font_file`) it looks the family up in the Google
#' Fonts metadata DB to get each variant's URL, then downloads each one to
#' `file.path(tempdir(), basename(url))` and hands those paths straight to
#' `font_add()`, which copies the glyph data into sysfonts' own internal
#' registry rather than keeping the file paths around. Google's URLs are
#' opaque hashes (e.g. ".../jizaRExUiTo99u79P0WOxOGMMDQ.ttf") with no
#' "Regular"/"Bold" in the filename, so those downloaded files can't be told
#' apart just by name -- we instead redo the same DB lookup ourselves to
#' recover each variant's URL (and therefore its exact download path), and
#' just check the resulting file still exists in `tempdir()` (it is never
#' deleted after download).
#'
#' This relies on unexported sysfonts internals (`google_font_db()`,
#' `search_db()`), not a documented public API -- if a future sysfonts
#' version renames/removes them, or changes the download path convention,
#' this function simply fails to find anything and silently no-ops. Either
#' way this is purely additive: the showtext fallback used for the
#' svglite/pdf/eps paths does not depend on this succeeding.
#' @noRd
.t61_register_systemfonts <- function(google_name = "PT Sans", family = "pt-sans") {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return(invisible(NULL))
  }

  handle <- curl::new_handle()
  db   <- sysfonts:::google_font_db(TRUE, handle)
  ind  <- sysfonts:::search_db(google_name, TRUE, handle)
  font <- db[[2]][[ind]]

  variant_path <- function(variant) {
    url <- font$files[[variant]]
    if (is.null(url)) return(NULL)
    path <- file.path(tempdir(), basename(url))
    if (!file.exists(path)) return(NULL)
    path
  }

  plain_path <- variant_path("regular")
  if (is.null(plain_path)) {
    return(invisible(NULL))
  }

  # Registered under the same family string theme_e61()/plot_label() use
  # as base_family ("pt-sans", the sysfonts local key) -- not the Google
  # display name "PT Sans" -- so that ragg's systemfonts-based font
  # matching actually resolves the plot's requested family. Registering
  # under a different name here would leave ragg silently falling back to
  # a substitute font while showtext (unaffected, since it patches at the
  # device level rather than matching by name) kept working normally.
  systemfonts::register_font(
    name   = family,
    plain  = plain_path,
    bold   = variant_path("700"),
    italic = variant_path("italic")
  )

  invisible(NULL)
}

# Needed to make sure tests work
.t61_readline <- base::readline
