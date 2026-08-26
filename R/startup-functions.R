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

    # In non-interactive sessions (Rscript, R CMD check, knitr/Quarto, CI),
    # readline() returns "" immediately, which would spin the prompt loop
    # below forever. Just note it and move on instead.
    if (!interactive()) {
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

  # If already registered, just enable showtext and exit
  fams <- try(sysfonts::font_families(), silent = TRUE)
  if (!inherits(fams, "try-error") && "pt-sans" %in% fams) {
    try(showtext::showtext_auto(), silent = TRUE)
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
      invisible(NULL)
    },
    error   = function(e) invisible(NULL),
    warning = function(w) invisible(NULL)
  )
}

# Needed to make sure tests work
.t61_readline <- base::readline
