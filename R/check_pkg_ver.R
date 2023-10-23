#' Checks theme61 version
#'
#' Compares the version of theme61 currently installed with the latest version
#' on Github.
#'
#' This function runs when the package is loaded and throws a warning to the
#' user if the package is out-of-date.
#'
#' @noRd
chk_pkg_ver <- function() {
  # Checks Github for latest version of theme61
  releases <- gh::gh("GET /repos/{owner}/{repo}/releases",
                     owner = "e61-institute",
                     repo = "theme61")

  latest_v <- releases[[1]][["tag_name"]]
  latest_v <- gsub("v", "", latest_v, fixed = TRUE)

  # Get the latest version of the local installation
  inst_v <- packageVersion("theme61")

  # Print a warning to update the package if it is out-of-date
  if (inst_v < latest_v) {
    cli::cli_alert_warning("Your version of theme61 is out-of-date. Please update by running `remotes::install_github('e61-institute/theme61')`")
  }

  invisible(NULL)

}

