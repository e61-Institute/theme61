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
  # Checks Github for latest version of theme61
  releases <- try(gh::gh("GET /repos/{owner}/{repo}/releases",
                     owner = "e61-institute",
                     repo = "theme61",
                     .max_wait = 5),
                  silent = TRUE)

  # Early return if there is a network error for any reason
  if(inherits(releases, "try-error"))
    return(cli::cli_alert_warning("R could not check if your version of theme61 is up-to-date."))

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

      resp <- readline()
    }

    if (resp == "Y" && !test)
      remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")
  }

  invisible(NULL)

}

