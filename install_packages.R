#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

cran_packages <- c(
  "R.utils",
  "RANN",
  "geoarrow",
  "here",
  "lidR",
  "optparse",
  "paws",
  "sfarrow",
  "withr",
  "yaml",
  "pak"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, " ...")
    tryCatch(
      install.packages(pkg),
      error = function(e) message("Failed to install ", pkg, ": ", conditionMessage(e))
    )
  } else {
    message("Already installed: ", pkg)
  }
}

invisible(lapply(cran_packages, install_if_missing))

pak::pkg_install("tidyverse")

message("\nR dependency installation complete.")
message("Environment is ready for run-scenarios.R and sourced workflow scripts.")
