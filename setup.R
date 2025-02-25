
# Define the list of packages
packages <- c(
  "terra", 
  "sf", 
  "lidR", 
  "tidyverse", 
  "here",
  "rgee",
  "gfcanalysis",
  "geojsonio"
  )

# Install any packages that aren't already installed
for(pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


library(reticulate)
conda_create(envname = "chri", python_version = "3.10", pip= TRUE, packages = "git+https://github.com/wri/cities-cif@main")

# TODO: remove once CIF includes this
conda_install(envname = "chri", packages = c("gdal=3.10.0", "numpy=2.1"))

use_condaenv("chri", required = TRUE)
