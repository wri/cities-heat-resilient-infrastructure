
# Define the list of packages
packages <- c(
  "terra", 
  "sf", 
  "lidR", 
  "tidyverse", 
  "here",
  "rgee",
  "gfcanalysis",
  "geojsonio",
  "withr",
  "R.utils",
  "yaml",
  "exactextractr"
  )

# Install any packages that aren't already installed
for(pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Python setup
library(reticulate)

# I had to install miniconda

# Step 1: Create the environment with pip and Git installed
conda_create(envname = "chri", python_version = "3.10", packages = c("pip", "git"))

# Step 2: Use the environment
use_condaenv("chri", required = TRUE)
py_config()

# Step 3: Install the GitHub package using pip
py_install("git+https://github.com/wri/cities-cif@main", method = "pip", pip = TRUE)

# If CIF needs to be updated:
# Force reinstall cities-cif from GitHub to get the latest version
# In Anaconda prompt, run as administrator:
# conda activate chri
# pip install --upgrade --force-reinstall git+https://github.com/wri/cities-cif@main

