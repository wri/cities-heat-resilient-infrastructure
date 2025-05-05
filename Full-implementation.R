library(here)


# Define parameters -------------------------------------------------------

city <- "ZAF-Cape_Town"

# # Setup directory ---------------------------------------------------------
# 
# city_dir <- here(data, city)
# dir.create(city_dir, showWarnings = FALSE)
# dir.create(here(city_dir, "scenarios"), showWarnings = FALSE)
# 

# Get data ----------------------------------------------------------------


# Define AOI

# url for file in S3
aoi_file <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_Town/processed/citycentre_roi.geojson"

# Name for AOI
aoi_name <- "business_district" 

# Year for albedo data
year <- "2024"

library(reticulate)
use_condaenv("chri", required = TRUE)
source_python(here("get-data.py"))

# Add the Python script folder to sys.path
script_dir <- here()  
py_run_string(sprintf("import sys; sys.path.append('%s')", script_dir))
get_data(city, aoi_file, year, script_dir)



# Run baseline ------------------------------------------------------------


# CTCM metadata

ctcm_run <- "baseline" # Name for CTCM run
version <- 1 # Version number
description <- "Baseline" # Short description
author <- "elizabeth.wesley@wri.org" # User email

utc_offset <- 2 # UTC offset for the city

source(here("scenario-generation", "baseline", "CTCM-baseline-function.R"))
run_CTCM_baseline(city, ctcm_run, version, description, author, utc_offset)



# Generate scenarios ------------------------------------------------------

# street trees achievable
source(here("scenario-generation", "street-trees", "01-street-tree-potentials.R"))
street_tree_scenario(city, scenario = "achievable", scenario_name = "achievable-90pctl", 
                     percentile = 0.9, min_tree_dist = 5)

# cool roofs program (large roofs)
source(here("scenario-generation", "cool-roofs", "01-cool-roofs-potentials.R"))
cool_roof_scenario(city, scenario = "program", scenario_name = "large-buildings", 
                   infrastructure = "cool-roofs", area_threshold = 2000, cool_roof_albedo = 0.62)

# park shade program (25% small parks, 100m max distance to shade large parks)



# Run scenarios -----------------------------------------------------------

# move data to tile folders
# run CTCM


# Calculate metrics -------------------------------------------------------


