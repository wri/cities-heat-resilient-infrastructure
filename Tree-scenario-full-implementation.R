###################
# Define city & AOI
###################
city <- "MEX-Monterrey"
aoi_file <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/MEX-Monterrey/scenarios/street-trees/monterrey-test-aoi.geojson"

#################################################################
## Run multiple times to create different scenarios for same area
#################################################################
# Define the scenario. Must have one of percentile or target_coverage but not both
scenario <- "achievable"
scenario_name <- "achievable-90pctl"
percentile <- 0.90
target_coverage <- NULL
min_tree_dist <- 5

###############
# CTCM metadata
###############
ctcm_run <- "test"
version <- 1
description <- "Street tree scenarios"
author <- "elizabeth.wesley@wri.org"

utc_offset <- -6

# If you want to use a specific met file save it in the city scenarios folder
# cities-heat-resilient-infrastructure/data/city/scenarios/met_file_name.txt
# and reference the file name here

# met_file <- "None"
met_file <- "met_era5_hottest_days.txt"

# List the names of all the scenario folders you want to run CTCM for
scenarios <- c("achievable-90pctl")
baseline <- TRUE

##########
# Run code
##########

library(here)


# Run python script to get data -------------------------------------------

# You only need to run this once if you're generating multiple scenarios
# for the same area.

library(reticulate)
use_condaenv("chri", required = TRUE)
source_python(here("get-data.py"))

# Add the Python script folder to sys.path
script_dir <- here()  
py_run_string(sprintf("import sys; sys.path.append('%s')", script_dir))
get_data(city, aoi_file, script_dir)


# Generate scenario -------------------------------------------------------

# Run this for as many scenarios as you want.

source(here("scenario-generation", "street-trees", "01-street-tree-potentials.R"))
create_scenario(city, scenario, scenario_name, percentile, 
                target_coverage, min_tree_dist)


# Run CTCM ----------------------------------------------------------------

# This will run for each scenario specified in scenarios list

source(here("scenario-generation", "street-trees", "02-run-CTCM.R"))
run_CTCM(city, ctcm_run, version, description, author, utc_offset, met_file,
         scenarios, baseline)


# Calculate metrics -------------------------------------------------------

# This will calculate scenarios for each scenario specified in scenarios list

source(here("scenario-generation", "street-trees", "03-calculate-metrics.R"))
calc_metrics(city, scenarios)

