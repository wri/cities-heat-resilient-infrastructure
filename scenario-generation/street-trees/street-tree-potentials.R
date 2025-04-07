####################################################
# Choose your city & specify the scenario parameters
####################################################
city <- "MEX-Monterrey"

scenario <- "achievable"
scenario_name <- "achievable-05pctl"
percentile <- 0.05
target_coverage <- NULL
min_tree_dist <- 5


# Setup ---------------------------------------------------------------------

# Load necessary libraries
library(terra)
library(sf)
library(lidR)
library(tidyverse)
library(here)

inputs_path <- here("data", city)
infrastructure_path <- here(inputs_path, "scenarios", "street-trees")

# Create scenario_path
if (!dir.exists(infrastructure_path)) {
  dir.create(infrastructure_path, recursive = TRUE)
}

# Load input data
aoi <- st_read(here(inputs_path, "aoi.geojson"))
lulc <- rast(here(inputs_path, "open-urban.tif"))
road_vectors <- st_read(here(inputs_path, "roads.geojson"))
lanes <- read_csv(here(inputs_path, "average-lanes.csv"))
canopy_height_existing <- rast(here(inputs_path, "tree-canopy-height.tif")) 
names(canopy_height_existing) <- "height"


# Generate scenarios ------------------------------------------------------

source(here("scenario-generation", "street-trees", "street-trees-scenario-function.R")) 

street_trees_scenario_function(scenario = scenario,
                               scenario_name = scenario_name,
                               percentile = percentile, 
                               target_coverage = target_coverage,
                               min_tree_dist = min_tree_dist,
                               aoi, lulc, road_vectors, lanes, canopy_height_existing)




# Calculate metrics -------------------------------------------------------

met <- read_delim(here("data", city, "scenarios", "met_20jan2022.txt"))

source(here("scenario-generation", "street-trees", "street-tree-metrics-function.R"))

calc_street_tree_metrics(city = city,
                         scenario_path = scenario_path,
                         met_data = met)

