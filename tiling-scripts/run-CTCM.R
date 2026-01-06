library(here)
library(terra)
library(sf)
library(dplyr)
library(glue)



# THIS SCRIPT ASSUMES THAT BASELINE DATA EXISTS FOR THE AOI

# Specify variables -------------------------------------------------------

# city, aoi_name, aoi_path, scenarios

# From urban extent
# city <- "ZAF-Cape_Town"
# aoi_name <- "business_district"
# aoi_path <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/scenarios/baseline/baseline/tile_00001/ccl_layers/aoi__baseline__baseline.geojson"

# From AOI
city <- "BRA-Recife"
aoi_name <- "accelerator_area"
aoi_path <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/accelerator_area/scenarios/baseline/baseline/aoi__baseline__baseline.geojson")
scenarios <- c("baseline", 
               "trees__pedestrian-achievable-90pctl",
               "cool-roofs__large-buildings",
               "shade-structures__all-parks")
copy_from_extent <- FALSE
# scenarios <- c("trees__pedestrian-achievable-90pctl")

# city <- "ZAF-Cape_Town"
# aoi_name <- "test"
# aoi_path <-  "~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/boundaries.geojson"

# infra_name <- "trees"
# scenario_name <- "pedestrian-achievable-90pctl"

# Functions 

source(here("tiling-scripts", "utils.R"))

# Sign in to AWS ----------------------------------------------------------

system("aws sso login --profile cities-data-dev")
Sys.setenv(AWS_PROFILE = "cities-data-dev",
           AWS_DEFAULT_REGION = "us-east-1",
           AWS_SDK_LOAD_CONFIG = "1")

s3 <- paws::s3()
bucket <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"

# Process baseline tile data ----------------------------------------------

# src_data_folder <- glue("city_projects/{city}/urban_extent")
city_folder <- glue("city_projects/{city}/{aoi_name}")
baseline_folder <- glue("{city_folder}/scenarios/baseline/baseline")

# Get tile ids
tiles <- list_tiles(glue("s3://wri-cities-tcm/{baseline_folder}"))

buffered_tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/tile_grid.geojson"))
tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/unbuffered_tile_grid.geojson"))

aoi <- st_read(aoi_path) %>% 
  st_transform(st_crs(tile_grid))

# If aoi does not exist, copy data from urban extent
if (copy_from_extent) {
  
  tile_ids <- tile_grid %>% 
    st_filter(aoi) %>% 
    pull(tile_name)
  
  # copy folders
  for (t in tile_ids){
    
    from <- glue("city_projects/{city}/urban_extent/baseline/baseline/{t}")
    to <- glue("{baseline_folder}/{t}")
    
    ensure_s3_prefix(bucket, to)
    s3_copy_vec(from = from, to = to, bucket = bucket)
  }
}

# Create baseline data layers
if ("baseline" %in% scenarios){
  source(here("tiling-scripts", "baseline-layers.R"))
  save_baseline_layers()
}

# create scenario data

# Trees
if ("trees__pedestrian-achievable-90pctl" %in% scenarios){
  source(here("tiling-scripts", "trees-functions.R"))
  
  infra <- "trees"
  scenario <- "pedestrian-achievable-90pctl"
  scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
  
  # run_tree_scenario(tiles, bucket, aws_http, baseline_folder, scenario_folder, city_folder)
  run_tree_scenario()
  
}

# Cool roofs
if ("cool-roofs__large-buildings" %in% scenarios){
  source(here("tiling-scripts", "cool-roofs-functions.R"))
  
  infra <- "cool-roofs"
  scenario <- "large-buildings"
  scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
  country <- str_split(city, "-")[[1]][1]
  
  map(tiles, update_albedo()) 
}

# Shade structures
if ("shade-structures__all-parks" %in% scenarios) {
  source(here("tiling-scripts", "park-shade-functions.R"))
  source(here("scenario-generation", "park-shade-structures", "shade-generating-functions.R"))
  
  infra <- "shade-structures"
  scenario <- "all-parks"
  scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
  
  run_shade_scenario()
}


# download tiles

# run CTCM on scenarios

# upload data to s3




