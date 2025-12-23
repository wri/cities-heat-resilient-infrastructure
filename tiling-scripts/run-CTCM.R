pkgs <- c(
  "here",
  "terra",
  "sf",
  "tidyverse",
  "lidR",
  "RANN",
  "glue",
  "geoarrow",
  "sfarrow"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(pkgs, library, character.only = TRUE))


# THIS SCRIPT ASSUMES THAT BASELINE DATA EXISTS FOR THE AOI

# Specify variables -------------------------------------------------------

# city, aoi_name, aoi_path, scenarios

# From urban extent
# city <- "ZAF-Cape_Town"
# aoi_name <- "business_district"
# aoi_path <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/scenarios/baseline/baseline/tile_00001/ccl_layers/aoi__baseline__baseline.geojson"

# From AOI
city <- "ZAF-Durban"
aoi_name <- "inner_city_lap"
aoi_path <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Durban/ZAF-Durban__inner_city_lap.geojson"
# scenarios <- c("trees__pedestrian-achievable-90pctl",
#                "cool-roofs__large-buildings",
#                "shade-structures__all-parks")
scenarios <- c("trees__pedestrian-achievable-90pctl")

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

# If aoi does not exist, copy data from urban extent
# if (glue("{aws_http}/{baseline_folder}"))
  

# Get tile ids
tiles <- list_tiles(glue("s3://wri-cities-tcm/{baseline_folder}"))

buffered_tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/tile_grid.geojson"))
tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/unbuffered_tile_grid.geojson"))

# create scenario data

# Trees
if ("trees__pedestrian-achievable-90pctl" %in% scenarios){
  source(here("tiling-scripts", "trees-functions.R"))
  
  infra <- "trees"
  scenario <- "pedestrian-achievable-90pctl"
  scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
  
  run_tree_scenario(tiles, bucket, aws_http, baseline_folder, scenario_folder, city_folder)
}

# Cool roofs
if ("cool-roofs__large-buildings" %in% scenarios){
  source(here("tiling-scripts", "cool-roofs-functions.R"))
  
  infra <- "cool-roofs"
  scenario <- "large-buildings"
  scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
  
  map(tiles, update_albedo) 
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