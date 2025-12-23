
# Specify variables -------------------------------------------------------

# From urban extent
city <- "ZAF-Cape_Town"
aoi_name <- "business_district"
aoi_path <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/scenarios/baseline/baseline/tile_00001/ccl_layers/aoi__baseline__baseline.geojson"

# From AOI
city <- "ZAF-Durban"
aoi_name <- "inner_city_lap"
aoi_path <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Durban/ZAF-Durban__inner_city_lap.geojson"

city <- "ZAF-Cape_Town"
aoi_name <- "test"
aoi_path <-  "~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/boundaries.geojson"

infra_name <- "trees"
scenario_name <- "pedestrian-achievable-90pctl"

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
src_data_folder <- glue("city_projects/{city}/urban_extent")
city_folder <- glue("city_projects/{city}/{aoi_name}")

open_urban_aws_http <- glue("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}")

baseline_name <- "baseline"

baseline_folder <- glue("{src_data_folder}/scenarios/baseline/{baseline_name}")

# Get tile ids
tiles <- list_tiles(glue("s3://wri-cities-tcm/{baseline_folder}"))

if (aoi_name == "urban_extent"){
  aoi <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/urban_extent_boundary.geojson"))
} else {
  aoi <- st_read(aoi_path)
}

buffered_tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/tile_grid.geojson"))
tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/unbuffered_tile_grid.geojson"))

# create scenario data
source(here("tiling-scripts", "trees-functions.R"))
run_tree_scenario(tiles, bucket, aws_http, baseline_folder, scenario_folder, city_folder, open_urban_aws_http)


# download tiles

# run CTCM on scenarios

# upload data to s3