library(here)
library(terra)
library(sf)
library(glue)
library(tidyverse)

s3 <- paws::s3()
bucket <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"

source(here("tiling-scripts", "utils.R"))

# THIS SCRIPT ASSUMES THAT BASELINE DATA EXISTS FOR THE AOI

# Specify variables -------------------------------------------------------

# city, aoi_name, aoi_path, scenarios

# From urban extent
# city <- "ZAF-Cape_Town"
# aoi_name <- "business_district"
# aoi_path <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/scenarios/baseline/baseline/tile_00001/ccl_layers/aoi__baseline__baseline.geojson"

# From AOI
for (c in c("BRA-Teresina", "BRA-Fortaleza", "BRA-Recife")){
  
  city <- c
  aoi_name <- "accelerator_area"
  aoi_path <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/accelerator_area/scenarios/baseline/baseline/aoi__baseline__baseline.geojson")
  scenarios <- c("trees",
                 "cool-roofs",
                 "shade-structures")
  copy_from_extent <- FALSE
  open_urban_aws_http <- glue("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}")
  
  # Process baseline tile data ----------------------------------------------
  
  # src_data_folder <- glue("city_projects/{city}/urban_extent")
  city_folder <- glue("city_projects/{city}/{aoi_name}")
  baseline_folder <- glue("{city_folder}/scenarios/baseline/baseline")
  
  # Get tile ids
  tiles_s3 <- list_tiles(glue("s3://wri-cities-tcm/{baseline_folder}"))
  
  buffered_tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/tile_grid.geojson")) %>% 
    filter(tile_name %in% tiles_s3)
  tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/unbuffered_tile_grid.geojson")) %>% 
    filter(tile_name %in% tiles_s3)
  
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
  
  # Filter tile grids to those that intersect the AOI
  buffered_tile_grid_aoi <- buffered_tile_grid %>%
    st_filter(aoi)
  tile_grid_aoi <- tile_grid %>%
    st_filter(aoi)
  
  tiles_aoi <- tile_grid$tile_name
  
  # Trees
  if ("trees" %in% scenarios){
    source(here("tiling-scripts", "trees-functions.R"))
    source(here("tiling-scripts", "CTCM-functions.R"))
    
    infra <- "trees"
    scenario <- "pedestrian-achievable-90pctl"
    scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
    
    run_tree_scenario()
    download_tree_data(city, infra, scenario, baseline_folder, scenario_folder, tiles = tiles_s3)
    
  }
  
  # Cool roofs
  if ("cool-roofs" %in% scenarios){
    source(here("tiling-scripts", "cool-roofs-functions.R"))
    
    infra <- "cool-roofs"
    country <- str_split(city, "-")[[1]][1]
    
    update_albedo() 
  }
  
  # Shade structures
  if ("shade-structures" %in% scenarios) {
    source(here("tiling-scripts", "park-shade-functions.R"))
    source(here("scenario-generation", "park-shade-structures", "shade-generating-functions.R"))
    
    infra <- "shade-structures"
    scenario <- "all-parks"
    scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
    
    run_shade_scenario()
  }
  
}



# download tiles

# run CTCM on scenarios

# upload data to s3




