library(here)
library(terra)
library(sf)
library(tidyverse)
library(glue)
library(geoarrow)
library(sfarrow)

# Sign in to AWS ----------------------------------------------------------

# system("aws sso login --profile cities-data-dev")
# Sys.setenv(AWS_PROFILE = "cities-data-dev",
#            AWS_DEFAULT_REGION = "us-east-1",
#            AWS_SDK_LOAD_CONFIG = "1")
# 
# s3 <- paws::s3()
# bucket <- "wri-cities-tcm"
# aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
# 
# # Process baseline tile data ----------------------------------------------
# city <- "ZAF-Cape_Town"
# aoi_name <- "urban_extent"
# city_folder <- glue("city_projects/{city}/{aoi_name}")
# 
# open_urban_aws_http <- glue("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}")
# 
# baseline_name <- "wholecity_start"
# infra_name <- "cool-roofs"
# scenario_name <- "all-buildings"
# 
# baseline_folder <- glue("{city_folder}/scenarios/baseline/{baseline_name}")
# scenario_folder <- glue("{city_folder}/scenarios/{infra_name}/{scenario_name}")
# 
# # Make scenario folder
# ensure_s3_prefix(bucket, scenario_folder)
# 
# # Get tile ids
# t <- s3$list_objects_v2(
#   Bucket = bucket,
#   Prefix = baseline_folder
# )

# if (is.null(t$Contents)) {
#   tiles <- character(0)
# } else {
#   tiles <- t$Contents %>%
#     map_chr("Key") %>%
#     str_extract("tile_\\d{5}") %>%  
#     { .[!is.na(.)] } %>%
#     unique() %>%
#     sort()
# }



# aoi <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/urban_extent_boundary.geojson"))
# buffered_tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/tile_grid.geojson"))
# tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/unbuffered_tile_grid.geojson"))



source(here("tiling-scripts", "utils.R"))

update_albedo <- function(tile_idx){
  
  tile <- tile_grid %>% 
    filter(tile_name == tile_idx)
  
  # Load data
  albedo <- rast(glue("{aws_http}/{baseline_folder}/{tile_idx}/raster_files/cif_albedo_cloud_masked.tif"))
  # lulc <- rast(glue("{aws_http}/{baseline_folder}/{tile_id}/raster_files/cif_open_urban.tif"))
  
  # Get buildings
  # TODO: fix geoparquet so that we can load with a bbox
  buildings_path <- glue("{open_urban_aws_http}/buildings/buildings_all.parquet")
  buildings <- st_read_parquet(buildings_path, quiet = TRUE) %>% 
    st_filter(tile) %>% 
    mutate(area_m2 = as.numeric(units::set_units(st_area(.), m^2)))
  
  # Buildings are considered high-slope if they are less than 821 m2 in size
  buildings <- buildings %>% 
    mutate(slope = if_else(area_m2 > 821, "low", "high"),
           alb   = if_else(slope == "low", 0.62, 0.28))
  
  # RAsterize buildings
  build_rast <- rasterize(buildings, albedo, field = "alb",
                        touches = TRUE, background = NA)
  
  # Mask albedo
  albedo <- cover(build_rast, albedo)
  
  # Ensure prefix
  ensure_s3_prefix(bucket, glue("{scenario_folder}/{tile_idx}/raster_files"))
  
  # Write raster
  write_s3(albedo, glue("{bucket}/{scenario_folder}/{tile_idx}/raster_files/albedo.tif"))
}

# map(tile_grid$tile_name, ~ update_albedo(.)) 
