# system("aws sso login --profile cities-data-dev")
# Sys.setenv(AWS_PROFILE = "cities-data-dev",
#            AWS_DEFAULT_REGION = "us-east-1",
#            AWS_SDK_LOAD_CONFIG = "1")

library(glue)
library(tidyverse)
library(here)
library(sf)
library(terra)

source(here("tiling-scripts", "metrics-functions.R"))
source(here("tiling-scripts", "utils.R"))
source(here("tiling-scripts", "post-processing-functions.R"))

cities <- c("BRA-Recife", "BRA-Fortaleza", "BRA-Florianopolis", "BRA-Campinas", "BRA-Teresina")


for (city in cities){
    print(city)
    city <- "ZAF-Cape_Town"
    aoi_name <- "business_district"
    
    bucket   <- "wri-cities-tcm"
    aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
    open_urban_aws_http <- paste0("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/OpenUrban/", city)
    s3 <- paws::s3()
    
    country <- strsplit(city, "-")[[1]][1]
    
    city_folder     <- file.path("city_projects", city, aoi_name)
    baseline_folder <- file.path(city_folder, "scenarios", "baseline", "baseline")
    
    aoi_path <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson")
    
    tiles_s3 <- list_tiles(paste0("s3://", bucket, "/", baseline_folder))
    
    tile_grid <- st_read(
      paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/unbuffered_tile_grid.geojson"),
      quiet = TRUE
    ) |>
      filter(tile_name %in% tiles_s3)
    
    buffered_tile_grid <- st_read(
      paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/tile_grid.geojson"),
      quiet = TRUE
    ) %>%
      dplyr::filter(tile_name %in% tiles_s3)
    
    utm <- st_crs(tile_grid)
    
    aoi <- st_read(aoi_path, quiet = TRUE) |>
      st_transform(st_crs(utm))
    
    tile_grid_aoi <- tile_grid |> 
      st_filter(aoi) 
    tiles_aoi <- tile_grid_aoi$tile_name
    
    infra <- "trees"
    scenario <- "pedestrian-achievable-90pctl"
    
    process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
    calc_cool_roofs_metrics(city, aoi_name, tiles_aoi, scenario)
    # for (scenario in c("baseline", "pedestrian-achievable-90pctl")){
    # 
    # if (scenario == "pedestrian-achievable-90pctl"){
    #   infra = "trees"
    # } else if (scenario == "baseline"){
    #   infra = "baseline"
    # }
    
    # scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
    # 
    # retain_only_tiles_s3(city,
    #                      aoi_name,
    #                      infra,
    #                      scenario,
    #                      tiles_aoi,
    #                      dry_run = FALSE)
    # 
    # process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
    ###########################
    
  }
  
  
  # baseline_metrics(city, aoi_name, tiles_aoi)
  # calc_street_tree_metrics(city, aoi_name, tiles_aoi, scenario = "custom-n_trees_uniform")
  
  
# }


