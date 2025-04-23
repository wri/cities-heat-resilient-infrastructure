city <- "ZAF-Cape_Town"
infrastructure <- "cool-roofs"
scenario <- "program"
scenario_name <- "large-buildings"
area_threshold = 2000
cool_roof_albedo = 0.62

cool_roof_scenario <- function(city, scenario, scenario_name, infrastructure,
                               area_threshold, cool_roof_albedo){
  
  # Setup ---------------------------------------------------------------------
  
  # Load necessary libraries
  library(terra)
  library(sf)
  library(lidR)
  library(tidyverse)
  library(here)
  
  inputs_path <- here("data", city)
  infrastructure_path <- here(inputs_path, "scenarios", "cool-roofs")
  
  # Create scenario_path
  if (!dir.exists(infrastructure_path)) {
    dir.create(infrastructure_path, recursive = TRUE)
  }
  
  # Load input data
  aoi <- st_read(here(inputs_path, "aoi.geojson"))
  lulc <- rast(here(inputs_path, "open-urban.tif"))
  albedo <- rast(here(inputs_path, "albedo.tif"))
  build_vectors <- st_read(here(inputs_path, "buildings.geojson"))
  # road_vectors <- st_read(here(inputs_path, "roads.geojson"))
  # lanes <- read_csv(here(inputs_path, "average-lanes.csv"))
  # canopy_height_existing <- rast(here(inputs_path, "tree-canopy-height.tif")) 
  # names(canopy_height_existing) <- "height"
  
  
  # Generate scenarios ------------------------------------------------------
  
  source(here("scenario-generation", infrastructure, paste0(scenario_name, "-scenario-function.R"))) 
  
  params <- tibble(
    city = city,
    scenario = scenario, 
    scenario_name = scenario_name, 
    area_threshold = area_threshold,
    cool_roof_albedo = cool_roof_albedo
  )
  
  write_csv(params, here(infrastructure_path, scenario_name, "scenario-params.csv"))
  
  albedo_delta <- large_buildings_scenario_function(scenario_name, infrastructure_path,
                                                    area_threshold, cool_roof_albedo,
                                                    aoi, lulc, albedo, build_vectors)
  
  ###### CALCULATE TEMP CHAGE ######
  # from Krayenhoff et al. 2021 DOI 10.1088/1748-9326/abdcf1
  
  # 0.1 increase in albedo results in a 0.6 C reduction in air temp for midday clear-sky conditiosn

  ###### ADJUST TO 12:00, 15:00, 18:00 TEMP CHANGE ######
  
  dT_12 <- albedo_delta * 6
  dT_15 <- 0.935315 * dT_12
  dT_18 <- 0.646853 * dT_12
  
}






