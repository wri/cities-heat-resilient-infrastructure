calc_street_park_shade_metrics <- function(city, scenario, infrastructure, met_data){

  library(terra)
  library(tidyverse)
  library(sf)
  library(here)
  
  infrastructure_path <- here("data", city, "scenarios", infrastructure)
  
  if (scenario == "baseline"){
    scenario_path <- here("data", city, "scenarios", scenario)
    
    albedo <- rast(here(infrastructure_path, 'albedo.tif')) 
  } else {
    scenario_path <- here(infrastructure_path, scenario)
    
    # Load albedo 
    albedo <- rast(here(scenario_path, 'scenario-albedo.tif')) 
  }
  
  # Load UTCI function
  source(here("utils", "utci.R"))
  
  # Load AOI
  aoi <- st_read(here("data", city, "aoi.geojson"))
  
  
  
  # Load LULC
  lulc <- rast(here("data", city, "open-urban.tif")) 
  
  # Define classification function
  reclass_fun <- function(x) {
    ifelse(x == 300 | (x >= 600 & x < 700), 0, 1)
  }
  
  # Apply function
  lulc_reclass <- app(lulc, reclass_fun)
  
  # Initialize results list
  results <- tibble()
  
  # scenario_base <- here(scenario_path, scenario)
  timestamps <- list.files(scenario_path, pattern = "Tmrt") %>% 
    str_extract("(?<=Tmrt_).*(?=\\.tif)")
  
  utci_files <- list.files(scenario_path, pattern = "UTCI")
  Tmrt_files <- list.files(scenario_path, pattern = "Tmrt")
  
  # Load AOI 
  aoi <- st_read(here("data", city, "aoi.geojson"))
  
  for (t in timestamps) {
    
    # Compute UTCI if the file doesn't already exist
    if (any(str_detect(utci_files, t))) {
      
      utci_rast <- rast(here(scenario_path, utci_files[str_detect(utci_files, t)]))  # Load existing UTCI raster
      
    } else {
      
      Tmrt <- rast(here(scenario_path, Tmrt_files[str_detect(Tmrt_files, t)]))
      utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = t, met_data = met_data)
      
      writeRaster(utci_rast, here(scenario_path, paste0("UTCI_", t, ".tif")))
      
    }
    
    # Mask UTCI to AOI
    utci_rast <- mask(utci_rast, aoi)
    
    # Compute metrics
    # utci in people areas
    utci_avg <- mean(values(mask(utci_rast, lulc_reclass, maskvalues = 0) %>% subst(0, NA)), na.rm = TRUE)

    # Albedo of AOI
    albedo_avg <- mean(values(mask(crop(albedo, aoi), aoi) %>% subst(0, NA)), na.rm = TRUE)
    
    # Area and number of buildings updates if any
    if (scenario == "baseline"){
      
      build_no <- 0
      build_area_m2 <- 0
      
    } else {
      
      buildings <- st_read(here(scenario_path, "buildings.geojson"))
      
      build_no <- nrow(buildings)
      build_area_m2 <- sum(buildings$area_sqm)
      
    }
    
    
    # Store results
    metrics <- tibble(
      cities = city,
      date = str_extract(t, ".*(?=_[^_]+$)"),
      time = str_extract(t, "(?<=_)[^_D]+(?=D$)"),
      scenarios = paste(str_replace(infrastructure, "-", "_"), scenario, time, sep = "_"),
      utci_mean_person_areas = utci_avg,
      albedo_mean_aoi = albedo_avg,
      updated_buildings_n_aoi = build_no,
      updated_buildings_area_m2_aoi = build_area_m2
    )
    
    results <- bind_rows(results, metrics)
  }
  
  return(results)

}
