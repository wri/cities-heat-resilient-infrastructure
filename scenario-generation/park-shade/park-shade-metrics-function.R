calc_street_park_shade_metrics <- function(city, scenario, infrastructure, met_data){

  library(terra)
  library(tidyverse)
  library(sf)
  library(here)
  
  infrastructure_path <- here("data", city, "scenarios", infrastructure)
  
  if (scenario == "baseline"){
    scenario_path <- here("data", city, "scenarios", scenario)
  } else {
    scenario_path <- here(infrastructure_path, scenario)
  }
  
  # Load UTCI function
  source(here("utils", "utci.R"))
  
  # Load parks 
  parks <- st_read(here(infrastructure_path, "parks.geojson"))
  
  # Initialize results list
  results <- tibble()
  
  # scenario_base <- here(scenario_path, scenario)
  timestamps <- list.files(scenario_path, pattern = "Tmrt") %>% 
    str_extract("(?<=Tmrt_).*(?=\\.tif)")
  
  utci_files <- list.files(scenario_path, pattern = "UTCI")
  Tmrt_files <- list.files(scenario_path, pattern = "Tmrt")
  shadow_files <- list.files(scenario_path, pattern = "Shadow")
  
  # Load AOI 
  aoi <- st_read(here("data", city, "aoi.geojson"))
  
  for (time in timestamps) {
    
    # Compute UTCI if the file doesn't already exist
    if (any(str_detect(utci_files, time))) {
      
      utci_rast <- rast(here(scenario_path, utci_files[str_detect(utci_files, time)]))  # Load existing UTCI raster
      
    } else {
      
      Tmrt <- rast(here(scenario_path, Tmrt_files[str_detect(Tmrt_files, time)]))
      utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = time, met_data = met_data)
      
      writeRaster(utci_rast, here(scenario_path, paste0("UTCI_", time, ".tif")))
      
    }
    
    # Load shade raster and mask to AOI
    shade_rast <- rast(here(scenario_path, shadow_files[str_detect(shadow_files, time)])) < 1
    shade_rast <- mask(shade_rast, aoi)
    
    # Mask UTCI to AOI
    utci_rast <- mask(utci_rast, aoi)
    
    # Compute metrics
    
    # shade in parks, 1 = shade
    shade_in_parks <- mask(crop(shade_rast, parks), parks)
    shade_pct <- mean(values(shade_in_parks), na.rm = TRUE)
    
    # distance to shade
    distance_to_shade <- distance(shade_in_parks, target = 0)
    average_distance <- mean(values(distance_to_shade), na.rm = TRUE)
    max_distance <- max(values(distance_to_shade), na.rm = TRUE)
    
    # utci in parks
    utci_avg <- mean(values(mask(crop(utci_rast, parks), parks)), na.rm = TRUE)
    
    # Store results
    metrics <- tibble(
      scenario = scenario,
      infrastructure = infrastructure,
      time = str_remove(time, "D"),
      utci_avg = utci_avg,
      shade_pct = shade_pct,
      shade_mean_dist = average_distance,
      shade_max_dist = max_distance
    )
    
    results <- bind_rows(results, metrics)
  }
  
  return(results)

}
