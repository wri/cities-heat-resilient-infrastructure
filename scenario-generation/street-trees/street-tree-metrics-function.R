

calc_street_tree_metrics <- function(city, scenario_path, met_data){
  
  library(terra)
  library(tidyverse)
  library(here)
  
  # Load UTCI function
  source(here("utils", "utci.R"))
  
  # Load pedestrian area raster
  ped_area_rast <- rast(here(str_remove(scenario_path, "/[^/]+$"), "pedestrian-area.tif"))
  pedestrian_area <- sum(values(ped_area_rast) != 0)
  
  # Initialize results list
  results <- tibble()
    
  # scenario_base <- here(scenario_path, scenario)
  timestamps <- list.files(scenario_path, pattern = "Tmrt") %>% 
    str_extract("(?<=Tmrt_).*(?=\\.tif)")
  
  utci_files <- list.files(scenario_path, pattern = "UTCI")
  Tmrt_files <- list.files(scenario_path, pattern = "Tmrt")
  shadow_files <- list.files(scenario_path, pattern = "Shadow")
  
  # Load updated trees
  tree_rast <- rast(here(scenario_path, "scenario-tree-canopy-height.tif"))
  
  # Load AOI and clip layers
  aoi <- st_read(here("data", city, "aoi.geojson"))
  tree_rast <- mask(tree_rast, aoi)
  
  for (time in timestamps) {
    
    # Compute UTCI if the file doesn't already exist
    if (any(str_detect(utci_files, time))) {
      
      utci_rast <- rast(here(scenario_path, utci_files[str_detect(utci_files, time)]))  # Load existing UTCI raster
      
    } else {
      
      Tmrt <- rast(here(scenario_path, Tmrt_files[str_detect(Tmrt_files, time)]))
      utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = time, met_data = met)
      
      writeRaster(utci_rast, here(scenario_path, paste0("UTCI_", time, ".tif")))
      
    }
    
    # Load shade raster and mask to AOI
    shade_rast <- rast(here(scenario_path, shadow_files[str_detect(shadow_files, time)])) < 1
    shade_rast <- mask(shade_rast, aoi)
    
    # Mask UTCI to AOI
    utci_rast <- mask(utci_rast, aoi)
    
    # Compute metrics
    tree_pct <- sum(values(mask(tree_rast, ped_area_rast, maskvalues = 0) %>% subst(NA, 0)) != 0) / pedestrian_area
    utci_avg <- mean(values(mask(utci_rast, ped_area_rast, maskvalues = 0) %>% subst(0, NA)), na.rm = TRUE)
    shade_pct <- sum(values(mask(shade_rast, ped_area_rast, maskvalues = 0) %>% subst(NA, 0)) != 0) / pedestrian_area
    
    # Store results
    metrics <- tibble(
      scenario = str_extract(scenario_path, "[^/]+$"),
      time = str_remove(time, "D"),
      tree_pct = tree_pct,
      utci_avg = utci_avg,
      shade_pct = shade_pct
    )
    
    results <- bind_rows(results, metrics)
    }
  
  # Save results
  write_csv(results, here(scenario_path, "scenario-metrics.csv"))
  
}

  
  