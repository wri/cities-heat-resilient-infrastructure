calc_UTCI <- function(city_folder, scenario, infrastructure = NULL){
  
  library(terra)
  library(tidyverse)
  library(sf)
  library(here)
  
  source(here("utils", "utci.R"))
  
  met_data <- list.files(here("data", city_folder, "scenarios", "baseline"), pattern = "met", full.names = TRUE) %>%
    first() %>% 
    read_delim()
  
  if (scenario == "baseline"){
    
    scenario_path <- here("data", city_folder, "scenarios", "baseline")
  } else {
    
    scenario_path <- here("data", city_folder, "scenarios", infrastructure, scenario)
  }
  
  if (is.null(infrastructure)) {
    # Do nothing
  } else if (infrastructure == "street-trees"){
    infra_file_name <- "street_trees"
  } else if (infrastructure == "park-shade-structures"){
    infra_file_name <- "park_shade"
  } else if (infrastructure == "cool-roofs"){
    infra_file_name <- "cool_roofs"
  } 
  
  tmrt_files <- list.files(scenario_path, pattern = "Tmrt") %>%
    discard(~ str_detect(.x, "\\.aux\\.xml$"))
  
  timestamps <- c("1200", "1500", "1800")
  
  for (time in timestamps) {
    
    file <- tmrt_files %>% keep(~ str_detect(.x, time))
    
    Tmrt <- rast(here(scenario_path, file))
    utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = time, met_data = met_data)
    
    writeRaster(utci_rast, 
                here(scenario_path, str_replace(file, "Tmrt", "utci")),
                overwrite = TRUE)
      
    if (scenario != "baseline"){
      
      # UTCI difference
      baseline_utci_rast <- rast(here("data", city_folder, "scenarios", "baseline", paste0("utci_", time, "_baseline.tif"))) 
      
      utci_rast <- utci_rast %>% 
        crop(baseline_utci_rast)
      
      diff_utci <- utci_rast - baseline_utci_rast
      writeRaster(diff_utci,
                  here(scenario_path,
                       paste0("utci_", time, "_", infra_file_name, "_achievable_vs_baseline", ".tif")),
                  overwrite = TRUE)
      
      baseline_shade_rast <- rast(here("data", city_folder, "scenarios", "baseline", paste0("shade_", time, "_baseline.tif"))) 
      baseline_shade_rast <- baseline_shade_rast < 1
      
      if (infrastructure == "cool-roofs") {
        baseline_alb <- rast(here("data", city_folder, "scenarios", "baseline", "albedo_baseline.tif")) 
        scenario_alb <- rast(here(scenario_path, "albedo_cool_roofs_achievable.tif")) 
        
        diff_alb <- scenario_alb - baseline_alb
        
        writeRaster(diff_alb, here(scenario_path, "albedo_achievable_vs_baseline.tif"), overwrite = TRUE) 
        
      } else {
        scenario_shade_rast <- rast(here(scenario_path,  paste0("shade_", time, "_", infra_file_name, "_achievable", ".tif"))) 
        scenario_shade_rast <- scenario_shade_rast < 1
        
        scenario_shade_rast <- scenario_shade_rast %>% 
          crop(baseline_shade_rast)
        
        diff_shadow <- scenario_shade_rast - baseline_shade_rast
        writeRaster(diff_shadow, 
                    here(scenario_path, 
                         paste0("shade_", time, "_", infra_file_name, "_achievable_vs_baseline", ".tif")),
                    overwrite = TRUE)
      }
    }

    

    
    
  
  }
}


