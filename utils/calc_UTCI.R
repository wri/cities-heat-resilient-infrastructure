calc_UTCI <- function(city, scenario, infrastructure){
  
  library(terra)
  library(tidyverse)
  library(sf)
  library(here)
  
  source(here("utils", "utci.R"))
  
  met_data <- list.files(here("data", city, "scenarios", "baseline"), pattern = "met", full.names = TRUE) %>%
    first() %>% 
    read_delim()
  
  baseline_path <- here("data", city, "scenarios", "baseline")
  scenario_path <- here("data", city, "scenarios", infrastructure, scenario)
  
  # scenario_base <- here(scenario_path, scenario)
  timestamps <- list.files(scenario_path, pattern = "Tmrt", recursive = FALSE) %>% 
    str_extract("(?<=Tmrt_).*(?=\\.tif)") %>% 
    unique()
  
  if (infrastructure == "street-trees"){
    infra_file_name <- "street_trees"
  } else if (infrastructure == "park-shade-structures"){
    infra_file_name <- "park_shade"
  } else if (infrastructure == "cool-roofs"){
    infra_file_name <- "cool_roofs"
  }
  
  for (time in timestamps) {
    
    time_str <- str_sub(str_remove(time, "D"), -4)
    
    # Compute UTCI if the file doesn't already exist
      
    Tmrt <- rast(here(scenario_path, paste0("Tmrt_", time, ".tif")))
    scenario_utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = time, met_data = met_data)
    
    writeRaster(scenario_utci_rast, 
                here(scenario_path, paste0("utci_", time_str, "_", infra_file_name, "_achievable", ".tif")),
                overwrite = TRUE)
      
    
    baseline_utci_rast <- rast(here(baseline_path, paste0("UTCI_", time, ".tif"))) 
    writeRaster(baseline_utci_rast, 
                here(baseline_path, paste0("utci_baseline_", time_str, ".tif")),
                overwrite = TRUE)
    
    scenario_utci_rast <- scenario_utci_rast %>% 
      crop(baseline_utci_rast)
    
    diff_utci <- scenario_utci_rast - baseline_utci_rast
    writeRaster(diff_utci,
                here(scenario_path,
                     paste0("utci_", time_str, "_", infra_file_name, "_achievable_vs_baseline", ".tif")),
                overwrite = TRUE)
    
    # Load shade raster and mask to AOI
    baseline_shade_rast <- rast(here(baseline_path, paste0("Shadow_", time, ".tif"))) 
    writeRaster(baseline_shade_rast, 
                here(baseline_path, paste0("shade_baseline_", time_str, ".tif")),
                overwrite = TRUE)
    baseline_shade_rast <- baseline_shade_rast < 1
    
    scenario_shade_rast <- rast(here(scenario_path, paste0("Shadow_", time, ".tif"))) 
    writeRaster(scenario_shade_rast, 
                here(scenario_path, paste0("shade_", time_str, "_", infra_file_name, "_achievable", ".tif")),
                overwrite = TRUE)
    scenario_shade_rast <- scenario_shade_rast < 1
    
    scenario_shade_rast <- scenario_shade_rast %>% 
      crop(baseline_shade_rast)
    
    diff_shadow <- scenario_shade_rast - baseline_shade_rast
    writeRaster(diff_shadow, 
                here(scenario_path, 
                     paste0("shade_", time_str, "_", infra_file_name, "_achievable_vs_baseline", ".tif")),
                overwrite = TRUE)
  }
}


