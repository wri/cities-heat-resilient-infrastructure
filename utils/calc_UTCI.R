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
  
  for (time in timestamps) {
    
    utci_path <- here(scenario_path, paste0("UTCI_", time, ".tif"))
    
    # Compute UTCI if the file doesn't already exist
    if (file.exists(utci_path)) {
      
      scenario_utci_rast <- rast(utci_path)  # Load existing UTCI raster
      
    } else {
      
      Tmrt <- rast(here(scenario_path, paste0("Tmrt_", time, ".tif")))
      scenario_utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = time, met_data = met_data)
      
      writeRaster(scenario_utci_rast, here(scenario_path, paste0("UTCI_", time, ".tif")))
      
    }
    
    # Load shade raster and mask to AOI
    baseline_shade_rast <- rast(here(baseline_path, paste0("Shadow_", time, ".tif"))) < 1
    scenario_shade_rast <- rast(here(scenario_path, paste0("Shadow_", time, ".tif"))) < 1
    scenario_shade_rast <- scenario_shade_rast %>% 
      crop(baseline_shade_rast)
    
    diff_shadow <- scenario_shade_rast - baseline_shade_rast
    writeRaster(diff_shadow, 
                here(scenario_path, 
                     paste0("shade_diff_", time, ".tif")),
                overwrite = TRUE)
  }
}

calc_UTCI(city, scenario = "large-buildings", infrastructure = "cool-roofs")
