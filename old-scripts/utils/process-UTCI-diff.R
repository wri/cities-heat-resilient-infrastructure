library(here)
library(tidyverse)
library(terra)


# Load UTCI function
source(here("utils", "utci.R"))


met_data <- read_delim((here("data", city, "scenarios", "baseline", "met_era5_hottest_days.txt")))

scenario_path <- here("data", city, "scenarios", "baseline")

# scenario_base <- here(scenario_path, scenario)
timestamps <- list.files(scenario_path, pattern = "Tmrt") %>% 
  str_extract("(?<=Tmrt_).*(?=\\.tif)")

utci_files <- list.files(scenario_path, pattern = "UTCI")
Tmrt_files <- list.files(scenario_path, pattern = "Tmrt")


for (time in timestamps) {
  
  # Compute UTCI if the file doesn't already exist
  if (any(str_detect(utci_files, time))) {
    
    next
    
  } else {
    
    Tmrt <- rast(here(scenario_path, Tmrt_files[str_detect(Tmrt_files, time)]))
    utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = time, met_data = met_data)
    
    writeRaster(utci_rast, here(scenario_path, paste0("UTCI_", time, ".tif")))
    
  }
}

for (time in timestamps){
  
  base_utci <- rast(here("data", city, "scenarios", "baseline", paste0("UTCI_", time, ".tif")))
  scen_utci <- rast(here("data", city, "scenarios", "street-trees", "achievable-90pctl", paste0("UTCI_", time, ".tif"))) %>% 
    crop(base_utci)
  
  diff_utci <- scen_utci - base_utci
  writeRaster(diff_utci, 
              here("data", city, "scenarios", "street-trees", "achievable-90pctl", 
                   paste0("baseline_scenario_diff_UTCI_", time, ".tif")),
              overwrite = TRUE)
  
  base_shadow <- rast(here("data", city, "scenarios", "baseline", paste0("Shadow_", time, ".tif")))
  scen_shadow <- rast(here("data", city, "scenarios", "street-trees", "achievable-90pctl", paste0("Shadow_", time, ".tif"))) %>% 
    crop(base_shadow)
  
  diff_shadow <- scen_shadow - base_shadow
  writeRaster(diff_shadow, 
              here("data", city, "scenarios", "street-trees", "achievable-90pctl", 
                   paste0("baseline_scenario_diff_Shadow_", time, ".tif")),
              overwrite = TRUE)
  
}
 




