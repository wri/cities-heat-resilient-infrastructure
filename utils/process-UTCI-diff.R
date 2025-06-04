library(here)
library(tidyverse)
library(terra)

city <- "ZAF-Cape_Town"
scenario_new <- rast(here("data", city, "scenarios", "street-trees", "achievable-90pctl", "scenario-new-trees.tif"))
scenario_all <- rast(here("data", city, "scenarios", "street-trees", "achievable-90pctl", "scenario-tree-canopy-height.tif"))

writeRaster((scenario_new >= 3),
            here("data", city, "scenarios", "street-trees", "achievable-90pctl", "scenario-new-tree-cover.tif"))

writeRaster((scenario_all >= 3),
            here("data", city, "scenarios", "street-trees", "achievable-90pctl", "scenario-tree-cover.tif"))


scenario_UTCI1500 <- rast(here("data", city, "scenarios", "street-trees", "achievable-90pctl", "UTCI_2023_1_1800D.tif"))
base_UTCI1500 <- rast(here("data", city, "scenarios", "baseline", "UTCI_2023_1_1800D.tif"))

# Load UTCI function
source(here("utils", "utci.R"))

mrt_rast <- rast(here("data", city, "scenarios", "baseline", "Tmrt_2023_1_1200D.tif"))
timestamp <- "1200"
met_data <- read_delim((here("data", city, "scenarios", "baseline", "met_era5_hottest_days.txt")))
utci <- create_utci(mrt_rast, timestamp, met_data)


scenario_path <- here("data", city, "scenarios", "baseline")

# scenario_base <- here(scenario_path, scenario)
timestamps <- list.files(scenario_path, pattern = "Tmrt") %>% 
  str_extract("(?<=Tmrt_).*(?=\\.tif)")

utci_files <- list.files(scenario_path, pattern = "UTCI")
Tmrt_files <- list.files(scenario_path, pattern = "Tmrt")
shadow_files <- list.files(scenario_path, pattern = "Shadow")


for (time in timestamps) {
  
  # Compute UTCI if the file doesn't already exist
  if (any(str_detect(utci_files, time))) {
    
    utci_rast <- rast(here(scenario_path, utci_files[str_detect(utci_files, time)]))  # Load existing UTCI raster
    
  } else {
    
    Tmrt <- rast(here(scenario_path, Tmrt_files[str_detect(Tmrt_files, time)]))
    utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = time, met_data = met_data)
    
    writeRaster(utci_rast, here(scenario_path, paste0("UTCI_", time, ".tif")))
    
  }
}


for (time in c(1200, 1500, 1800)){
  
  base <- rast(here("data", city, "scenarios", "baseline", paste0("UTCI_2023_1_", time, "D.tif")))
  scen <- rast(here("data", city, "scenarios", "street-trees", "achievable-90pctl", paste0("UTCI_2023_1_", time, "D.tif"))) %>% 
    crop(base)
  
  diff <- scen - base
  writeRaster(diff, here("data", city, "scenarios", "street-trees", "achievable-90pctl", paste0("baseline_scenario_diff_UTCI_2023_1_", time, "D.tif")))
  
}
