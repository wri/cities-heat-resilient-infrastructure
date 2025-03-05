library(terra)
library(tidyverse)
library(aws.s3)
library(here)


# README ------------------------------------------------------------------

# You must set the system environment (https://www.rdocumentation.org/packages/aws.s3/versions/0.3.21) 
# to contain the AWS keys (https://d-9067428136.awsapps.com/start/ , click on the 
# account number and then access keys

# Load UTCI function
source(here("utils", "utci.R"))

# Define paths
aws_base_path <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_town/scenarios/street-trees"
tree_canopy_base <- "ZAF-Cape_town/scenarios/street-trees/ZAF_Capetown_trees_3trees_v1/primary_data/raster_files"
results_base <- "ZAF-Cape_town/scenarios/street-trees/ZAF_Capetown_trees_3trees_v1/tcm_results_umep/met_20jan2022"

# Define scenarios and timesteps
scenarios <- c("baseline" = "tile_001", 
               "achievable" = "tile_002", 
               "technical" = "tile_003")
timesteps <- c("1200D", "1500D", "1800D")

# Load pedestrian area raster
ped_area_rast <- rast(paste0(aws_base_path, "/rasters/ped-area.tif"))
pedestrian_area <- sum(values(ped_area_rast) != 0)

# Initialize results list
results <- list()

# Iterate over scenarios and timesteps
for (scenario in names(scenarios)) {
  
  tile <- scenarios[[scenario]]
  
  for (time in timesteps) {
    
    scenario_path <- paste0(results_base, "/", tile)
    utci_path <- paste0(scenario_path, "/UTCI_2022_20_", time, ".tif")
    
    # Compute UTCI
    if (head_object(utci_path, bucket = "wri-cities-heat") == TRUE) {
      
      utci_rast <- rast(paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/", utci_path))  # Load existing UTCI raster
      
    } else {
      
      utci_rast <- create_utci(paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/", scenario_path, "/Tmrt_2022_20_", time, ".tif"),
                               paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/", scenario_path, "/metforcing.txt"))
      
      s3write_using(utci_rast, FUN = writeRaster,
                    object = utci_path,
                    bucket = 'wri-cities-heat')  # Save new UTCI raster
      
    }
    
    # Load rasters
    shade_rast <- rast(paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/", scenario_path, "/Shadow_2022_20_", time, ".tif")) < 1
    trees_rast <- rast(paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/", tree_canopy_base, "/", tile, "/tree_canopy.tif")) >= 1
    
    # Compute metrics
    tree_pct <- sum(values(mask(trees_rast, ped_area_rast, maskvalues = 0) %>% subst(NA, 0)) != 0) / pedestrian_area
    utci_avg <- mean(values(mask(utci_rast, ped_area_rast, maskvalues = 0) %>% subst(0, NA)), na.rm = TRUE)
    shade_pct <- sum(values(mask(shade_rast, ped_area_rast, maskvalues = 0) %>% subst(NA, 0)) != 0) / pedestrian_area
    
    # Store results
    results <- append(results, list(data.frame(
      scenario = scenario,
      time = time,
      tree_pct = tree_pct,
      utci_avg = utci_avg,
      shade_pct = shade_pct
    )))
  }
}

# Convert results list to dataframe
results_df <- bind_rows(results)

# Save results
s3write_using(results_df, FUN = write_csv,
              object = "ZAF-Cape_town/scenarios/street-trees/metrics.csv",
              bucket = 'wri-cities-heat') 
