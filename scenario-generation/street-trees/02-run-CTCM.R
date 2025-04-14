# city <- "MEX-Monterrey"
# 
# ctcm_run <- "test"
# version <- 1
# description <- "Street tree scenarios"
# author <- "elizabeth.wesley@wri.org"
# 
# utc_offset <- -6
# 
# # If you want to use a specific met file save it in the city scenarios folder
# # cities-heat-resilient-infrastructure/data/city/scenarios/met_file_name.txt
# # and reference the file name here
# met_file <- "None"
# # met_file <- "met_era5_hottest_days.txt"
#   
# scenarios <- c("achievable-90pctl")
# baseline <- TRUE

run_CTCM <- function(city, ctcm_run, version, description, author, utc_offset, met_file = "None",
                     scenarios, baseline = TRUE){
  
  library(R.utils)
  library(here)
  library(tidyverse)
  library(terra)
  library(sf)
  library(yaml)
  
  
  # Setup CTCM data ---------------------------------------------------------
  
  
  
  ctcm_setup_path <- file.path("C:", "CTCM_data_setup")
  template <- file.path("C:", "CTCM_data_setup", "ZZZ_template_city")
  
  # Create setup folder for new run
  run_setup_folder <- file.path(ctcm_setup_path, paste0(city, "-", ctcm_run))
  
  copyDirectory(template, run_setup_folder)
  
  # Path to tile folder
  tile_folder <- file.path(run_setup_folder, "primary_data", "raster_files", "tile_001")
  
  tile_mapping <- tibble()
  
  # Copy tree scenario data into tile folders and save mapping
  for (i in seq_along(scenarios)){
    
    tile_num <- paste0("tile_", sprintf("%03d", i))
    
    if (i != 1){
      dir.create(str_replace(tile_folder, "tile_001", tile_num))
    }
    
    # Get tree scenario data
    scenario <- scenarios[i]
    tree_height_path <- here("data", city, "scenarios", "street-trees",
                             scenario, "scenario-tree-canopy-height.tif")
    
    # Copy file
    destination_path <- file.path(run_setup_folder, "primary_data", "raster_files",
                                  tile_num, "tree_canopy.tif")
    copyFile(tree_height_path, destination_path, overwrite = TRUE)
    
    tile_mapping <- tile_mapping %>% 
      bind_rows(tibble(tile = tile_num, scenario = scenario))
    
  }
  
  if (baseline) {
    # Baseline scenario
    baseline_tile <- paste0("tile_", sprintf("%03d", i + 1))
    
    dir.create(str_replace(tile_folder, "tile_001", baseline_tile), showWarnings = FALSE)
    
    copyFile(here("data", city, "tree-canopy-height.tif"), 
             file.path(str_replace(tile_folder, "tile_001", baseline_tile), "tree_canopy.tif"),
             skip = TRUE)
    
    tile_mapping <- tile_mapping %>% 
      bind_rows(tibble(tile = baseline_tile, scenario = "baseline"))
  }
  
  
  # Write tile mapping to scenarios folder
  write_csv(tile_mapping, here("data", city, "scenarios", "street-trees", "tile_mapping.csv"))
  
  # get bounding coordinates
  scenario_rast <- rast(destination_path) 
  bbox <- as.polygons(ext(scenario_rast), crs = terra::crs(scenario_rast)) %>% 
    st_as_sf() %>% 
    st_transform(crs = 4326) %>%
    st_bbox() %>%
    round(digits = 13)
  
  
  
  
  # Update the yaml file ----------------------------------------------------
  
  
  
  # Modify yaml file
  yaml_path <- file.path(run_setup_folder, ".config_method_parameters.yml")
  scenario_yaml <- read_yaml(yaml_path)
  
  # run metadata
  scenario_yaml[[1]]$short_title <- ctcm_run
  scenario_yaml[[1]]$version <- version
  scenario_yaml[[1]]$description <- description
  scenario_yaml[[1]]$author <- author
  
  # bounds
  scenario_yaml[[2]]$utc_offset <- utc_offset
  scenario_yaml[[2]]$min_lon <- bbox["xmin"]
  scenario_yaml[[2]]$min_lat <- bbox["ymin"]
  scenario_yaml[[2]]$max_lon <- bbox["xmax"]
  scenario_yaml[[2]]$max_lat <- bbox["ymax"]
  
  # Specify the met file
  if (met_file == "None"){
    
    scenario_yaml[[3]]$MetFiles <- scenario_yaml[[3]]$MetFiles[-1]
    
  } else {
    
    scenario_yaml[[3]]$MetFiles <- scenario_yaml[[3]]$MetFiles[1]
    scenario_yaml[[3]]$MetFiles[[1]]$filename <- met_file
    
    copyFile(here("data", city, "scenarios", met_file), 
             file.path(run_setup_folder, "primary_data", "met_files", met_file),
             overwrite = TRUE)
    
    
  }
  
  # filenames
  scenario_yaml[[4]]$dem_tif_filename <- "None"
  scenario_yaml[[4]]$dsm_tif_filename <- "None"
  scenario_yaml[[4]]$lulc_tif_filename <- "None"
  scenario_yaml[[4]]$tree_canopy_tif_filename <- "tree_canopy.tif"
  
  # Set sampling_local_hours as a verbatim string
  scenario_yaml[[6]]$solweig$sampling_local_hours <- "12,15,18"
  class(scenario_yaml[[6]]$solweig$sampling_local_hours) <- "verbatim"
  
  # Define custom handler to write without quotes
  verbatim_handler <- function(x) {
    x  # return string directly with no quotes
  }
  
  # Write YAML with custom handler
  write_yaml(scenario_yaml, yaml_path, handlers = list(verbatim = verbatim_handler))
  
  # Run CTCM check bat script
  # setwd(run_setup_folder)
  # shell.exec(file.path(run_setup_folder, "a_run_CTCM_pre_check.bat"))
  
  # Run CTCM bat script
  setwd(run_setup_folder)
  system(file.path(run_setup_folder, "b_run_CTCM_processing.bat"), wait = TRUE)
  
  # Then continue with the rest of your R script
  message("CTCM processing complete. Copying ouput files to scenario folders...")
  
  
  # After CTCM runs... ------------------------------------------------------
  
  tile_mapping <- read_csv(here("data", city, "scenarios", "street-trees", "tile_mapping.csv"))
  
  # Copy CTCM output to scenario folder
  ctcm_output <- file.path("C:", "CTCM_outcome", paste0(city, "-", ctcm_run))
  
  for (row in 1:nrow(tile_mapping)) {
    
    tile <- tile_mapping$tile[row]
    scenario <- tile_mapping$scenario[row]
    
    output_data <- list.files(ctcm_output, recursive = TRUE, full.names = TRUE) %>%
      keep(~ str_detect(.x, "tcm_results_umep") & 
             str_detect(.x, tile) & 
             str_detect(.x, "Shadow|Tmrt") &
             !str_detect(.x, "Tmrt_average"))
    
    # Skip if there are no matching files
    if (length(output_data) == 0) next
    
    scenario_folder <- here("data", city, "scenarios", "street-trees", scenario)
    
    if(!dir.exists(scenario_folder)){
      dir.create(scenario_folder)
    }
    
    # Output data extents are slightly offset from inputs so we shift them to match
    scenario_rast <- rast(here(scenario_folder, "scenario-tree-canopy-height.tif"))
    
    walk(output_data, function(file_path) {
      # Read the raster
      r <- rast(file_path)
      
      # Update extent to match scenario_rast
      ext(r) <- ext(scenario_rast)
      
      # Define destination path (same filename in new folder)
      dest_path <- file.path(scenario_folder, basename(file_path))
      
      # Save the raster
      writeRaster(r, dest_path, overwrite = TRUE)
    })
    
  }
  
  
  
  
  
  
}
