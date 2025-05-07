

run_CTCM_park_shade_structures <- function(city, author, utc_offset, scenario_name, transmissivity){
  
  
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
  run_setup_folder <- file.path(ctcm_setup_path, paste0(city, "-park-shade-structures-", scenario_name))
  copyDirectory(template, run_setup_folder)
  
   # Copy files
  tile_folder <- file.path(run_setup_folder, "primary_data", "raster_files", "tile_001")
  
  # Baselayers
  baselayers <- file.path(here("data", city), 
                          c("cif_dem.tif", "cif_dsm_ground_build.tif", "cif_lulc.tif"))
  file.copy(from = baselayers, to = tile_folder)
  
  # Wall layers
  wall_layers <- file.path(here("data", city, "scenarios", "baseline"), 
                           c("ctcm_wallheight.tif", "ctcm_wallaspect.tif"))
  dir.create(file.path(run_setup_folder, "processed_data", "tile_001"), 
             recursive = TRUE, showWarnings = FALSE)
  file.copy(from = wall_layers, to = file.path(run_setup_folder, "processed_data", "tile_001"))
  
  # Copy structures as trees layer
  file.copy(from = here("data", city, "scenarios", "park-shade-structures", 
                        scenario_name, "structures-as-trees.tif"),
            to = tile_folder)
  
  # get bounding coordinates
  scenario_rast <- rast(file.path(tile_folder, "cif_lulc.tif")) 
  bbox <- as.polygons(ext(scenario_rast), crs = terra::crs(scenario_rast)) %>% 
    st_as_sf() %>% 
    st_transform(crs = 4326) %>%
    st_bbox()
  
  
  
  # Update the yaml file ----------------------------------------------------
  
  
  
  # Modify yaml file
  yaml_path <- file.path(run_setup_folder, ".config_method_parameters.yml")
  scenario_yaml <- read_yaml(yaml_path)
  
  # run metadata
  scenario_yaml[[1]]$short_title <- scenario_name
  scenario_yaml[[1]]$author <- author
  # 
  # bounds
  scenario_yaml[[2]]$utc_offset <- utc_offset
  scenario_yaml[[2]]$min_lon <- bbox["xmin"]
  scenario_yaml[[2]]$min_lat <- bbox["ymin"]
  scenario_yaml[[2]]$max_lon <- bbox["xmax"]
  scenario_yaml[[2]]$max_lat <- bbox["ymax"]
  
  # Specify the met file
  scenario_yaml[[3]]$MetFiles <- scenario_yaml[[3]]$MetFiles[1]
  scenario_yaml[[3]]$MetFiles[[1]]$filename <- "met_era5_hottest_days.txt"
  
  file.copy(from = here("data", city, "scenarios", "baseline", "met_era5_hottest_days.txt"),
            to = file.path(run_setup_folder, "primary_data", "met_files"))
  
  # filenames
  scenario_yaml[[4]]$dem_tif_filename <- "cif_dem.tif"
  scenario_yaml[[4]]$dsm_tif_filename <- "cif_dsm_ground_build.tif"
  scenario_yaml[[4]]$lulc_tif_filename <- "cif_lulc.tif"
  scenario_yaml[[4]]$tree_canopy_tif_filename <- "structures-as-trees.tif"
  
  scenario_yaml[[5]]$wall_aspect_filename <- "ctcm_wallaspect.tif"
  scenario_yaml[[5]]$wall_height_filename <- "ctcm_wallheight.tif"
  
  # Change transmissivity
  scenario_yaml[[6]]$skyview_factor$transmissivity_of_light_through_vegetation <- transmissivity
  
  # Set sampling_local_hours as a verbatim string
  scenario_yaml[[6]]$solweig$sampling_local_hours <- "12,15,18"
  class(scenario_yaml[[6]]$solweig$sampling_local_hours) <- "verbatim"
  
  # Define custom handler to write without quotes
  verbatim_handler <- function(x) {
    x  # return string directly with no quotes
  }
  
  # Write YAML with custom handler
  write_yaml(scenario_yaml, yaml_path, handlers = list(verbatim = verbatim_handler))
  
  # Run CTCM bat script
  library(withr)
  
  with_dir(run_setup_folder, {
    system(file.path(run_setup_folder, "b_run_CTCM_processing.bat"), wait = TRUE)
  })
  
  # Then continue with the rest of your R script
  message("CTCM processing complete. Copying ouput files to scenario folders...")
  
  
  # After CTCM runs... ------------------------------------------------------
  
  # Copy CTCM output to scenario folder
  ctcm_output_path <- file.path("C:", "CTCM_outcome", paste0(city, "-park-shade-structures-", scenario_name))
  
  scenario_folder <- here("data", city, "scenarios", "park-shade-structures", scenario_name)
  t_folder <- here(scenario_folder, paste0("t", transmissivity))
  
  if(!dir.exists(t_folder)){
    dir.create(t_folder, recursive = TRUE)
  }
  
  # Copy processed data to scenario folder
  processed_data <- list.files(path = Sys.glob(here(ctcm_output_path, "*", "processed_data", "tile_001")),
                               full.names = TRUE)
  
  file.copy(from = processed_data, to = t_folder)

  # Copy CTCM output to scenario folder
  output_data <- list.files(path = Sys.glob(here(ctcm_output_path, "*", "tcm_results_umep", "met_era5_hottest_days", "tile_001")),
                            full.names = TRUE) %>%
    keep(~ str_detect(.x, "Shadow|Tmrt") &
           !str_detect(.x, "Tmrt_average"))
  
  file.copy(from = output_data, to = t_folder)
  
  
  
  
  
}
