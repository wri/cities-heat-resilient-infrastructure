run_CTCM_baseline <- function(city_folder, aoi_file, ctcm_run, author, utc_offset, tile_size, buffer){
  
  library(R.utils)
  library(here)
  library(tidyverse)
  library(terra)
  library(sf)
  library(yaml)
  library(withr)
  
  
  # Setup CTCM data ---------------------------------------------------------
  
  
  ctcm_setup_path <- file.path("C:", "CTCM_data_setup")
  template <- file.path("C:", "CTCM_data_setup", "ZZZ_template_city")
  
  # Create setup folder for new run
  run_setup_folder <- file.path(ctcm_setup_path, paste0(city_folder, "-", ctcm_run))
  unlink(run_setup_folder, recursive = TRUE)
  
  copyDirectory(template, run_setup_folder, overwrite = TRUE)
  
  # Path to tile folder
  tile_folder <- file.path(run_setup_folder, "primary_data", "raster_files", "tile_001")
  
  # get bounding coordinates
  aoi <- st_read(aoi_file) %>% 
    st_transform(4326)
  
  bbox <- aoi %>% 
    st_bbox() 
  
  bbox_df <- data.frame(
    name = names(bbox),
    value = as.numeric(bbox)
  )

  write_csv(bbox_df, here("data", city_folder, "coords.csv"))
  
  
  # Update the yaml file ----------------------------------------------------
  
  
  # Modify yaml file
  yaml_path <- file.path(run_setup_folder, ".config_method_parameters.yml")
  baseline_yaml <- suppressWarnings(read_yaml(yaml_path))
  
  # run metadata
  baseline_yaml[[1]]$short_title <- ctcm_run
  # baseline_yaml[[1]]$version <- version
  # baseline_yaml[[1]]$description <- description
  baseline_yaml[[1]]$author <- author
  
  # bounds
  baseline_yaml[[2]]$utc_offset <- utc_offset
  baseline_yaml[[2]]$min_lon <- bbox["xmin"]
  baseline_yaml[[2]]$min_lat <- bbox["ymin"]
  baseline_yaml[[2]]$max_lon <- bbox["xmax"]
  baseline_yaml[[2]]$max_lat <- bbox["ymax"]
  
  # tiling (optional)
  baseline_yaml[[2]]$tile_side_meters <- tile_size
  
  # buffer
  baseline_yaml[[2]]$tile_buffer_meters <- buffer
  
  # no clipping
  baseline_yaml[[2]]$remove_mrt_buffer_for_final_output <- "False"
  
  # Met file
  baseline_yaml[[3]]$MetFiles <- baseline_yaml[[3]]$MetFiles[-1]
  # baseline_yaml[[3]]$MetFiles <- "None"
  
  # filenames
  baseline_yaml[[4]]$dem_tif_filename <- "None"
  baseline_yaml[[4]]$dsm_tif_filename <- "None"
  baseline_yaml[[4]]$lulc_tif_filename <- "None"
  baseline_yaml[[4]]$open_urban_tif_filename <- "None"
  baseline_yaml[[4]]$tree_canopy_tif_filename <- "None"
  
  # Set sampling_local_hours as a verbatim string
  baseline_yaml[[6]]$solweig$sampling_local_hours <- "12,15,18"
  class(baseline_yaml[[6]]$solweig$sampling_local_hours) <- "verbatim"
  
  # Define custom handler to write without quotes
  verbatim_handler <- function(x) {
    x  # return string directly with no quotes
  }
  
  # Write YAML with custom handler
  write_yaml(baseline_yaml, yaml_path, handlers = list(verbatim = verbatim_handler))
  
  # Run CTCM bat script
  with_dir(run_setup_folder, {
    system(file.path(run_setup_folder, "b_run_CTCM_processing.bat"), wait = TRUE)
  })
  
  # Then continue with the rest of your R script
  message("CTCM processing complete. Copying ouput files to scenario folders...")
  
  
  # After CTCM runs... ------------------------------------------------------
  
  ctcm_output_path <- file.path("C:", "CTCM_outcome", paste0(city_folder, "-", ctcm_run))
  
  baseline_folder <- here("data", city_folder, "scenarios", "baseline")
  
  if(!dir.exists(baseline_folder)){
    dir.create(baseline_folder, recursive = TRUE)
  }
  
  # Copy baseline layers to city data folder
  baseline_layers <- list.files(path = Sys.glob(here(ctcm_output_path, "*", "primary_data", "raster_files", "tile_001")),
                                full.names = TRUE, recursive = TRUE)
  
  file.copy(from = baseline_layers, to = here("data", city_folder), overwrite = TRUE)
  
  # Copy processed data to baseline folder
  processed_data <- list.files(path = Sys.glob(here(ctcm_output_path, "*", "processed_data", "tile_001")),
                               full.names = TRUE)
  
  file.copy(from = processed_data, to = baseline_folder, overwrite = TRUE)
    
  # Copy CTCM output to scenario folder
  output_data <- list.files(path = Sys.glob(here(ctcm_output_path, "*", "tcm_results_umep", "met_era5_hottest_days", "tile_001")),
                            full.names = TRUE) %>%
    keep(~ str_detect(.x, "Shadow|Tmrt") &
           !str_detect(.x, "Tmrt_average"))
  
  # Rename files
  new_filenames <- output_data %>%
    map_chr(~ {
      basename(.x) %>%
        str_match("^(Shadow|Tmrt)_.*_(\\d{4})D\\.tif$") %>%
        { 
          prefix <- ifelse(.[2] == "Shadow", "shade", .[2])
          paste0(prefix, "_", .[3], "_baseline.tif")
        }
    })
  
  
  # Create full destination paths
  new_paths <- file.path(baseline_folder, new_filenames)
  
  # Copy files with new names
  file.copy(from = output_data, to = new_paths, overwrite = TRUE)
  
  # Copy met file
  met_file <- list.files(path = Sys.glob(here(ctcm_output_path, "*", "primary_data", "met_files")),
                         full.names = TRUE)
  file.copy(met_file, baseline_folder, overwrite = TRUE)
  
  # Rename openurban file
  file.rename(from = here("data", city_folder, "cif_open_urban.tif"), to = here("data", city_folder, "open-urban.tif"))

}
  
  
  
  
  
  

