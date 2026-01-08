# Download CTCM data for scenarios
library(here)
library(tidyverse)
library(terra)
library(sf)
library(yaml)

download_tree_data <- function(city, infra, scenario, baseline_folder, scenario_folder, tiles){
  
  # Create folder
  ctcm_setup_path <- file.path("~", "CTCM_data_setup")
  template <- file.path(ctcm_setup_path, "ZZZ_template_city")
  
  # Create setup folder for new run
  run_setup_folder <- file.path(ctcm_setup_path, glue("{city}_{infra}_{scenario}"))
  R.utils::copyDirectory(template, run_setup_folder, overwrite = TRUE)
  
  unlink(file.path(run_setup_folder, "primary_data", "raster_files", "tile_00001"),
         recursive = TRUE,
         force = TRUE)
  
  for (t in tiles){
    
    # copy baseline files
    baseline_tile <- glue("{baseline_folder}/{t}")
    scenario_tile <- glue("{scenario_folder}/{t}")
    base_files <- c(
      # Baseline layers
      glue("{baseline_tile}/raster_files/cif_albedo_cloud_masked.tif"),
      glue("{baseline_tile}/raster_files/cif_dem.tif"),
      glue("{baseline_tile}/raster_files/cif_dsm_ground_build.tif"),
      glue("{baseline_tile}/raster_files/cif_lulc.tif"),
      glue("{baseline_tile}/raster_files/cif_open_urban.tif"),
      
      # scenario layers
      glue("{scenario_tile}/raster_files/tree_canopy.tif")
    )
    
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = base_files,
      local_dir = glue("{run_setup_folder}/primary_data/raster_files/{t}")
    )
    
    intermediate_files <- c(
      # intermediate layers
      glue("{baseline_tile}/processed_data/ctcm_wallaspect.tif"),
      glue("{baseline_tile}/processed_data/ctcm_wallheight.tif")
    )
    
    dir.create(glue("{run_setup_folder}/processed_data/{t}"), recursive = TRUE)
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = intermediate_files,
      local_dir = glue("{run_setup_folder}/processed_data/{t}")
    )
  }
  
  met_file <- glue("{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv")
  download_s3_files(
    bucket    = "wri-cities-tcm",
    s3_keys   = met_file,
    local_dir = glue("{run_setup_folder}/primary_data/met_files")
  )
}

download_shade_data <- function(city, infra, scenario, baseline_folder, scenario_folder, tiles){
  
  # Create folder
  ctcm_setup_path <- file.path("~", "CTCM_data_setup")
  template <- file.path(ctcm_setup_path, "ZZZ_template_city")
  
  # Create setup folder for new run
  run_setup_folder <- file.path(ctcm_setup_path, glue("{city}_{infra}_{scenario}"))
  R.utils::copyDirectory(template, run_setup_folder, overwrite = TRUE)
  
  unlink(file.path(run_setup_folder, "primary_data", "raster_files", "tile_00001"),
         recursive = TRUE,
         force = TRUE)
  
  for (t in tiles){
    
    # copy baseline files
    baseline_tile <- glue("{baseline_folder}/{t}")
    scenario_tile <- glue("{scenario_folder}/{t}")
    base_files <- c(
      # Baseline layers
      glue("{baseline_tile}/raster_files/cif_albedo_cloud_masked.tif"),
      glue("{baseline_tile}/raster_files/cif_dem.tif"),
      glue("{baseline_tile}/raster_files/cif_dsm_ground_build.tif"),
      glue("{baseline_tile}/raster_files/cif_lulc.tif"),
      glue("{baseline_tile}/raster_files/cif_open_urban.tif"),
      
      # scenario layers
      glue("{scenario_tile}/ccl_layers/structures-as-trees.tif")
    )
    
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = base_files,
      local_dir = glue("{run_setup_folder}/primary_data/raster_files/{t}")
    )
    
    intermediate_files <- c(
      # intermediate layers
      glue("{baseline_tile}/processed_data/ctcm_wallaspect.tif"),
      glue("{baseline_tile}/processed_data/ctcm_wallheight.tif")
    )
    
    dir.create(glue("{run_setup_folder}/processed_data/{t}"), recursive = TRUE)
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = intermediate_files,
      local_dir = glue("{run_setup_folder}/processed_data/{t}")
    )
  }
  
  met_file <- glue("{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv")
  download_s3_files(
    bucket    = "wri-cities-tcm",
    s3_keys   = met_file,
    local_dir = glue("{run_setup_folder}/primary_data/met_files")
  )
}


run_tree_CTCM <- function(city, infra, scenario, utc_offset){
  
  run_setup_folder <- file.path(ctcm_setup_path, glue("{city}_{infra}_{scenario}"))
  
  # Modify yaml file
  yaml_path <- file.path(run_setup_folder, "config_method_parameters.yml")
  scenario_yaml <- read_yaml(yaml_path)
  
  # Scenario
  scenario_yaml[[1]]$scenario_id <- scenario
  scenario_yaml[[1]]$infra_id <- infra
  
  # Processing AOI
  scenario_yaml[[2]]$utc_offset <- utc_offset
  scenario_yaml[[2]]$min_lon <- bbox["xmin"]
  scenario_yaml[[2]]$min_lat <- bbox["ymin"]
  scenario_yaml[[2]]$max_lon <- bbox["xmax"]
  scenario_yaml[[2]]$max_lat <- bbox["ymax"]
  
  # MetFiles
  scenario_yaml[[3]]$MetFiles[[1]]$filename <- "met_era5_hottest_days.txt"
  
  # CutomTiffFilenames
  scenario_yaml[[4]]$albedo_cloud_masked_tif_filename <- "cif_albedo_cloud_masked.tif"
  scenario_yaml[[4]]$dem_tif_filename <- "cif_dem.tif"
  scenario_yaml[[4]]$dsm_tif_filename <- "cif_dsm_ground_build.tif"
  scenario_yaml[[4]]$lulc_tif_filename <- "cif_lulc.tif"
  scenario_yaml[[4]]$open_urban_tif_filename <- "cif_open_urban.tif"
  scenario_yaml[[4]]$tree_canopy_tif_filename <- "tree_canopy.tif"
  
  # PreparedIntermediateFilenames
  scenario_yaml[[5]]$wall_aspect_filename <- "ctcm_wallaspect.tif"
  scenario_yaml[[5]]$wall_height_filename <- "ctcm_wallheight.tif"
  
  # MethodAttributes
  # Set sampling_local_hours as a verbatim string
  scenario_yaml[[6]]$solweig$sampling_local_hours <- "12,15,18"
  class(scenario_yaml[[6]]$solweig$sampling_local_hours) <- "verbatim"
  
  # Define custom handler to write without quotes
  verbatim_handler <- function(x) {
    x  # return string directly with no quotes
  }
  
  # Write YAML with custom handler
  write_yaml(scenario_yaml, yaml_path, handlers = list(verbatim = verbatim_handler))
  
}