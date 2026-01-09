#!/usr/bin/env Rscript
# run_aoi_pipeline.R
#
# Example (single city):
# EC2_TERMINATE_ON_COMPLETE=true Rscript run-scenarios.R \
#   --city BRA-Recife \
#   --aoi_name accelerator_area \
#   --aoi_path "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson" \
#   --scenarios trees,cool-roofs,shade-structures \
#   --copy_from_extent false \
#   --ctcm_only false
#
# Example (multiple cities):
# EC2_TERMINATE_ON_COMPLETE=true Rscript run-scenarios.R \
#   --city "BRA-Recife,BRA-Campinas" \
#   --aoi_name accelerator_area \
#   --aoi_path "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson" \
#   --scenarios trees \
#   --copy_from_extent false \
#   --ctcm_only true

suppressPackageStartupMessages({
  library(optparse)
  library(here)
  library(terra)
  library(sf)
  library(tidyverse)
  library(glue)
})

# ---------------------------------------------------------------------
# CLI arguments
# ---------------------------------------------------------------------
option_list <- list(
  make_option("--city", type = "character",
              help = "City code OR comma-separated list, e.g. BRA-Recife,BRA-Campinas (required)"),
  make_option("--aoi_name", type = "character",
              help = "AOI name, e.g. accelerator_area (required)"),
  make_option("--aoi_path", type = "character",
              help = "Full AOI GeoJSON URL. You may use {city} and {aoi_name} placeholders (required)"),
  make_option("--scenarios", type = "character",
              help = "Comma-separated list: trees,cool-roofs,shade-structures,baseline (required)"),
  make_option("--copy_from_extent", type = "character",
              default = "false",
              help = "true/false. Copy baseline tiles from urban_extent (default: false)"),
  make_option("--generate_scenario_data", type = "character",
              default = "false",
              help = "true/false. If true, only run CTCM step for trees (default: false)")
)

opts <- parse_args(OptionParser(option_list = option_list))

# ---------------------------------------------------------------------
# Enforce required args
# ---------------------------------------------------------------------
required <- c("city", "aoi_name", "aoi_path", "scenarios")
missing <- required[!nzchar(unlist(opts[required]))]
if (length(missing) > 0) {
  stop("Missing required argument(s): ", paste(missing, collapse = ", "))
}

# ---------------------------------------------------------------------
# Parse inputs
# ---------------------------------------------------------------------
cities <- strsplit(opts$city, ",")[[1]] |>
  trimws() |>
  (\(x) x[nzchar(x)])()

aoi_name <- opts$aoi_name
aoi_path_template <- opts$aoi_path

scenarios <- strsplit(opts$scenarios, ",")[[1]] |>
  trimws() |>
  (\(x) x[nzchar(x)])()

copy_from_extent <- tolower(opts$copy_from_extent) %in% c("true", "t", "1", "yes", "y")
generate_scenario_data <- tolower(opts$generate_scenario_data) %in% c("true", "t", "1", "yes", "y")

# ---------------------------------------------------------------------
# Constants / setup
# ---------------------------------------------------------------------
bucket   <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
s3 <- paws::s3()

source(here("tiling-scripts", "utils.R"))

message("Running with:")
message("  cities = ", paste(cities, collapse = ", "))
message("  aoi_name = ", aoi_name)
message("  aoi_path template = ", aoi_path_template)
message("  scenarios = ", paste(scenarios, collapse = ", "))
message("  copy_from_extent = ", copy_from_extent)
message("  generate_scenario_data = ", generate_scenario_data)

# ---------------------------------------------------------------------
# Run serially per city
# ---------------------------------------------------------------------
for (city in cities) {
  
  message("\n============================================================")
  message("CITY: ", city)
  message("============================================================\n")
  
  # Build AOI path (supports placeholders)
  aoi_path <- aoi_path_template
  aoi_path <- gsub("\\{city\\}", city, aoi_path)
  aoi_path <- gsub("\\{aoi_name\\}", aoi_name, aoi_path)
  
  open_urban_aws_http <- paste0(
    "https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/",
    city
  )
  
  # Folder structure
  city_folder     <- file.path("city_projects", city, aoi_name)
  baseline_folder <- file.path(city_folder, "scenarios", "baseline", "baseline")
  
  # Get tile ids from S3
  tiles_s3 <- list_tiles(paste0("s3://", bucket, "/", baseline_folder))
  
  if (length(tiles_s3) == 0) {
    warning("No tiles found for city ", city, " at ", baseline_folder, " — skipping city.")
    next
  }
  
  buffered_tile_grid <- st_read(
    paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/tile_grid.geojson"),
    quiet = TRUE
  ) |>
    filter(tile_name %in% tiles_s3)
  
  tile_grid <- st_read(
    paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/unbuffered_tile_grid.geojson"),
    quiet = TRUE
  ) |>
    filter(tile_name %in% tiles_s3)
  
  # Read AOI
  aoi <- st_read(aoi_path, quiet = TRUE) |>
    st_transform(st_crs(tile_grid))
  
  # Copy baseline tiles from urban extent (optional)
  if (copy_from_extent) {
    
    tile_ids <- tile_grid |>
      st_filter(aoi) |>
      pull(tile_name)
    
    for (t in tile_ids) {
      
      from <- file.path(
        "city_projects", city,
        "urban_extent", "baseline", "baseline", t
      )
      
      to <- file.path(baseline_folder, t)
      
      ensure_s3_prefix(bucket, to)
      s3_copy_vec(from = from, to = to, bucket = bucket)
    }
  }
  
  # Baseline layers
  if ("baseline" %in% scenarios) {
    source(here("tiling-scripts", "baseline-layers.R"))
    save_baseline_layers()
  }
  
  # Tiles intersecting AOI (kept, though you still use tiles_s3 downstream)
  buffered_tile_grid_aoi <- buffered_tile_grid |> st_filter(aoi)
  tile_grid_aoi          <- tile_grid |> st_filter(aoi)
  tiles_aoi              <- buffered_tile_grid_aoi$tile_name
  
  # Trees scenario
  if ("trees" %in% scenarios) {
    
    infra    <- "trees"
    scenario <- "pedestrian-achievable-90pctl"
    scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
    
    source(here("tiling-scripts", "trees-functions.R"))
    source(here("tiling-scripts", "CTCM-functions.R"))
    
    if (generate_scenario_data) {
      run_tree_scenario()
    } 
    
    download_tree_data(
      city            = city,
      infra           = infra,
      scenario        = scenario,
      baseline_folder = baseline_folder,
      scenario_folder = scenario_folder,
      tiles           = tiles_s3
    )
    
    # Run the CTCM
    run_tree_CTCM(city, infra, scenario)
    
    # Upload the data to s3
    upload_CTCM_results_to_s3(city, infra, scenario, aoi_name)
    
  }
  
  # Cool roofs scenario
  if ("cool-roofs" %in% scenarios) {
    
    infra   <- "cool-roofs"
    country <- strsplit(city, "-")[[1]][1]
    
    source(here("tiling-scripts", "cool-roofs-functions.R"))
    source(here("tiling-scripts", "CTCM-functions.R"))
    
    # Generate scenario data
    if (generate_scenario_data) {
      update_albedo()
    }
    
    # Calculate air temperature change
    for (s in c("all-buildings", "large-buildings")){
      calc_air_temp_delta(city, s, aoi)
    }
    
    download_cool_roof_data(city, aoi_name, scenario = "all-buildings", baseline_folder, tiles_s3)
    # Run CTCM
    
    # Upload the data to s3
    upload_CTCM_results_to_s3(city, infra, scenario, aoi_name) 
    
  }
  
  # Shade structures scenario
  if ("shade-structures" %in% scenarios) {
    
    infra    <- "shade-structures"
    scenario <- "all-parks"
    scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
    
    source(here("tiling-scripts", "park-shade-functions.R"))
    source(here("scenario-generation", "park-shade-structures", "shade-generating-functions.R"))
    
    # Generate scenario data
    if (generate_scenario_data) {
      run_shade_scenario()
    }
    
    # Run CTCM
    
    # Upload the data to s3
    upload_CTCM_results_to_s3(city, infra, scenario, aoi_name) 
    
  }
  
  message("\nFinished city: ", city)
}

message("\nAll cities complete.")

# ---------------------------------------------------------------------
# OPTIONAL: terminate instance when finished
# ---------------------------------------------------------------------
if (Sys.getenv("EC2_TERMINATE_ON_COMPLETE") == "true") {
  message("Terminating EC2 instance...")
  system("sudo shutdown -h now")
}
