library(R.utils)
library(here)
library(tidyverse)
library(terra)
library(sf)
library(yaml)

city = "MEX-Monterrey"
ctcm_run = "baseline"
author = "ejwesley@wri.org"
utc_offset = -6

ctcm_setup_path <- file.path("C:", "CTCM_data_setup")
template <- file.path("C:", "CTCM_data_setup", "ZZZ_template_city")

# Create setup folder for new run
run_setup_folder <- file.path(ctcm_setup_path, paste0(city, "-", ctcm_run))
unlink(run_setup_folder, recursive = TRUE)
copyDirectory(template, run_setup_folder, overwrite = TRUE)

# Path to tile folder
tile_folder <- file.path(run_setup_folder, "primary_data", "raster_files", "tile_001")

# get bounding coordinates
scenario_rast <- rast(file.path(tile_folder, "cif_lulc.tif"))

bbox <- as.polygons(ext(scenario_rast), crs = terra::crs(scenario_rast)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  st_bbox()

bbox_df <- data.frame(
  name = names(bbox),
  value = as.numeric(bbox)
  )
write_csv(bbox_df, here("data", city, "coords.csv"))

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

# buffer
scenario_yaml[[2]]$tile_buffer_meters <- buffer

# Specify the met file
scenario_yaml[[3]]$MetFiles <- scenario_yaml[[3]]$MetFiles[1]
scenario_yaml[[3]]$MetFiles[[1]]$filename <- "reduced_temps.txt"

# filenames
scenario_yaml[[4]]$dem_tif_filename <- "cif_dem.tif"
scenario_yaml[[4]]$dsm_tif_filename <- "cif_dsm_ground_build.tif"
scenario_yaml[[4]]$lulc_tif_filename <- "cif_lulc.tif"
scenario_yaml[[4]]$tree_canopy_tif_filename <- "cif_tree_canopy.tif"

scenario_yaml[[5]]$skyview_factor_filename <- "ctcm_svfs.zip"
scenario_yaml[[5]]$wall_aspect_filename <- "ctcm_wallaspect.tif"
scenario_yaml[[5]]$wall_height_filename <- "ctcm_wallheight.tif"

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
ctcm_output_path <- file.path("C:", "CTCM_outcome", paste0(city, "-cool-roofs-", scenario_name))

scenario_folder <- here("data", city, "scenarios", "cool-roofs", scenario_name)

if(!dir.exists(scenario_folder)){
  dir.create(scenario_folder)
}

# Copy CTCM output to scenario folder
output_data <- list.files(path = Sys.glob(here(ctcm_output_path, "*", "tcm_results_umep", "reduced_temps", "tile_001")),
                          full.names = TRUE) %>%
  keep(~ str_detect(.x, "Shadow|Tmrt") &
         !str_detect(.x, "Tmrt_average"))

file.copy(from = output_data, to = scenario_folder)




