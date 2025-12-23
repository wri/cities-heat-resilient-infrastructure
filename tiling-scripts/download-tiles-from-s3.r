#!/usr/bin/env Rscript


# Run example -------------------------------------------------------------



# Rscript download-tiles-from-s3.r \
#   "https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Durban/ZAF-Durban__inner_city_lap.geojson" \
#   "inner_city_lap" \
#   "ZAF-Durban" \
#   "trees" \
#   "pedestrian-achievable-90pctl" \
#   FALSE \
#   FALSE \
#   TRUE


# -------------------------------------------------------------------------


args <- commandArgs(trailingOnly = TRUE)
aoi_path <-       args[1]
aoi_name <-       args[2]
city     <-       args[3]
infra    <-       args[4]
scenario <-       args[5]
from_urban_extent <- args[6]
s3_copy <-        args[7]
local_download <- args[8]



if (any(is.na(c(aoi_path, aoi_name, city, infra, scenario)))) {
  stop("Usage: download-tiles-from-s3.R <aoi_path> <aoi_name> <city> <infra> <scenario>")
}

pkgs <- c("sf", "glue", "here")

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(pkgs, library, character.only = TRUE))

source(here("tiling-scripts", "utils.R"))

download_tiles <- function(aoi_path, aoi_name, city, infra, scenario, from_urban_extent, s3_copy, local_download){
  
  profile  <- "cities-data-dev"
  bucket <- "wri-cities-tcm"
  
  # # 1) Load AOI polygon
  aoi <- st_read(aoi_path, quiet = TRUE)
  out_dir <- file.path("~/CTCM_data_setup", paste(city, scenario, sep = "_"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 2) Load city grid geojson from AWS (public URL)
  if (from_urban_extent){
    grid_url <- glue(
      "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/urban_extent/scenarios/{infra}/{scenario}/metadata/.qgis_data/unbuffered_tile_grid.geojson",
    )
    grid <- st_read(grid_url, quiet = TRUE)
    
    # Intersect and get tile ids
    aoi <- st_transform(aoi, st_crs(grid))              
    grid <- st_filter(grid, aoi)
    tiles <- grid$tile_name
    
    for (t in tiles) {
      s3_src  <- glue("s3://wri-cities-tcm/city_projects/{city}/urban_extent/scenarios/{infra}/{scenario}/{t}/raster_files")
      
      if (isTRUE(local_download)){
        local_d <- file.path(out_dir, "primary_data", "raster_files", t)
        dir.create(local_d, recursive = FALSE, showWarnings = FALSE)
        
        cmd <- sprintf("aws s3 cp %s %s --recursive --no-sign-request",
                       shQuote(s3_src), shQuote(local_d))
        cat("\n", cmd, "\n", sep = "")
        system(cmd)
      }
      
      if (isTRUE(s3_copy)) {
        s3_dst <- glue("s3://wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}/{t}/raster_files")
        
        cmd_up <- sprintf(
          "aws s3 cp %s %s --recursive --profile %s",
          shQuote(local_d), shQuote(s3_dst), shQuote(profile)
        )
        cat("\n", cmd_up, "\n", sep = "")
        system(cmd_up)
      }
    }
    
  } else {
    
    data_path <- glue(
      "s3://wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}/"
    )
    
    tiles <- list_tiles(data_path)
    
    for (t in tiles) {
      s3_src  <- glue("{data_path}/{t}/raster_files")
      
      if (isTRUE(local_download)){
        local_d <- file.path(out_dir, "primary_data", "raster_files", t)
        dir.create(local_d, recursive = FALSE, showWarnings = FALSE)
        
        cmd <- sprintf("aws s3 cp %s %s --recursive --no-sign-request",
                       shQuote(s3_src), shQuote(local_d))
        cat("\n", cmd, "\n", sep = "")
        system(cmd)
      }
    }
  }
  
}

download_tiles(
  aoi_path = aoi_path,
  aoi_name = aoi_name,
  city = city, 
  infra = infra,
  scenario = scenario,
  from_urban_extent = from_urban_extent,
  s3_copy = s3_copy,
  local_download = local_download
)