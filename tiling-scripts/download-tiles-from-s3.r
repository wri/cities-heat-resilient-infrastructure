#!/usr/bin/env Rscript

# args <- commandArgs(trailingOnly = TRUE)
# aoi_path <- args[1]
# city     <- args[2]
# infra    <- args[3]
# scenario <- args[4]
# 
# if (any(is.na(c(aoi_path, city, infra, scenario)))) {
#   stop("Usage: get_tiles_and_copy.R <aoi_path> <city_name> <infra> <scenario>")
# }

list_tiles <- function(baseline_path, profile = "cities-data-dev") {
  lines <- system2(
    "aws",
    c("s3", "ls", baseline_path, "--profile", profile),
    stdout = TRUE,
    stderr = TRUE
  )
  
  tile_lines <- grep("^[[:space:]]*PRE[[:space:]]+tile_[0-9]+/", lines, value = TRUE)
  sub("^[[:space:]]*PRE[[:space:]]+(tile_[0-9]+)/$", "\\1", tile_lines)
}

download_tiles <- function(aoi, aoi_name, city, infra, scenario, from_urban_extent = TRUE){
  
  library(sf)
  library(glue)
  
  profile  <- "cities-data-dev"
  bucket <- "wri-cities-tcm"
  # # 1) Load AOI polygon
  # aoi <- st_read(aoi_path, quiet = TRUE)
  out_dir <- file.path("~/CTCM_data_setup", paste(city, scenario, sep = "_"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 2) Load city grid geojson from AWS (public URL)
  if (from_urban_extent){
    grid_url <- glue(
      "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/urban_extent/scenarios/baseline/baseline/metadata/.qgis_data/unbuffered_tile_grid.geojson",
    )
    grid <- st_read(grid_url, quiet = TRUE)
    
    # Intersect and get tile ids
    aoi <- st_transform(aoi, st_crs(grid))              
    grid <- st_filter(grid, aoi)
    tiles <- grid$tile_name
    
    for (t in tiles) {
      s3_src  <- glue("s3://wri-cities-tcm/city_projects/{city}/urban_extent/scenarios/baseline/baseline/{t}/raster_files")
      local_d <- file.path(out_dir, "primary_data", "raster_files", t)
      dir.create(local_d, recursive = FALSE, showWarnings = FALSE)
      
      cmd <- sprintf("aws s3 cp %s %s --recursive --no-sign-request",
                     shQuote(s3_src), shQuote(local_d))
      cat("\n", cmd, "\n", sep = "")
      system(cmd)
    }
    
  } else {
    
    baseline_path <- glue(
      "s3://wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/"
    )
    
    tiles <- list_tiles(baseline_path)
    
    for (t in tiles) {
      s3_src  <- glue("{baseline_path}/{t}/raster_files")
      local_d <- file.path(out_dir, "primary_data", "raster_files", t)
      dir.create(local_d, recursive = FALSE, showWarnings = FALSE)
      
      cmd <- sprintf("aws s3 cp %s %s --recursive --no-sign-request",
                     shQuote(s3_src), shQuote(local_d))
      cat("\n", cmd, "\n", sep = "")
      system(cmd)
    }
  }
  
}
