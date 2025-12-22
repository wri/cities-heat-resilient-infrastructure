#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
aoi_path <- args[1]
city     <- args[2]
infra    <- args[3]
scenario <- args[4]

if (any(is.na(c(aoi_path, city, infra, scenario)))) {
  stop("Usage: get_tiles_and_copy.R <aoi_path> <city_name> <infra> <scenario>")
}

library(sf)

# 1) Load AOI polygon
aoi <- st_read(aoi_path, quiet = TRUE)

# 2) Load city grid geojson from AWS (public URL)
grid_url <- sprintf(
  "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/%s/urban_extent/scenarios/%s/%s/metadata/.qgis_data/unbuffered_tile_grid.geojson",
  city, infra, scenario
)
grid <- st_read(grid_url, quiet = TRUE)

# 3) Intersect and get tile ids
aoi <- st_transform(aoi, st_crs(grid))              
grid <- st_filter(grid, aoi)
tiles <- grid$tile_name

paste0(length(tiles), " tiles ")

# 4) Copy each tile folder from S3 to local
out_dir <- file.path("~/CTCM_data_setup", paste(city, scenario, sep = "-"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (t in tiles) {
  s3_src  <- sprintf("s3://wri-cities-tcm/city_projects/%s/urban_extent/scenarios/%s/%s/", 
                     city, infra, scenario)
  local_d <- file.path(out_dir, city, t)
  dir.create(local_d, recursive = TRUE, showWarnings = FALSE)
  
  cmd <- sprintf("aws s3 cp %s %s --recursive --no-sign-request",
                 shQuote(s3_src), shQuote(local_d))
  cat("\n", cmd, "\n", sep = "")
  system(cmd)
}
