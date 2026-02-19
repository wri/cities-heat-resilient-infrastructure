library(terra)
library(sf)
library(glue)
library(dplyr)

# S3 prefix where all tile folders live (note trailing /)
prefix <- "s3://wri-cities-tcm/city_projects/ZAF-Durban/inner_city_lap/scenarios/baseline/baseline/"

# Use AWS CLI to list "directories" (tile_00001/, tile_00002/, ...)
cmd <- glue("aws s3 ls {shQuote(prefix)}")
lines <- system(cmd, intern = TRUE)

# Lines that represent "folders" look like: "                           PRE tile_00001/"
tile_ids <- grep("PRE", x = lines, value = TRUE) |>
  sub(".*PRE +", "", x = _) |>
  sub("/$", "", x = _)

bucket_prefix <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Durban/inner_city_lap/scenarios/baseline/baseline"

tile_polys <- lapply(tile_ids, function(tile) {
  tif_path <- glue("{bucket_prefix}/{tile}/raster_files/cif_open_urban.tif")
  r <- rast(tif_path)
  
  # make polygon from extent
  e  <- ext(r)
  cr <- crs(r)
  
  p <- as.polygons(rast(e), extent = TRUE)  # polygon from extent
  crs(p) <- cr
  p$tile <- tile
  p
})

grid <- do.call(rbind, tile_polys) |> st_as_sf()

# e.g. write to file
# st_write(grid, "ZAF_Durban_tile_grid.gpkg")

