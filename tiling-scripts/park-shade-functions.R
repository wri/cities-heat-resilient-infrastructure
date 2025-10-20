library(here)
library(terra)
library(sf)
library(dplyr)
library(purrr)
library(readr)
library(glue)
library(geoarrow)
library(sfarrow)
library(tidyverse)
library(exactextractr)

# Sign in to AWS ----------------------------------------------------------

system("aws sso login --profile cities-data-dev")
Sys.setenv(AWS_PROFILE = "cities-data-dev",
           AWS_DEFAULT_REGION = "us-east-1",
           AWS_SDK_LOAD_CONFIG = "1")

s3 <- paws::s3()
bucket <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"

# Process baseline tile data ----------------------------------------------
city <- "ZAF-Cape_Town"
aoi_name <- "urban_extent"
city_folder <- glue("city_projects/{city}/{aoi_name}")

open_urban_aws_http <- glue("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}")

baseline_name <- "wholecity_start"
infra_name <- "shade-structures"
scenario_name <- "all-parks"

baseline_folder <- glue("{city_folder}/scenarios/baseline/{baseline_name}")
scenario_folder <- glue("{city_folder}/scenarios/{infra_name}/{scenario_name}")

# Make scenario folder
ensure_s3_prefix(bucket, scenario_folder)

# Get tile ids
t <- s3$list_objects_v2(
  Bucket = bucket,
  Prefix = baseline_folder
)

# if (is.null(t$Contents)) {
#   tiles <- character(0)
# } else {
#   tiles <- t$Contents %>%
#     map_chr("Key") %>%
#     str_extract("tile_\\d{5}") %>%  
#     { .[!is.na(.)] } %>%
#     unique() %>%
#     sort()
# }
# TODO: remove when all tiles are ready
tiles <- c(
  "tile_00015","tile_00016","tile_00017",
  "tile_00077","tile_00078","tile_00079","tile_00080","tile_00081",
  "tile_00139","tile_00140","tile_00141","tile_00142","tile_00143","tile_00144",
  "tile_00201","tile_00202","tile_00203","tile_00204","tile_00205","tile_00206"
)

aoi <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/urban_extent_boundary.geojson"))
buffered_tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/tile_grid.geojson"))
tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/unbuffered_tile_grid.geojson"))

# TODO: Remove after grid is complete ####
buffered_tile_grid <- buffered_tile_grid %>% 
  filter(tile_name %in% tiles)
tile_grid <- tile_grid %>% 
  filter(tile_name %in% tiles)

parks <- st_read_parquet(
  glue("{open_urban_aws_http}/openspace/openspace_all.parquet")) %>% 
  # TODO: Do we want to clip to the AOI?
  # restricting to parks entirely within the AOI
  st_filter(aoi, .predicate = st_within)  

# Parks vectors, contiguous areas dissolved
park_vectors <- parks %>% 
  filter(leisure != "pitch") %>% 
  # dissolve
  st_union() %>% 
  st_sf() %>% 
  # break apart
  st_cast("POLYGON") %>% 
  mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2")),
         park_id = row_number())

# Get sports fields
pitch_vectors <- parks %>% 
  filter(leisure == "pitch") %>%  
  st_union() %>% 
  st_sf()

# Parks with sport fields erased
if (nrow(pitch_vectors) > 0){
  park_suitable_area_vectors <- park_vectors %>% 
    # Erase sports fields
    st_difference(pitch_vectors) %>% 
    mutate(area_sqm_suitable = as.numeric(units::set_units(st_area(geometry), "m^2")))
} else {
  park_suitable_area_vectors <- park_vectors
}

# Add tile_names for each intersecting tile grid
park_suitable_area_vectors <- park_suitable_area_vectors %>%
  {
    bt  <- st_transform(buffered_tile_grid, st_crs(.))
    ubt <- st_transform(tile_grid,        st_crs(.))
    
    mutate(.,
           buffered_tile_names = st_intersects(., bt)  %>% map(\(idx) unique(bt$tile_name[idx])),
           unbuffered_tile_names = st_intersects(., ubt) %>% map(\(idx) unique(ubt$tile_name[idx]))
    )
  } %>%
  filter(lengths(unbuffered_tile_names) > 0) 
# %>%   # simple length test
#   split(seq_len(nrow(.)))



# Functions ---------------------------------------------------------------

source(here("tiling-scripts", "utils.R"))
source(here("scenario-generation", "park-shade-structures", "shade-generating-functions.R"))

shade_structures_all_parks <- st_sf(geometry = st_sfc(), crs = st_crs(park_vectors))

# Calculate average distance to shade
calc_avg_shade_dist <- function(park_idx, shade_rast, structure_size = 5, shade_pct = 0.25, 
                                spacing = 5, min_shade_area = 25, max_dist_to_shade = 50){
  
  park <- park_suitable_area_vectors %>% 
    filter(park_id == park_idx)
  
  print(glue("Park {park$park_id}"))
  
  tile_ids <- park %>% 
    pull(buffered_tile_names) %>% 
    unlist()
  
  shade_paths <- glue("{aws_http}/{baseline_folder}/{tile_ids}/tcm_results/met_era5_hottest_days/Shadow_2022_22_1200D.tif")
  shade_rast <- load_and_merge(shade_paths)
  
  # Combine tree shade and building shade
  shaded <- shade_rast < 1
  unshaded <- isFALSE(shaded) 
  
  park <- park %>% 
    mutate(shaded_pct = exact_extract(shaded, geometry, "mean"),
           shaded_area = shaded_pct * area_sqm,
           unshaded_pct = 1 - shaded_pct,
           unshaded_area = unshaded_pct * area_sqm)
  
  if (park$area_sqm > 4046.86) {
    shade_structures <- shade_dist_area(
      park = park, 
      unshaded_raster = unshaded, 
      min_shade_area = min_shade_area,
      max_dist_to_shade = max_dist_to_shade,
      structure_size = structure_size,
      spacing = spacing
    )
  } else {
    shade_structures <- generate_squares_in_valid_area(
      park = park, 
      unshaded_raster = unshaded, 
      structure_size = structure_size, 
      shade_pct = shade_pct, 
      spacing = spacing
    )
  }
  
  # https://srpshade.caddetails.com/products/square-hip-shades-4430/80366
  # 8-ft height for shade structures
  structure_height <- 8 / 3.281 # convert to meters
  
  shade_structures <- shade_structures %>% 
    mutate(height = structure_height)
  
  shade_structures_rast <- shade_structures %>% 
    rasterize(shade_rast, field = "height", background = 0) 
  
  # If shade structures are added, save the raster and add them to feature
  # collection
  if (!is.null(shade_structures)) {
    
    for (t in tile_ids){
      
      tile <- buffered_tile_grid %>% 
        filter(tile_name == t)
      x <- shade_structures_rast %>% 
        crop(tile)
      
      ensure_s3_prefix(bucket, glue("{scenario_folder}/{t}/layers"))
      
      # IF there is already a structures-as-trees raster, load it and merge it
      existing <- tryCatch(rast(url_existing), error = function(e) NULL)
      
      if (!is.null(existing)) {
        x <- mosaic(x, existing, fun = "max", na.rm = TRUE)
      }
        
      write_s3(x, glue("{bucket}/{scenario_folder}/{t}/layers/structures-as-trees.tif"))
      print(glue("{t} shade raster saved"))
    }
    
    shade_structures_all_parks <- bind_rows(shade_structures_all_parks, shade_structures)
  }
 
  return(shade_structures_all_parks)
}

shade_structures_all_parks <- map(park_suitable_area_vectors$park_id, 
                                  ~ calc_avg_shade_dist(.x, shade_rast)) %>% 
  compact() %>% 
  bind_rows()

write_s3(shade_structures_all_parks, glue("{bucket}/{scenario_folder}/shade_structures_all_parks.geojson"))



