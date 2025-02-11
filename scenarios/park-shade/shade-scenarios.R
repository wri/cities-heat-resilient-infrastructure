# Park shade scenario
# Parks 1 acre or smaller, pocket parks, get at least 25% shade
# Large parks have shade within a 100-m walk
# Pitches are excluded from available area for shade structures

library(terra)
library(tidyterra)
library(sf)
library(rgee)
library(tidyverse)
library(ggnewscale)
library(leaflet)
library(exactextractr)
library(here)

scenario_path <- here("data", "ZAF-Cape_Town", "scenarios", "park-shade")
data_path <- here("data", "ZAF-Cape_Town", "scenarios", "data")

# AOI ---------------------------------------------------------------------


# Cape town AOI
aoi <- st_read("https://wri-cities-heat.s3.amazonaws.com/ZAF-Cape_town/processed/citycentre_roi.geojson") %>% 
  st_transform(4326)

city_centroid <- aoi %>% 
  st_centroid() %>% 
  st_coordinates()

utm_epsg <- gfcanalysis::utm_zone(y = city_centroid[2], x = city_centroid[1], proj4string = TRUE) %>% 
  str_sub(12) %>% 
  as.numeric()

rm(city_centroid)

# Transform aoi to UTM
aoi <- aoi %>% 
  st_transform(utm_epsg) 

bbox <- aoi %>% 
  st_bbox() %>% 
  st_as_sfc()

# LULC --------------------------------------------------------------------

lulc <- rast(here(data_path, "lulc.tif"))


# Existing shade ----------------------------------------------------------

# Use 1200 shade map
shadow_12pm <- rast("https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_town/scenarios/street-trees/thermal-comfort/baseline_street_tree_Shadow_2022_20_1200D.tif") 

# Combine tree shade and building shade
shaded <- shadow_12pm <= 0.5

writeRaster(shaded, here(scenario_path, "shade.tif"),
         filetype = "COG",
         gdal = c("TILED=YES", "COMPRESS=LZW", "BIGTIFF=IF_SAFER", "COPY_SRC_OVERVIEWS=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512"))

# Created unshaded raster
unshaded <- isFALSE(shaded) 


# Parks -------------------------------------------------------------------


# Parks vectors, continguous areas dissolved
park_vectors <- st_read(here(data_path, "openspace_10.geojson")) %>% 
  filter(leisure != "pitch") %>% 
  # dissolve
  st_union() %>% 
  st_sf() %>% 
  # break apart
  st_cast("POLYGON") %>% 
  # cropping to AOI before calculating area
  st_intersection(bbox) %>% 
  mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2")),
         park_id = row_number())

st_write(park_vectors, here(scenario_path, "parks.geojson"))

## zonal statistics per park shaded/unshaded ####
park_vectors <- park_vectors %>% 
  mutate(shaded_pct = exact_extract(shaded, geometry, "mean"),
         shaded_area = shaded_pct * area_sqm,
         unshaded_pct = 1 - shaded_pct,
         unshaded_area = unshaded_pct * area_sqm)

ggplot() +
  geom_spatraster(data = shaded) +
  geom_sf(data = park_vectors, fill = NA)

# Get sports fields
pitch_vectors <- st_read(here(data_path, "openspace_10.geojson")) %>% 
  filter(leisure == "pitch") %>%  
  st_union() %>% 
  st_sf()

# Parks with sport fields erased
park_suitable_area_vector <- park_vectors %>% 
  # Erase sports fields
  st_difference(pitch_vectors) %>% 
  mutate(area_sqm_suitable = as.numeric(units::set_units(st_area(geometry), "m^2")))

# NEED TO CALCULATE THE AREA SUITABLE FOR SHADE STRUCTURES
# RECALCULATE THE MEAN OF SHADE PER UPDATED GEOMETRY

# Small parks -------------------------------------------------------------

source(here("scripts", "shade-generating-functions.R"))

# Pocket parks—1 acre (~ 0.4 hectares, 4046.86 m^2) or less
# 25% shade
small_parks <- park_suitable_area_vector %>% 
  filter(area_sqm <= 4046.86, 
         shaded_pct < 0.25) 

# Create empty shade structure geometry
shade_structures_small_parks <- st_sf(geometry = st_sfc(), crs = utm_epsg)

# Generate shade in small parks
for (i in 1:nrow(small_parks)) {
  print(i)
  park <- slice(small_parks, i)
  shade_structures <- generate_squares_in_valid_area(
    park = park, 
    unshaded_raster = unshaded, 
    structure_size = 5, 
    shade_pct = 0.25, 
    spacing = 5)
  
  if (!is.null(squares)) {
    shade_structures_small_parks <- bind_rows(shade_structures_small_parks, shade_structures)
  }
}


# Large parks -------------------------------------------------------------

large_parks <- park_suitable_area_vector %>% 
  filter(area_sqm > 4046.86) 

shade_structures_large_parks <- st_sf(geometry = st_sfc(), crs = utm_epsg)

for (i in 1:nrow(large_parks)) {
  print(i)
  park <- slice(large_parks, i)
  shade_structures <- shade_dist_area(
    park = park, 
    unshaded_raster = unshaded, 
    min_shade_area = 25,
    max_dist_to_shade = 50,
    structure_size = 5,
    spacing = 5)
  
  if (!is.null(shade_structures)) {
    shade_structures_large_parks <- bind_rows(shade_structures_large_parks, shade_structures)
  }
}


# Combine small and large parks -------------------------------------------

shade_structures <- shade_structures_small_parks %>% 
  bind_rows(shade_structures_large_parks)

st_write(shade_structures, here(scenario_path, "shade-structures.geojson"))
