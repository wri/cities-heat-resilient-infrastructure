library(tidyverse)
library(sf)
library(here)
library(terra)

scenario_path <- here("data", "ZAF-Cape_Town", "scenarios", "park-shade")
data_path <- here("data", "ZAF-Cape_Town", "scenarios", "data")

shade_structures <- st_read(here(scenario_path, "shade-structures.geojson"))
dsm <- rast(here(data_path, "dsm_ground_build.tif"))

# https://srpshade.caddetails.com/products/square-hip-shades-4430/80366
# 8-ft height for shade structures
structure_height <- 8 / 3.281 # convert to meters

shade_structures <- shade_structures %>% 
  mutate(height = structure_height)

shade_structures_rast <- shade_structures %>% 
  rasterize(dsm, field = "height", background = 0) 
  
new_dsm <- dsm + shade_structures_rast
writeRaster(new_dsm, here(scenario_path, "shade-structure-dsm.tif"))
