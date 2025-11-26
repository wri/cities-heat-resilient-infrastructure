library(sf)
library(terra)
library(tidyverse)
library(here)

parks_with_structures <- st_read("https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_Town/scenarios/aoi/business_district/park-shade-structures/parks_with_new_shade_structures.geojson")

# Scenario
shaded_12_scenario <- rast("https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_Town/scenarios/aoi/business_district/park-shade-structures/shade_1200_park_shade_achievable.tif")
shaded_12_scenario <- shade_12_scenario < 1
unshaded_12_scenario <- isFALSE(shaded_12_scenario) 



# Distance to shade
dist_to_shade_s <- distance(subst(shaded_12_scenario, 0, NA)) %>% 
  mask(vect(parks_with_structures))
dist_to_shade_s_max <- global(dist_to_shade_s, "max", na.rm = TRUE)[,1]
dist_to_shade_s_mean <- global(dist_to_shade_s, "mean", na.rm = TRUE)[,1]


# Baseline
shaded_12_baseline <- rast("https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_Town/scenarios/aoi/business_district/baseline/shade_1200_baseline.tif")
shaded_12_baseline <- shaded_12_baseline < 1
unshaded_12_baseline <- isFALSE(shaded_12_baseline) 

# Distance to shade
dist_to_shade_b <- distance(subst(shaded_12_baseline, 0, NA)) %>% 
  mask(vect(parks_with_structures))
dist_to_shade_b_max <- global(dist_to_shade_b, "max", na.rm = TRUE)[,1]
dist_to_shade_b_mean <- global(dist_to_shade_b, "mean", na.rm = TRUE)[,1]

