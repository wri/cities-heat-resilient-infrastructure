library(sf)
library(terra)
library(tidyverse)
library(here)
library(glue)

grid <- st_read("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/urban_extent/scenarios/baseline/wholecity_start/metadata/.qgis_data/unbuffered_tile_grid.geojson")
aoi <- st_read("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/baseline/baseline/tile_00001/aoi__baseline__baseline.geojson") %>% 
  st_transform(st_crs(grid))

tile_list <- grid %>% 
  mutate(intersects = lengths(st_intersects(., aoi)) > 0) %>%
  filter(intersects) %>%
  pull(tile_name)

base_url <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/urban_extent/scenarios/baseline/wholecity_start"

# 1. build URLs for each tile
tile_urls <- map_chr(tile_list, ~ glue("{base_url}/{.x}/tcm_results/met_era5_hottest_days/utci-1500.tif"))

# 2. read each raster
ras_list <- map(tile_urls, ~ rast(.x))

# 3. mosaic them
# mosaic() will merge, using the first non-NA by default; you can change fun
utci_1500_mosaic <- do.call(mosaic, c(ras_list, list(fun = "mean")))  %>% 
  mask(aoi)
old_utci <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/baseline/baseline/tile_00001/utci-1500__baseline__baseline.tif") %>% 
  resample(utci_1500_mosaic) %>% 
  mask(aoi)

old_tree_utci <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/trees/pedestrian-achievable-90pctl/tile_00001/utci-1500__trees__pedestrian-achievable-90pctl.tif") %>% 
  resample(utci_1500_mosaic) %>% 
  mask(aoi)
new_tree_utci <- old_tree_utci * (utci_1500_mosaic / old_utci)

# 4. (optional) crop/mask to AOI
aoi_vect <- vect(aoi)  # convert sf → SpatVector
# utci_1500_mosaic <- crop(utci_1500_mosaic, aoi_vect) |> mask(aoi_vect)

# Risk categories
rcl <- matrix(c(
  -Inf,  -40, 1,   # extreme cold stress
  -40,   -27, 2,   # very strong cold stress
  -27,   -13, 3,   # strong cold stress
  -13,     0, 4,   # moderate cold stress
  0,     9, 5,   # slight cold stress
  9,    26, 6,   # no thermal stress
  26,    32, 7,   # moderate heat stress
  32,    38, 8,   # strong heat stress
  38,    46, 9,   # very strong heat stress
  46,   Inf,10    # extreme heat stress
), ncol = 3, byrow = TRUE)



utci_levels <- data.frame(
  value = 1:10,
  category = c(
    "extreme cold stress",
    "very strong cold stress",
    "strong cold stress",
    "moderate cold stress",
    "slight cold stress",
    "no thermal stress",
    "moderate heat stress",
    "strong heat stress",
    "very strong heat stress",
    "extreme heat stress"
  )
)

# utci_1500_mosaic <- utci_1500_mosaic %>% 
#   mask(aoi)
# new_tree_utci <- new_tree_utci %>% 
#   mask(aoi)




ped_area <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/baseline/baseline/tile_00001/pedestrian-areas__baseline__baseline.tif") %>% 
  subst(0, NA) %>% 
  resample(utci_1500_mosaic, method = "mode") %>% 
  mask(aoi) 




# Person ####
# tree_cover_baseline <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/baseline/baseline/tile_00001/tree-cover__baseline__baseline.tif") %>% 
#   resample(utci_1500_mosaic, method = "mode") %>% 
#   crop(aoi) 
tree_cover_scenario <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/trees/pedestrian-achievable-90pctl/tile_00001/tree-cover__trees__pedestrian-achievable-90pctl.tif") %>% 
  resample(utci_1500_mosaic, method = "mode") %>% 
  crop(aoi) %>% 
  subst(0, NA)

# UTCI decrease directly under new tree canopy
utci_diff <- new_tree_utci - utci_1500_mosaic 
utci_decrease_trees <- global(mask(utci_diff, tree_cover_scenario), "mean", na.rm = TRUE)[1, 1]

# Area ####
# tree cover difference
tree_cover_diff <- global(mask(tree_cover_scenario, ped_area), "sum", na.rm = TRUE)[1, 1] / global(ped_area, "sum", na.rm = TRUE)[1, 1]

# distance to shade
# baseline
shaded_12_baseline <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/baseline/baseline/tile_00001/shade-1200__baseline__baseline.tif") < 1
shaded_12_baseline <- shaded_12_baseline %>% 
  resample(utci_1500_mosaic, method = "mode") %>% 
  crop(aoi) %>% 
  subst(0, NA)
dist_to_shade_b <- distance(shaded_12_baseline) %>% 
  mask(ped_area)

dist_to_shade_b_max <- global(dist_to_shade_b, "max", na.rm = TRUE)[,1]
dist_to_shade_b_mean <- global(dist_to_shade_b, "mean", na.rm = TRUE)[,1]

# scenario
shaded_12_scenario <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/trees/pedestrian-achievable-90pctl/tile_00001/shade-1200__trees__pedestrian-achievable-90pctl.tif") < 1
shaded_12_scenario <- shaded_12_scenario %>% 
  resample(utci_1500_mosaic, method = "mode") %>% 
  crop(aoi) %>% 
  subst(0, NA)

dist_to_shade_s <- distance(shaded_12_scenario) %>% 
  mask(ped_area)

dist_to_shade_s_max <- global(dist_to_shade_s, "max", na.rm = TRUE)[,1]
dist_to_shade_s_mean <- global(dist_to_shade_s, "mean", na.rm = TRUE)[,1]

dist_to_shade_b_max - dist_to_shade_s_max
dist_to_shade_b_mean - dist_to_shade_s_mean

# Decreased risk category
utci_cat <- classify(utci_1500_mosaic, rcl, right = FALSE)
levels(utci_cat) <- utci_levels

new_tree_utci_cat <- classify(new_tree_utci, rcl, right = FALSE)
levels(new_tree_utci_cat) <- utci_levels

cat_diff <- new_tree_utci_cat - utci_cat
decreased_risk_pct <- global((cat_diff < 0), "sum", na.rm = TRUE)[1, 1] / global(ped_area, "sum", na.rm = TRUE)[1, 1]

# Policy
tree_pts <- st_read("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/trees/pedestrian-achievable-90pctl/tile_00001/new-tree-points__trees__pedestrian-achievable-90pctl.geojson") %>% 
  filter(type == "new")


# Cool roofs --------------------------------------------------------------

utci_cr <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/cool-roofs/large-buildings/tile_00001/utci-1500__cool-roofs__large-buildings.tif") %>% 
  resample(utci_1500_mosaic) %>% 
  mask(aoi)
new_utci_cr <- utci_cr * (utci_1500_mosaic / old_utci)

utci_diff_cr <- new_utci_cr - utci_1500_mosaic

albedo <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/cool-roofs/large-buildings/tile_00001/albedo__cool-roofs__large-buildings__vs-baseline.tif") %>% 
  resample(utci_1500_mosaic) %>% 
  mask(aoi)

# Person
# Mean UTCI reduction on cool roofs
builds <- st_read("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/baseline/baseline/tile_00001/building-polygons__baseline__baseline.geojson") %>% 
  filter(area_sqm >= 2000) %>% 
  rasterize(utci_1500_mosaic) %>% 
  mask(aoi) %>% 
  subst(0, NA)

global(mask(utci_diff_cr, builds), "mean", na.rm = TRUE)[,1]

build_alb_diff <- global(subst(albedo, 0, NA), "mean", na.rm = TRUE)[,1]

# Area
non_build_area <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/baseline/baseline/tile_00001/non-building-areas__baseline__baseline.tif") %>% 
  resample(utci_1500_mosaic, method = "mode") %>% 
  mask(aoi)

global(mask(utci_diff_cr, non_build_area), "mean", na.rm = TRUE)[,1]



# Shade structures --------------------------------------------------------

parks <- st_read("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/shade-structures/small-parks/tile_00001/parks-with-structures__shade-structures__small-parks.geojson")

utci_ss <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/shade-structures/small-parks/tile_00001/utci-1500__shade-structures__small-parks.tif") %>% 
  resample(utci_1500_mosaic, method = "mode") %>% 
  mask(aoi)
new_utci_ss <- utci_ss * (utci_1500_mosaic / old_utci)
utci_diff_ss <- new_utci_ss - utci_1500_mosaic

shade_structures <- st_read("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/shade-structures/small-parks/tile_00001/structures__shade-structures__small-parks.geojson") %>% 
  rasterize(utci_1500_mosaic) %>% 
  mask(aoi) %>% 
  subst(0, NA)

# UTCI diff under shade structures
global(mask(utci_diff_ss, shade_structures), "mean", na.rm = TRUE)[,1]

# Distance to shaade
shaded_12_scenario_ss <- rast("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ZAF-Cape_Town/business_district/shade-structures/small-parks/tile_00001/shade-1200__shade-structures__small-parks.tif") < 1
shaded_12_scenario_ss <- shaded_12_scenario_ss %>% 
  resample(utci_1500_mosaic, method = "mode") %>% 
  crop(aoi) %>% 
  subst(0, NA)

dist_to_shade_ss <- distance(shaded_12_scenario_ss) %>% 
  mask(vect(parks))

dist_to_shade_ss_mean <- global(dist_to_shade_ss, "mean", na.rm = TRUE)[,1]
dist_to_shade_b_mean - dist_to_shade_ss_mean

# Decreased risk in parks
new_ss_utci_cat <- classify(new_utci_ss, rcl, right = FALSE)
levels(new_ss_utci_cat) <- utci_levels

cat_diff_ss <- mask((new_ss_utci_cat - utci_cat), vect(parks))
decreased_risk_pct_ss <- global((cat_diff_ss < 0), "sum", na.rm = TRUE)[1, 1] / global(parks %>% rasterize(utci_1500_mosaic), "sum", na.rm = TRUE)[1, 1]
