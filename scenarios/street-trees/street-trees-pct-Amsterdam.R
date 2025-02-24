# Load necessary libraries
library(terra)
library(sf)
library(lidR)
library(rgee)
library(tidyverse)
library(here)

set.seed(5511)

ee_Initialize("elizabeth.wesley@wri.org", drive = TRUE, gcs = TRUE)
# ee_Authenticate("elizabeth.wesley@wri.org") 

# Specify path to save files
city <- "NLD-Amsterdam"
scenario_path <- here("data", city, "scenarios", "street-trees")

# load trees

existing_trees <- rast(here(scenario_path, "AMS_aoi1_cif_tree_canopy.tif")) %>% 
  merge(rast(here(scenario_path, "AMS_aoi2_cif_tree_canopy.tif")))

# AOI ---------------------------------------------------------------------


# Specify AOI
# aoi <- st_read("https://wri-cities-heat.s3.us-east-1.amazonaws.com/BRA-Rio_de_janeiro/raw/boundaries/BRA-Rio_de_janeiro-DBE_low_emission_zone.geojson") %>% 
#   st_transform(4326)
aoi <- existing_trees %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_transform(4326)

city_centroid <- aoi %>% 
  st_centroid() %>% 
  st_coordinates()

utm_epsg <- gfcanalysis::utm_zone(y = city_centroid[2], x = city_centroid[1], proj4string = TRUE) %>% 
  str_sub(12) %>% 
  as.numeric()

utm_ee <- paste0("EPSG:", utm_epsg)

rm(city_centroid)

# Transform aoi to UTM
aoi <- aoi %>% 
  st_transform(utm_epsg) 

bbox <- aoi %>% 
  st_bbox() %>% 
  st_as_sfc()

bb_ee <- sf_as_ee(bbox)


# LULC --------------------------------------------------------------------

lulc <-ee$ImageCollection('projects/wri-datalab/cities/OpenUrban/OpenUrban_LULC')$
  filterBounds(bb_ee)

tiles <- lulc$aggregate_array("grid_cell")$getInfo()

lulc <- lulc$
  max()$int()$
  reproject(crs = utm_ee, scale = 1) %>% 
  ee_as_rast(region = bb_ee, scale = 1)

writeRaster(lulc, here(scenario_path, "lulc.tif"))
# lulc <- rast(here(scenario_path, "lulc.tif"))

# Trees -------------------------------------------------------------------

# load tree functions
source(here("scripts", "scenarios", "street-trees", "tree-generating-functions.R"))

# existing_trees <- ee$ImageCollection("projects/meta-forest-monitoring-okw37/assets/CanopyHeight")$
#   filterBounds(bb_ee)$
#   mosaic() %>% 
#   ee_as_rast(scale = 1, crs = utm_ee, region = bb_ee)
# 
# writeRaster(existing_trees, here("data", city, "scenarios", "street-trees", "existing-tree-canopy.tif"))

# existing_trees <- rast(here("data", city, "scenarios", "street-trees", "existing-tree-canopy.tif"))
existing_trees <- existing_trees %>% 
  resample(lulc)

writeRaster(existing_trees, here("data", city, "scenarios", "street-trees", "existing-tree-resample.tif"),
            overwrite = TRUE)

names(existing_trees) <- "height"
tree_height <- existing_trees %>% 
  subst(0, NA) 
tree_height[tree_height < 3] <- 0 ### Changed to 3

# Get tree height and area for local maxima
# locate trees
ttops <- locate_trees(tree_height, lmf(3)) ### Changed to 3

# segment crowns
crowns <- dalponte2016(tree_height, ttops)()
names(crowns) <- "treeID"

writeRaster(crowns, here("data", city, "scenarios", "street-trees", "existing-tree-crowns.tif"),
            overwrite = TRUE)

# crown vectors
crown_vectors <- crowns %>% 
  as.polygons() %>% 
  st_as_sf() %>% 
  left_join(st_drop_geometry(ttops), by = "treeID") %>% 
  # mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2"))) %>% 
  rename(height = Z) 

ttops <- ttops %>% 
  rename(height = Z) %>% 
  st_zm(drop = TRUE, what = "ZM")

# Probabilities for tree height classes
tree_structure <- tibble(
  tree_classes = c("small", "medium", "large"),
  tree_heights = c(ceiling(quantile(crown_vectors$height, 0.25)),
                   ceiling(quantile(crown_vectors$height, 0.50)),
                   ceiling(quantile(crown_vectors$height, 0.75))),
  weights = c(0.25, 0.50, 0.25)
)

# Save tree data
save(ttops, crown_vectors, tree_structure, 
     file = here("data", city, "scenarios", "street-trees", "tree-vars.RData"))


# Plantable area  -------------------------------------------------------

source(here("scripts", "scenarios", "street-trees", "plantable-street-function.R"))

road_paths <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/", city,
                     "/vector-data/roads/roads_", tiles, ".geojson")

road_vectors <- road_paths %>%
  map_dfr(st_read)

lanes <- read_csv(paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/", city,
                         "/vector-data/roads/average_lanes.csv"))

plantable_street <- generate_plantable_street(aoi = aoi, 
                                              lulc_rast = lulc, 
                                              existing_trees = existing_trees,
                                              road_vectors = road_vectors, 
                                              lanes = lanes,
                                              city = city,
                                              save_files = TRUE)

ped_area <- plantable_street$ped_area
plantable_street <- plantable_street$plantable_street

rm(road_vectors, lanes)



# Prepare the new tree cover raster and tree points

# this will get covered with the updated rasters
updated_tree_cover <- tree_height
# trees will get added here, both existing and new
updated_tree_points <- st_sf(geometry = st_sfc(), crs = utm_epsg)

aoi_grid <- aoi %>% 
  st_make_grid(cellsize = c(1000, 1000), square = TRUE, what = "polygons") %>% 
  st_sf() %>% 
  st_filter(aoi) %>% 
  mutate(ID = row_number())

aoi_grid <- calc_pct_grid_cover(aoi_grid = aoi_grid,
                                existing_tree_cover = tree_height,
                                ped_area = ped_area)


# Achievable potential ----------------------------------------------------

# 90th percentile value
pct_value <- 0.9
achievable_tree_cover <- quantile(aoi_grid$prop_covered, pct_value, names = FALSE, na.rm = TRUE)

# Generate new trees

# Define parameters
target_coverage <- achievable_tree_cover
min_distance <- 5         # Minimum distance between trees in pixels


# Add columns to aoi
aoi_grid <- aoi_grid %>%
  mutate(plantable_area = NA_real_,
         pedestrian_area = NA_real_,
         beginning_tree_cover_area = NA_real_,
         target_tree_cover_area = NA_real_,
         final_tree_cover_area = NA_real_)

# Loop through each grid cell 
for (i in aoi_grid$ID) {
  
  gridcell <- filter(aoi_grid, ID == i)
  
  # Extract the plantable area for the current cell
  plantable_grid <- crop(plantable_street, gridcell)
  
  # Extract the existing tree cover for the current cell
  existing_tree_cover_grid <- crop(tree_height, gridcell) 
  
  # Extract the existing pedestrian area for the current cell
  ped_area_grid <- crop(ped_area, gridcell) 
  
  # Filter ttops to those in current grid
  existing_tree_points_grid <- ttops %>% 
    filter(st_intersects(gridcell, sparse = FALSE))
  
  # # Generate new trees for the current cell
  updated_trees <- generate_trees(plantable_area = plantable_grid,
                                  ped_area = ped_area_grid,
                                  existing_tree_cover = existing_tree_cover_grid,
                                  existing_tree_points = existing_tree_points_grid,
                                  target_coverage = target_coverage,
                                  min_dist = min_distance,
                                  crown_vectors = crown_vectors,
                                  crown_raster = crowns)
  
  # Update the raster with new tree cover
  updated_tree_cover <- merge(updated_trees$updated_tree_cover, updated_tree_cover)
  
  # Update the points
  updated_tree_points <- updated_tree_points %>% 
    bind_rows(updated_trees$tree_points)
  
  # Update grid with info
  aoi_grid <- aoi_grid %>% 
    mutate(plantable_area = case_when(ID == i ~ updated_trees$plantable_area, .default = plantable_area),
           pedestrian_area = case_when(ID == i ~ updated_trees$ped_area, .default = pedestrian_area),
           beginning_tree_cover_area = case_when(ID == i ~ updated_trees$beginning_tree_cover_area, .default = beginning_tree_cover_area),
           target_tree_cover_area = case_when(ID == i ~ updated_trees$target_tree_cover_area, .default = target_tree_cover_area),
           final_tree_cover_area = case_when(ID == i ~ updated_trees$final_tree_cover_area, .default = final_tree_cover_area))
  
}

# Save aoi grid
st_write(aoi_grid, here(scenario_path, "aoi_grid.geojson"))

# Add back in the original vegetation canopy to include areas with height <= 1
updated_tree_cover <- max(updated_tree_cover, existing_trees, na.rm = TRUE)

# Save the new tree cover raster
writeRaster(updated_tree_cover, 
            here(scenario_path, paste0("all-street-trees-" , pct_value * 100, "pctl-achievable", ".tif")),
            overwrite = TRUE)

# Save the new tree points
st_write(updated_tree_points, here(scenario_path, paste0("all-street-trees-" , pct_value * 100, "pctl-achievable", ".geojson")))

# Create a logical raster where TRUE indicates cells that differ between A and B
diff_mask <- existing_trees != updated_tree_cover
# Use the mask to create a raster that keeps values from B where they differ from A
tree_diff_raster <- mask(updated_tree_cover, diff_mask, maskvalue = FALSE)

writeRaster(tree_diff_raster, 
            here(scenario_path, "new-street-trees-achievable.tif"), 
            overwrite = TRUE)


  