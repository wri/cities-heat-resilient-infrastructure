# Setup ---------------------------------------------------------------------

# Load necessary libraries
library(terra)
library(sf)
library(lidR)
library(tidyverse)
library(here)

city = "BRA-Rio_de_janeiro"

inputs_path <- here("data", city)
scenario_path <- here("data", city, "scenarios", "street-trees")

# Create scenario_path
if (!dir.exists(scenario_path)) {
  dir.create(scenario_path, recursive = TRUE)
}

# Inputs
aoi <- st_read(here(inputs_path, "aoi.geojson"))
lulc <- rast(here(inputs_path, "open-urban.tif"))
road_vectors <- st_read(here(inputs_path, "roads.geojson"))
lanes <- read_csv(here(inputs_path, "lanes.csv"))
tree_height <- rast(here(inputs_path, "existing-tree-canopy.tif"))

#TODO: Get utm_epsg from OpenUrban
# UTM
source(here("utils", "utm.R"))
utm <- get_aoi_utm(aoi)

# Plantable area  -------------------------------------------------------

source(here("scenarios", "street-trees", "plantable-street-function.R"))

plantable_street <- generate_plantable_street(aoi = aoi, 
                                              lulc_rast = lulc, 
                                              existing_trees = tree_height,
                                              road_vectors = road_vectors, 
                                              lanes = lanes,
                                              city = city,
                                              utm = utm,
                                              save_files = TRUE)

rm(road_vectors, lanes)


names(tree_height) <- "height"
tree_height[tree_height < 1] <- 0

# Get tree height and area for local maxima
# locate trees
ttops <- locate_trees(tree_height, lmf(9)) 

# segment crowns
crowns <- dalponte2016(tree_height, ttops)()
names(crowns) <- "treeID"

writeRaster(crowns, here("data", city, "scenarios", "street-trees", "existing-tree-crowns.tif"),
            filetype = "COG",
            gdal = c("TILED=YES", "COMPRESS=LZW", "BIGTIFF=IF_SAFER", "COPY_SRC_OVERVIEWS=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512"))

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

# Prepare the new tree cover raster and tree points

# this will get covered with the updated rasters
updated_tree_cover <- tree_height
# trees will get added here, both existing and new
updated_tree_points <- st_sf(geometry = st_sfc(), crs = utm$epsg)

aoi_grid <- aoi %>% 
  st_make_grid(cellsize = c(1000, 1000), square = TRUE, what = "polygons") %>% 
  st_sf() %>% 
  st_filter(aoi) %>% 
  mutate(ID = row_number())


source(here("scenarios", "street-trees", "tree-generating-functions.R"))

aoi_grid <- calc_pct_grid_cover(aoi_grid = aoi_grid,
                                existing_tree_cover = tree_height,
                                plantable_area = plantable_street)


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
  
  # Filter ttops to those in current grid
  existing_tree_points_grid <- ttops %>% 
    filter(st_intersects(gridcell, sparse = FALSE))
  
  # # Generate new trees for the current cell
  updated_trees <- generate_trees(plantable_area = plantable_grid,
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
           beginning_tree_cover_area = case_when(ID == i ~ updated_trees$beginning_tree_cover_area, .default = beginning_tree_cover_area),
           target_tree_cover_area = case_when(ID == i ~ updated_trees$target_tree_cover_area, .default = target_tree_cover_area),
           final_tree_cover_area = case_when(ID == i ~ updated_trees$final_tree_cover_area, .default = final_tree_cover_area))
  
}


# Save the new tree cover raster
gdal_opts <- c("TILED=YES", "COMPRESS=LZW", "BIGTIFF=IF_SAFER", "COPY_SRC_OVERVIEWS=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512")
writeRaster(updated_tree_cover, 
            here(scenario_path, "rasters", paste0("all-street-trees-" , pct_value * 100, "pctl-achievable", ".tif")),
            overwrite = TRUE,
            filetype = "COG", 
            gdal = gdal_opts)

# Save the new tree points
st_write(updated_tree_points, here(scenario_path, "vectors", paste0("all-street-trees-" , pct_value * 100, "pctl-achievable", ".geojson")))

# Create a logical raster where TRUE indicates cells that differ between A and B
diff_mask <- tree_height != updated_tree_cover
# Use the mask to create a raster that keeps values from B where they differ from A
tree_diff_raster <- mask(updated_tree_cover, diff_mask, maskvalue = FALSE)

writeRaster(tree_diff_raster, 
            here(scenario_path, "rasters", "new-street-trees-achievable.tif"), 
            overwrite = TRUE,
            filetype = "COG", 
            gdal = gdal_opts)


  
