# Setup ---------------------------------------------------------------------

# Load necessary libraries
library(terra)
library(sf)
library(lidR)
library(tidyverse)
library(here)

city = "BRA-Rio_de_janeiro"

inputs_path <- here("data", city)
scenario_path <- here(inputs_path, "scenarios", "street-trees")

# Create scenario_path
if (!dir.exists(scenario_path)) {
  dir.create(scenario_path, recursive = TRUE)
}

# Inputs
aoi <- st_read(here(inputs_path, "aoi.geojson"))
lulc <- rast(here(inputs_path, "open-urban.tif"))
canopy_height_existing <- rast(here(inputs_path, "tree-canopy-height.tif"))

plantable_area <- rast(here(scenario_path, "plantable-street.tif"))
crowns <- rast(here(scenario_path, "existing-tree-crowns.tif"))
load(here(scenario_path, "tree-vars.RData"))

# OpenUrban should be in the right projection, get UTM from lulc
# UTM
utm <- st_crs(lulc)


# load tree functions
source(here("scenario-generation", "street-trees", "tree-generating-functions.R"))

names(canopy_height_existing) <- "height"
tree_height <- canopy_height_existing
tree_height[tree_height < 3] <- 0

# this will get covered with the updated rasters
updated_tree_cover <- tree_height
# trees will get added here, both existing and new
updated_tree_points <- st_sf(geometry = st_sfc(), crs = utm$epsg)

aoi_grid <- aoi %>% 
  st_make_grid(cellsize = c(1000, 1000), square = TRUE, what = "polygons") %>% 
  st_sf() %>% 
  st_filter(aoi) %>% 
  mutate(ID = row_number())

aoi_grid <- calc_pct_grid_cover(aoi_grid = aoi_grid,
                                existing_tree_cover = tree_height,
                                ped_area = ped_area)

# Generate new trees

# Define parameters
target_coverage <- 1
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
  plantable_grid <- crop(plantable_area, gridcell)
  
  # Extract the existing tree cover for the current cell
  existing_tree_cover_grid <- crop(tree_height, gridcell) 
  
  # Extract the existing pedestrian area for the current cell
  ped_area_grid <- crop(ped_area, gridcell) 
  
  # Filter ttops to those in current grid
  existing_tree_points_grid <- ttops %>% 
    filter(rowSums(st_intersects(gridcell, sparse = FALSE)) > 0)
  
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

# Save the AOI grid
aoi_grid <- aoi_grid %>% 
  mutate(final_prop_cover = final_tree_cover_area / plantable_area,
         final_increase = final_prop_cover - prop_covered)

st_write(aoi_grid, here(scenario_path, "technical-potential-aoi-grid.geojson"))

# Add back in the original vegetation canopy to include areas with height <= 1
updated_tree_cover <- max(updated_tree_cover, canopy_height_existing, na.rm = TRUE)

# Save the new tree cover raster
writeRaster(updated_tree_cover, 
            here(scenario_path, paste0("all-street-trees-technical-potential", ".tif")),
            overwrite = TRUE)

# Save the new tree points
st_write(updated_tree_points, here(scenario_path, paste0("all-street-trees-technical-potential", ".geojson")))

# Create a logical raster where TRUE indicates cells that differ between A and B
diff_mask <- tree_height != updated_tree_cover
# Use the mask to create a raster that keeps values from B where they differ from A
tree_diff_raster <- mask(updated_tree_cover, diff_mask, maskvalue = FALSE)

writeRaster(tree_diff_raster, 
            here(scenario_path, "new-street-trees-technical-potential.tif"), 
            overwrite = TRUE)


