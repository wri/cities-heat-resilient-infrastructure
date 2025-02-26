# Specify city and scenario  
city = "BRA-Rio_de_janeiro"
scenario <- "street-trees"

# Paths
inputs_path <- here("data", city)
scenario_path <- here("data", city, "scenarios", scenario)

# Inputs
lulc <- rast(here(inputs_path, "open-urban.tif"))
plantable_area <- rast(here(scenario_path, "plantable-street.tif"))
tree_height <- rast(here(inputs_path, "existing-tree-canopy.tif"))
crowns <- rast(here(scenario_path, "existing-tree-crowns.tif"))
load(here(scenario_path, "tree-vars.RData"))


# load tree functions
source(here("scripts", "scenarios", "street-trees", "tree-generating-functions.R"))

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
                                plantable_area = plantable_area)

# Generate new trees

# Define parameters
target_coverage <- 1
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
  plantable_grid <- crop(plantable_area, gridcell)
  
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
            here(scenario_path, paste0("all-street-trees-max-cover", ".tif")),
            overwrite = TRUE,
            filetype = "COG", 
            gdal = gdal_opts)

# Save the new tree points
st_write(updated_tree_points, here(scenario_path, paste0("all-street-trees-max-cover", ".geojson")))

# Create a logical raster where TRUE indicates cells that differ between A and B
diff_mask <- tree_height != updated_tree_cover
# Use the mask to create a raster that keeps values from B where they differ from A
tree_diff_raster <- mask(updated_tree_cover, diff_mask, maskvalue = FALSE)

writeRaster(tree_diff_raster, 
            here(scenario_path, "new-street-trees-max-cover.tif"), 
            overwrite = TRUE,
            filetype = "COG", 
            gdal = gdal_opts)

aoi_grid <- aoi_grid %>% 
  mutate(final_prop_cover = final_tree_cover_area / plantable_area,
         final_increase = final_prop_cover - prop_covered)

st_write(aoi_grid, here(scenario_path, "max-cover-aoi-grid.geojson"))
