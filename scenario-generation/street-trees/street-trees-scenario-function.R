street_trees_scenario_function <- function(scenario, percentile = NULL, target_coverage = NULL, min_tree_dist, aoi, lulc, 
                                           road_vectors, lanes, canopy_height_existing, scenario_name) {
  
  scenario_path <- here(infrastructure_path, scenario_name)
  
  if (!dir.exists(scenario_path)) {
    
    dir.create(scenario_path)
    
  }
  
  # Get UTM from OpenUrban
  
  utm <- st_crs(lulc)
  
  # Create a tree height raster of vegetation canopy over 3 meters
  tree_height <- canopy_height_existing
  tree_height[tree_height < 3] <- 0
  
  # Process tree canopy to individual trees ---------------------------------
  
  # If the tree data already exists, use it, otherwise create it
  if (file.exists(here(infrastructure_path, "tree-vars.RData"))) {
    
    load(here(infrastructure_path, "tree-vars.RData"))
    crowns <- rast(here(infrastructure_path, "existing-tree-crowns.tif"))
    
  } else {
    
    source(here("scenario-generation", "street-trees", "canopy-to-trees-function.R"))
    
    process_trees(tree_raster = tree_height, 
                  save_files = TRUE)
    
    load(here(infrastructure_path, "tree-vars.RData"))
    crowns <- rast(here(infrastructure_path, "existing-tree-crowns.tif"))
    
  }
  
  
  # Plantable area  -------------------------------------------------------
  
  if (file.exists(here(infrastructure_path, "plantable-street.tif"))) {
    
    ped_area <- rast(here(infrastructure_path, "pedestrian-area.tif"))
    plantable_street <- rast(here(infrastructure_path, "plantable-street.tif"))
  } else {
    
    source(here("scenario-generation", "street-trees", "plantable-street-function.R"))
    
    plantable_street <- generate_plantable_street(aoi = aoi, 
                                                  lulc_rast = lulc, 
                                                  existing_trees = tree_height,
                                                  road_vectors = road_vectors, 
                                                  lanes = lanes,
                                                  city = city,
                                                  utm = utm,
                                                  save_files = TRUE)
    
    ped_area <- plantable_street$ped_area
    plantable_street <- plantable_street$plantable_street
    
    rm(road_vectors, lanes)
    
  }
  
  
  # Evaluate baseline conditions --------------------------------------------
  
  
  
  # Copy the tree height raster that the new trees will get added to
  updated_tree_cover <- tree_height
  
  # Create an empty sf that trees will get added to
  updated_tree_points <- st_sf(geometry = st_sfc(), crs = utm$epsg)
  
  # Create a grid over the aoi to iterate over for creating trees
  aoi_grid <- aoi %>% 
    st_make_grid(cellsize = c(1000, 1000), square = TRUE, what = "polygons") %>% 
    st_sf() %>% 
    st_filter(aoi) %>% 
    mutate(ID = row_number())
  
  # Calculate the existing percent cover of trees in pedestrian areas for each gridcell
  source(here("scenario-generation", "street-trees", "tree-generating-functions.R"))
  
  aoi_grid <- calc_pct_grid_cover(aoi_grid = aoi_grid,
                                  existing_tree_cover = tree_height,
                                  ped_area = ped_area)
  
  
  # Achievable potential ----------------------------------------------------
  
  aws_path <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/", city, "/scenarios/street-trees/street-tree-pct-1km-grid.csv")
    
  # Get percentile value
  ped_area_tree_dist <- read_csv(aws_path)
  
  if (scenario == "achievable"){
    
    target_coverage <- quantile(ped_area_tree_dist$`pct-tree`, percentile, names = FALSE, na.rm = TRUE)
    
  } else if (scenario == "technical") {
    
    target_coverage <- 1
    
  } else if (scenario == "program") {
    
    target_coverage <- target_coverage
    
  }
  
  # Generate new trees
  
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
      filter(rowSums(st_intersects(gridcell, sparse = FALSE)) > 0)
    
    
    # # Generate new trees for the current cell
    updated_trees <- generate_trees(plantable_area = plantable_grid,
                                    ped_area = ped_area_grid,
                                    existing_tree_cover = existing_tree_cover_grid,
                                    existing_tree_points = existing_tree_points_grid,
                                    tree_structure = tree_structure,
                                    tree_height = tree_height,
                                    target_coverage = target_coverage,
                                    min_dist = min_tree_dist,
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
  aoi_grid <- aoi_grid %>% 
    mutate(final_prop_cover = final_tree_cover_area / plantable_area,
           final_increase = final_prop_cover - prop_covered)
  
  st_write(aoi_grid, here(output_path, "aoi_street-tree-grid.geojson"))
  
  # Add back in the original vegetation canopy to include areas with height <= 1
  updated_tree_cover <- max(updated_tree_cover, canopy_height_existing, na.rm = TRUE)
  
  
  # Save the new tree cover raster
  writeRaster(updated_tree_cover, 
              here(output_path, "scenario-tree-canopy-height.tif"),
              overwrite = TRUE)
  
  # Save the new tree points
  st_write(updated_tree_points, "scenario-tree-points.geojson")
  
  # Create a logical raster where TRUE indicates cells that differ between A and B
  diff_mask <- canopy_height_existing != updated_tree_cover
  # Use the mask to create a raster that keeps values from B where they differ from A
  tree_diff_raster <- mask(updated_tree_cover, diff_mask, maskvalue = FALSE)
  
  writeRaster(tree_diff_raster, 
              here(output_path, "scenario-new-trees.tif"),
              overwrite = TRUE)
  
}
