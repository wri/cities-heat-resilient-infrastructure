street_trees_scenario_function <- function(city_folder, scenario, percentile = NULL, target_coverage = NULL, min_tree_dist, aoi, lulc, 
                                           road_vectors, lanes, canopy_height_existing, scenario_name, infrastructure, tile) {
  
  library(tidyverse)
  library(sf)
  library(terra)
  library(RANN)
  
  baseline_path <- here("data", city_folder, "scenarios", "baseline", tile)
  if (!dir.exists(baseline_path)) {
    dir.create(baseline_path)
  }
  
  infrastructure_path <- here("data", city_folder, "scenarios", infrastructure, tile)
  if (!dir.exists(infrastructure_path)) {
    dir.create(infrastructure_path)
  }
  
  scenario_path <- here(infrastructure_path, scenario_name, tile)
  if (!dir.exists(scenario_path)) {
    dir.create(scenario_path)
  }
  
  # Get UTM from OpenUrban
  
  utm <- st_crs(lulc)
  
  # Create a tree height raster of vegetation canopy over 3 meters
  tree_height <- canopy_height_existing
  tree_height[tree_height < 3] <- 0
  
  # Save binary tree raster to baseline folder 
  writeRaster((tree_height > 0),  here(baseline_path, "tree_cover_baseline.tif"),
              overwrite = TRUE)
  
  # Tile boundary
  tile_geom <- st_as_sf(as.polygons(ext(tree_height)))
  st_crs(tile_geom) <- utm
  
  # Process tree canopy to individual trees ---------------------------------
    
  source(here("scenario-generation", "street-trees", "canopy-to-trees-function.R"))
  
  process_trees(tree_raster = tree_height, 
                infrastructure_path = infrastructure_path)
  
  load(here(infrastructure_path, "tree-vars.RData"))
  st_write(ttops, here(baseline_path, "tree_points_baseline.geojson"), append = FALSE, delete_dsn = TRUE)
  
  crowns <- rast(here(infrastructure_path, "existing-tree-crowns.tif"))
    
  
  # Plantable area  -------------------------------------------------------
    
  source(here("scenario-generation", "street-trees", "plantable-street-function-tile.R"))
  
  plantable_street <- generate_plantable_street(aoi = tile_geom, 
                                                lulc_rast = lulc, 
                                                existing_trees = tree_height,
                                                road_vectors = road_vectors, 
                                                lanes = lanes,
                                                city_folder = city_folder,
                                                utm = utm,
                                                tile = tile)
  
  ped_area <- rast(here(baseline_path, "pedestrian_areas.tif"))
  plantable_street <- rast(here(infrastructure_path, "plantable_areas.tif"))
  
  rm(road_vectors, lanes)
  
  # Evaluate baseline conditions --------------------------------------------

  # Create a grid over the tile geometery to iterate over for creating trees
  # intersect with AOI so only areas within the aoi are planted
  aoi_grid <- tile_geom %>% 
    st_make_grid(cellsize = c(100, 100), square = TRUE, what = "polygons") %>% 
    st_sf() %>% 
    st_intersection(aoi) %>% 
    select(geometry) %>% 
    mutate(ID = row_number())
  
  # Calculate the existing percent cover of trees in pedestrian areas for each gridcell
  source(here("scenario-generation", "street-trees", "tree-generating-functions.R"))
  
  # Achievable potential ----------------------------------------------------
  aws_path <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/", 
                     city, "/scenarios/street-trees/", 
                     city, "-street-tree-pct-1km-grid.csv")
    
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
  
  # Initialize outputs
  updated_tree_cover <- canopy_height_existing  
  updated_tree_points <- st_sf(geometry = st_sfc(), crs = utm$epsg)
  
  # Ensure area columns exist in aoi_grid
  aoi_grid <- aoi_grid %>%
    mutate(
      plantable_area = NA_real_,
      pedestrian_area = NA_real_,
      beginning_tree_cover_area = NA_real_,
      beginning_prop_cover = NA_real_, 
      target_tree_cover_area = NA_real_,
      final_tree_cover_area = NA_real_,
      final_prop_cover = NA_real_
    )
  
  # Loop over grid cells
  for (i in pull(aoi_grid, ID)) {
    
    gridcell <- filter(aoi_grid, ID == i)
    
    # Crop rasters to gridcell
    plantable_grid <- crop(plantable_street, gridcell)
    existing_tree_cover_grid <- crop(updated_tree_cover, gridcell)
    ped_area_grid <- crop(ped_area, gridcell)
    tree_height_grid <- crop(tree_height, gridcell)
    
    # Filter existing tree points to current grid
    existing_tree_points_grid <- st_filter(ttops, gridcell)
    
    # Generate trees for the grid cell
    updated <- generate_trees(
      plantable_area = plantable_grid,
      ped_area = ped_area_grid,
      existing_tree_cover = existing_tree_cover_grid,
      existing_tree_points = existing_tree_points_grid,
      tree_structure = tree_structure,
      tree_height = tree_height_grid,
      target_coverage = target_coverage,
      min_dist = min_tree_dist,
      crown_vectors = crown_vectors,
      crown_raster = crowns,
      infrastructure_path = infrastructure_path,
      city_folder = city_folder
    )
    
    # Merge updated raster back into global raster
    updated_tree_cover <- terra::merge(updated$updated_tree_cover, updated_tree_cover)

    # Append new tree points
    updated_tree_points <- bind_rows(updated_tree_points, updated$new_tree_pts)
    
    # Update area summaries in grid
    aoi_grid <- aoi_grid %>%
      mutate(
        plantable_area = if_else(ID == i, updated$plantable_area, plantable_area),
        pedestrian_area = if_else(ID == i, updated$ped_area, pedestrian_area),
        beginning_tree_cover_area = if_else(ID == i, updated$beginning_tree_cover_area, beginning_tree_cover_area),
        beginning_prop_cover = if_else(ID == i, updated$beginning_prop_cover, beginning_prop_cover),
        target_tree_cover_area = if_else(ID == i, updated$target_tree_cover_area, target_tree_cover_area),
        final_tree_cover_area = if_else(ID == i, updated$final_tree_cover_area, final_tree_cover_area),
        final_prop_cover = if_else(ID == i, updated$final_prop_cover, final_prop_cover)
      )
  }
  
  st_write(aoi_grid, 
           dsn = here(scenario_path, "aoi_street-tree-grid.geojson"),
           delete_dsn = TRUE,
           append = FALSE)
  
  # Add back in the original vegetation canopy to include areas with height <= 1
  updated_tree_cover <- max(updated_tree_cover, canopy_height_existing, na.rm = TRUE)
  
  # Save the new tree height raster
  writeRaster(updated_tree_cover, 
              here(scenario_path, "scenario-tree-canopy-height.tif"),
              overwrite = TRUE)
  
  # Save the new tree cover raster
  writeRaster((updated_tree_cover >= 3), 
              here(scenario_path, "scenario-tree-cover.tif"),
              overwrite = TRUE)
  
  # Save the new tree points
  st_write(updated_tree_points, 
           dsn = here(scenario_path, "tree_points_achievable.geojson"),
           append = FALSE,
           delete_dsn = TRUE)
  
  # Create a logical raster where TRUE indicates cells that differ between A and B
  diff_mask <- canopy_height_existing != updated_tree_cover
  # Use the mask to create a raster that keeps values from B where they differ from A
  tree_diff_raster <- mask(updated_tree_cover, diff_mask, maskvalue = FALSE) >= 3
  
  writeRaster(tree_diff_raster, 
              here(scenario_path, "tree_cover_achievable.tif"),
              overwrite = TRUE)

}
