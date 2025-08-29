# Load necessary libraries

library(sf)
library(lidR)
library(tidyverse)
library(here)
library(terra)

calc_pct_grid_cover <- function(aoi_grid, existing_tree_cover, ped_area){
  
  aoi_grid <- aoi_grid %>%
    mutate(prop_covered = NA_real_)
  
  # Loop through each grid cell and calculate the proportion of the pedestrian
  # area with tree cover
  for (i in aoi_grid$ID) {
    
    gridcell <- filter(aoi_grid, ID == i)
    
    # Extract the raster cells for the current grid cell
    ped_area_grid <- crop(subst(ped_area, 0, NA), gridcell, extend = TRUE)
    
    existing_tree_cover_grid <- crop(subst(existing_tree_cover, 0, NA), gridcell, extend = TRUE) 
    binary_tree_grid <- existing_tree_cover_grid >= 3 ### Changed to 3
    
    
    # mask tree cover to pedestrian area
    masked_tree_cover_grid <- mask(binary_tree_grid, ped_area_grid)
    
    # Calculate the number of cells with tree cover and plantable area
    tree_cover_area_value <- global(cellSize(masked_tree_cover_grid) * masked_tree_cover_grid, "sum", na.rm = TRUE)[1,1] %>%
      replace_na(0)
    ped_area_grid_value <- global(cellSize(ped_area_grid) * ped_area_grid, "sum", na.rm = TRUE)[1,1] %>%
      replace_na(0)
    
    
    # Calculate the proportion of plantable area with tree cover
    if (ped_area_grid_value > 0) {
      aoi_grid <- aoi_grid %>% 
        mutate(prop_covered = case_when(ID == i ~ tree_cover_area_value / ped_area_grid_value,
                                        .default = prop_covered))
    } else {
      aoi_grid <- aoi_grid %>% 
        mutate(prop_covered = case_when(ID == i ~ NA,
                                        .default = prop_covered))  # If no plantable area, set to NA
    }
  }
  
  return(aoi_grid)
  
}


is_far_enough <- function(point, trees_pts, min_distance) {
  distances <- as.numeric(st_distance(point, trees_pts))
  all(distances > min_distance)  # Return TRUE if all distances are greater than min_distance
}

move_polygon_to_cover_point <- function(polygon, point) {
  # Get the centroid of the polygon
  polygon_centroid <- st_coordinates(st_centroid(polygon))
  
  # Calculate the offset
  offset <- st_coordinates(point) - polygon_centroid
  
  # Move the polygon by adjusting its coordinates
  moved_polygon <- st_geometry(polygon) + matrix(offset, ncol = 2, byrow = TRUE)
  
  # Create a new polygon object with the moved coordinates
  new_polygon <- st_sfc(moved_polygon, crs = st_crs(polygon))
  
  return(new_polygon)
}

generate_trees <- function(plantable_area, ped_area, existing_tree_cover, 
                           existing_tree_points, tree_structure, tree_height,
                           target_coverage, min_dist, crown_vectors, crown_raster,
                           infrastructure_path, city_folder) {
  
  # Existing trees
  crowns <- rast(here(infrastructure_path, "existing-tree-crowns.tif"))
  canopy_height_existing <- rast(here("data", city_folder, "cif_tree_canopy.tif"))
  
  # Setup
  plantable_area <- plantable_area %>% subst(0, NA)
  plantable_area_size <- global(cellSize(plantable_area) * plantable_area, "sum", na.rm = TRUE)[1,1] %>%
    replace_na(0)
  
  ped_area <- ped_area %>% subst(0, NA)
  ped_area_size <- global(cellSize(ped_area) * ped_area, "sum", na.rm = TRUE)[1,1] %>%
    replace_na(0)
  
  current_tree_cover <- existing_tree_cover %>% mask(ped_area)
  binary_tree_cover <- current_tree_cover >= 3
  
  beginning_tree_cover_size <- global(cellSize(binary_tree_cover) * binary_tree_cover, "sum", na.rm = TRUE)[1,1] %>%
    replace_na(0)
  
  current_tree_cover_size <- beginning_tree_cover_size
  target_tree_cover_size <- ceiling(ped_area_size * target_coverage)
  
  prop_covered <- current_tree_cover_size / ped_area_size
  
  # Exit early if no space or no target
  if (target_tree_cover_size <= current_tree_cover_size | plantable_area_size <= 0) {
    return(list(
      plantable_area = plantable_area_size,
      ped_area = ped_area_size,
      beginning_tree_cover_area = beginning_tree_cover_size,
      beginning_prop_cover = prop_covered,
      target_tree_cover_area = target_tree_cover_size,
      final_tree_cover_area = current_tree_cover_size,
      new_tree_pts = st_sf(geometry = st_sfc(), crs = st_crs(existing_tree_points)),
      updated_tree_cover = existing_tree_cover,
      final_prop_cover = prop_covered
    ))
  }
  
  # Initialize
  new_tree_pts <- st_sf(geometry = st_sfc(), crs = st_crs(existing_tree_points))
  raster_stack <- list()
  tree_coords <- st_coordinates(existing_tree_points)
  
  # Find available positions
  available <- mask(plantable_area, binary_tree_cover, maskvalue = 1, updatevalue = NA) %>% 
    subst(0, NA)
  available_pts <- as.points(available) %>% 
    st_as_sf() %>% 
    mutate(pt_id = row_number())
  
  # Main loop
  while (current_tree_cover_size < target_tree_cover_size && nrow(available_pts) > 0) {
    
    candidate <- available_pts %>% sample_n(1)
    coord <- st_coordinates(candidate)
    
    if (nrow(tree_coords) > 0) {
      d <- RANN::nn2(tree_coords, query = coord, k = 1)$nn.dists[1]
      if (d <= min_dist) {
        available_pts <- available_pts %>% filter(pt_id != candidate$pt_id)
        next
      }
    }
    
    # Accept tree
    tree_height_class <- sample(tree_structure$tree_heights, 1, prob = tree_structure$weights)
    crown_geom <- crown_vectors %>% filter(height == tree_height_class) %>% sample_n(1)
    
    candidate <- candidate %>%
      mutate(height = as.numeric(tree_height_class),
             type = "new")
    
    new_tree_pts <- rbind(new_tree_pts, candidate)
    tree_coords <- rbind(tree_coords, coord)
    available_pts <- available_pts %>% filter(pt_id != candidate$pt_id)
    
    # Rasterize crown
    tree_pixels <- crown_raster == crown_geom$treeID
    tree_pixels <- subst(tree_pixels, 0, NA)
    tree_pixels <- mask(canopy_height_existing, tree_pixels)
    
    px_coords <- xyFromCell(tree_pixels, which(!is.na(values(tree_pixels))))
    if (nrow(px_coords) == 0) next
    crown_centroid <- colMeans(px_coords)
    
    point_rast <- rasterize(vect(candidate), existing_tree_cover)
    pt_coords <- xyFromCell(point_rast, which(!is.na(values(point_rast))))
    
    dx <- pt_coords[1] - crown_centroid[1]
    dy <- pt_coords[2] - crown_centroid[2]
    
    shifted <- terra::shift(tree_pixels, dx = dx, dy = dy) %>% resample(existing_tree_cover)
    raster_stack[[length(raster_stack) + 1]] <- shifted
    
    # Add new tree cover to binary tree raster
    binary_tree_cover <- binary_tree_cover | (shifted > 0)
    
    # Mask binary layer to ped area and calculate coverage
    binary_tree_cover <- binary_tree_cover * ped_area
    current_tree_cover_size <- global(cellSize(binary_tree_cover) * binary_tree_cover, "sum", na.rm = TRUE)[1,1] %>%
      replace_na(0)
    
    # Merge crown rasters
    if (length(raster_stack) > 0) {
      new_crowns <- do.call(app, c(list(rast(raster_stack)), fun = max, na.rm = TRUE))
      existing_tree_cover <- app(c(existing_tree_cover, new_crowns), fun = max, na.rm = TRUE)
    } else {
      existing_tree_cover <- existing_tree_cover
    }
  }
  
  
  
  return(list(
    plantable_area = plantable_area_size,
    ped_area = ped_area_size,
    beginning_tree_cover_area = beginning_tree_cover_size,
    beginning_prop_cover = prop_covered,
    target_tree_cover_area = target_tree_cover_size,
    final_tree_cover_area = current_tree_cover_size,
    new_tree_pts = new_tree_pts,
    updated_tree_cover = existing_tree_cover,
    final_prop_cover = current_tree_cover_size / ped_area_size
  ))
}
