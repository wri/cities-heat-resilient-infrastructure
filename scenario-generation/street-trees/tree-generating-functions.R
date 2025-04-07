# Load necessary libraries
library(terra)
library(sf)
library(lidR)
library(tidyverse)
library(here)

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
    tree_cover_area_value <- sum(values(masked_tree_cover_grid), na.rm = TRUE)
    ped_area_grid_value <- sum(values(ped_area_grid), na.rm = TRUE)
    
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
                           target_coverage, min_dist, crown_vectors, crown_raster) {
  
  # Indicate that trees already exist
  existing_tree_points <- existing_tree_points %>% 
    mutate(type = "existing")
  
  # Existing tree cover masked to the pedestrian area
  existing_cover_mask <- existing_tree_cover %>% 
    mask(subst(ped_area, 0, NA))
  
  binary_tree_cover <- existing_cover_mask > 1
  # binary_no_trees <- abs(binary_trees - 1)
  
  # Calculate the plantable area and current tree cover
  plantable_area_size <- sum(values(plantable_area), na.rm = TRUE)
  ped_area_size <- sum(values(ped_area), na.rm = TRUE)
  current_tree_cover_size <- sum(values(binary_tree_cover), na.rm = TRUE)
  
  # Calculate the target tree cover size
  target_tree_cover_size <- ceiling(ped_area_size * target_coverage)
  
  
  # Check if there's enough space for new trees or if any new trees are needed
  if (target_tree_cover_size <= 0 | plantable_area_size <= 0) {
    # return(NULL)  # No new trees needed
    return(list(plantable_area = plantable_area_size,
                ped_area = ped_area_size,
                beginning_tree_cover_area = current_tree_cover_size,
                target_tree_cover_area = target_tree_cover_size, 
                final_tree_cover_area = current_tree_cover_size,
                tree_points = existing_tree_points, 
                updated_tree_cover = existing_tree_cover))
  }
  
  updated_tree_cover_area <- current_tree_cover_size
  
  # Find available positions in the plantable area
  available_positions <- plantable_area %>% 
    mask(binary_tree_cover) %>% 
    subst(0, NA) %>% 
    as.points() %>% 
    st_as_sf() %>% 
    mutate(pt_id = row_number())
  
  while (updated_tree_cover_area < target_tree_cover_size) {
    
    if (nrow(available_positions) == 0) break  # No available positions
    
    # Sample a candidate point from available positions
    valid_candidate_found <- FALSE
    while (!valid_candidate_found) {
      
      # Check if there are still available positions
      if (nrow(available_positions) == 0) break  # No valid positions left
      
      # Sample a candidate point
      candidate_point <- available_positions %>% sample_n(1)
      
      # Check minimum distance constraint
      if (is_far_enough(candidate_point, existing_tree_points, min_dist)) {
        valid_candidate_found <- TRUE
        available_positions <- available_positions %>% filter(pt_id != candidate_point$pt_id)
      } else {
        # Remove the invalid candidate from available positions
        available_positions <- available_positions %>% filter(pt_id != candidate_point$pt_id)
        print(nrow(available_positions))
      }
    }
    
    # If no valid candidate was found, break out of the main loop
    if (!valid_candidate_found) break
    
    # Randomly select a tree height class based on weights
    tree_height_class <- sample(tree_structure$tree_heights, 1, prob = tree_structure$weights)
    
    # Sample a corresponding tree crown geometry for the selected height class
    sampled_geometry <- crown_vectors %>%
      filter(height == tree_height_class) %>%
      sample_n(1)
    
    # Assign the geometry to the candidate point
    candidate_tree <- candidate_point %>%
      mutate(geometry = move_polygon_to_cover_point(sampled_geometry$geometry,
                                                    candidate_point$geometry))
    
    # Get the height pixels for the tree
    candidate_tree_pixels <- crown_raster == sampled_geometry$treeID
    candidate_tree_pixels <- candidate_tree_pixels %>% 
      subst(0,  NA)
    
    # Get heights of tree pixels
    candidate_tree_pixels <- tree_height %>% 
      mask(candidate_tree_pixels) 
    
    # Move the height pixels to the desired point
    # Get the coordinates of the tree pixels
    candidate_tree_pixel_coords <- xyFromCell(candidate_tree_pixels, cells(candidate_tree_pixels))
    
    # Calculate the centroid of the tree pixels (mean of the x and y coordinates)
    candidate_tree_pixel_centroid <- colMeans(candidate_tree_pixel_coords)
    
    # Calculate the centroid of the point where the tree pixels should go
    candidate_point_rast <- rasterize(vect(candidate_point), existing_tree_cover)
    candidate_point_coords <- xyFromCell(candidate_point_rast, cells(candidate_point_rast))
    
    # Calculate the offset between the tree pixel centroid and the target point
    x_offset <- candidate_point_coords[1] - candidate_tree_pixel_centroid[1]
    y_offset <- candidate_point_coords[2] - candidate_tree_pixel_centroid[2]
    
    # Shift the tree pixels by the calculated offset
    shifted_tree <- terra::shift(candidate_tree_pixels, dx = x_offset, dy = y_offset) %>% 
      resample(existing_tree_cover)
    
    # Add the new tree point to the list
    candidate_point <- candidate_point %>% 
      select(geometry) %>% 
      mutate(treeID = max(existing_tree_points$treeID) + 1,
             height = as.numeric(tree_height_class),
             type = "new")
    
    existing_tree_points <- rbind(existing_tree_points, candidate_point)
    
    print("added tree")
    
    # Update existing tree cover by taking the cell-wise maximum
    # existing_tree_cover <- cover(shifted_tree, existing_tree_cover) 
    existing_tree_cover <- app(c(shifted_tree, existing_tree_cover), fun = max, na.rm = TRUE)
    
    # Update existing cover mask (masked to pedestrian area)
    existing_cover_mask <- existing_tree_cover %>% 
      mask(subst(ped_area, 0, NA))
    
    # Update binary tree cover
    binary_tree_cover <- existing_cover_mask > 1
    
    updated_tree_cover_area <- sum(values(binary_tree_cover), na.rm = TRUE)
    print(paste0("update area: ", updated_tree_cover_area))
    
  }
  
  return(list(plantable_area = plantable_area_size,
              ped_area = ped_area_size,
              beginning_tree_cover_area = current_tree_cover_size,
              target_tree_cover_area = target_tree_cover_size, 
              final_tree_cover_area = updated_tree_cover_area,
              tree_points = existing_tree_points, 
              updated_tree_cover = existing_tree_cover))
}
