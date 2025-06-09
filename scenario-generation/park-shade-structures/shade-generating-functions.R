# Function to generate the 5x5 meter squares only in valid raster areas
generate_squares_in_valid_area <- function(park, unshaded_raster, structure_size, shade_pct, spacing) {
  
  # Mask the unshaded raster by the park boundary to get valid placement areas
  park_raster_mask <- mask(crop(unshaded_raster, park), vect(park)) %>% 
    subst(0, NA) 
  
  park_valid_polygon <- park_raster_mask %>% 
    as.polygons() %>% 
    st_as_sf()

  # get pixels where shade could go
  
  # Buffer the park inward to exclude points where the structure will overhang
  # the park boundary
  inner_buffer <- st_buffer(park, dist = -structure_size / 2)
  
  # Safely check if buffer exists before filtering
  if (nrow(inner_buffer) == 0 || all(st_is_empty(inner_buffer))) {
    warning("Inner buffer is empty; no points retained.")
  } else {
    park_pixel_pts <- park_raster_mask %>%
      as.points(na.rm = TRUE) %>%
      st_as_sf() %>%
      mutate(id = row_number()) %>%
      filter(st_within(geometry, inner_buffer, sparse = FALSE)[, 1])
  }
  
  
  # # Ensure valid area is not empty
  # if (nrow(park_pixel_pts) == 0) {
  #   return(NULL)
  # }
  if (!exists("park_pixel_pts")) {
    return(NULL)
  }
  
  # Step 4: Calculate the target area (25% of the park area)
  # 0.25 - current shaded percent * unshaded area
  target_area <- (shade_pct - park$shaded_pct) * park$area_sqm
  # if (target_area < structure_size ^ 2){
  #   target_area <- 25
  # }
  # target_area <- as.numeric(st_area(park) * coverage_threshold)
  
  # Create an empty list to store valid points and squares
  squares <- st_sf(geometry = st_sfc(), crs = st_crs(park))
  total_square_area <- 0
  park_pixel_pts2 <- park_pixel_pts
  restart_counter <- 0  # Initialize a counter for restarts
  
  while (total_square_area < target_area) {

    # Randomly sample from park pixels
    if (nrow(park_pixel_pts2) > 0){
      
      rand_point <- park_pixel_pts2 %>% 
        sample_n(1)
      
      # print(paste0("rand point ", rand_point$id))
      
      park_pixel_pts2 <- park_pixel_pts2 %>% 
        filter(id != rand_point$id)
      
      rand_square <- rand_point %>% 
        st_buffer((structure_size/2), endCapStyle = 'SQUARE', joinStyle = 'MITRE') %>% 
        mutate(park_id = park$park_id)
      
      if (nrow(squares) == 0 || sum(st_is_within_distance(rand_square, squares, spacing, sparse = FALSE)) == 0 & st_contains(park_valid_polygon, rand_square, sparse = FALSE)) {
        
        squares <- bind_rows(squares, rand_square)
        
        # Update the total area covered
        total_square_area <- sum(sapply(squares$geometry, st_area))
        print(total_square_area)
        
      } 
    } else {
      if (total_square_area < target_area) {
        # If there are 5 restarts, 
        if (restart_counter >= 3) {
          print(paste0("Maxed out at ", nrow(squares)))
          return(squares)
        }
        # Reset park_pixel_pts2 to start over and try again
        print("No more points available. Restarting...")
        
        squares <- st_sf(geometry = st_sfc(), crs = st_crs(park))
        total_square_area <- 0
        park_pixel_pts2 <- park_pixel_pts
        
        restart_counter <- restart_counter + 1  # Increment the restart counter
        
        
        
        next  # Restart the loop from the top
      }
    }
    
  }
  
  return(squares)  # Return union of all squares as a multipolygon
}



# Accessibility (large parks) ---------------------------------------------

# Function to generate the 5x5 meter squares only in valid raster areas
# unshaded and not a pitch
shade_dist_area <- function(park, unshaded_raster, min_shade_area, max_dist_to_shade, structure_size, spacing) {
  # Mask and crop the unshaded raster to the park boundary
  park_raster_mask <- mask(crop(unshaded_raster, park), vect(park)) > 0
  
  # Generate points for unshaded areas
  park_pixel_pts <- subst(park_raster_mask, 0, NA) %>% 
    as.points(na.rm = TRUE) %>% 
    st_as_sf() %>% 
    mutate(id = row_number(), include = lengths(st_within(geometry, park)) > 0) %>% 
    filter(include == TRUE)
  
  if (nrow(park_pixel_pts) == 0) {
    return(st_sf(geometry = st_sfc(), crs = st_crs(park)))
  }
  
  # Identify contiguous shade areas and filter by minimum shade area threshold
  shade_areas <- (1 - park_raster_mask) %>% 
    patches(directions = 4, zeroAsNA = TRUE) %>% 
    as.polygons() %>% 
    st_as_sf() %>% 
    mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2")),
           park_id = park$park_id) %>% 
    filter(area_sqm >= min_shade_area)
  
  # Compute distance to the nearest shade area or park boundary
  shade_dist <- if (nrow(shade_areas) != 0) {
    distance(subst(park_raster_mask, 0, NA), shade_areas) %>% mask(vect(park))
  } else {
    distance(subst(terra::extend(park_raster_mask, c(10, 10)), NA, 2), target = 1)
  }
  
  # Compute minimum distances between shade areas
  compute_min_shade_dist <- function(shade_areas) {
    if (nrow(shade_areas) == 0) return(Inf)
    min_distances <- sapply(1:nrow(shade_areas), function(i) {
      other_polygons <- shade_areas[-i, ]
      min(st_distance(shade_areas[i, ], other_polygons))
    })
    return(max(min_distances))
  }
  
  min_shade_dist <- compute_min_shade_dist(shade_areas)
  
  # Create an empty sf object for new shade structures
  squares <- st_sf(geometry = st_sfc(), crs = st_crs(park))
  
  # Add shade structures if necessary
  while (nrow(shade_areas) == 0 || min_shade_dist > max_dist_to_shade) {
    # Identify the most distant areas from shade
    max_dist_rast <- shade_dist > quantile(values(shade_dist), 0.95, na.rm = TRUE)
    shade_pt <- subst(max_dist_rast, 0, NA) %>% 
      as.points(na.rm = TRUE) %>% 
      st_as_sf() %>% 
      sample_n(1)
    
    # Create a square shade structure around the selected point
    shade_square <- shade_pt %>% 
      st_buffer((structure_size / 2), endCapStyle = 'SQUARE', joinStyle = 'MITRE') %>% 
      mutate(park_id = park$park_id)
    
    # Update shade areas and recalculate distances
    squares <- bind_rows(squares, shade_square)
    shade_areas <- bind_rows(shade_areas, squares)
    shade_dist <- distance(subst(park_raster_mask, 0, NA), shade_areas) %>% mask(vect(park))
    min_shade_dist <- compute_min_shade_dist(shade_areas)
  }
  
  return(squares)
}



