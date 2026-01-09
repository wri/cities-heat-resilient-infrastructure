library(here)
library(terra)
library(sf)
library(tidyverse)
library(lidR)
library(RANN)
library(glue)


source(here("tiling-scripts", "utils.R"))

# Make binary tree cover --------------------------------------------------

make_binary_tree_cover <- function(tree_canopy){
  binary_tree <- tree_canopy >= 3
  return(binary_tree)
}


# process urban extent trees ----------------------------------------------


process_trees_staged_extent <- function(city){
  
  # list tif files in s3://wri-cities-tcm/data/pre-release/layers/TreeCanopyHeightCTCM/tif/
  keys <- list_s3_keys(
    bucket = "wri-cities-tcm",
    prefix = glue("data/pre-release/layers/TreeCanopyHeightCTCM/tif/{city}__urban_extent__TreeCanopyHeightCTCM__bufferm_700.tif")
  )
  
  tif_files <- keys[grepl("\\.tif$", keys, ignore.case = TRUE)]
  tif_paths <- paste0("s3://wri-cities-tcm/", tif_files)
  
  # process trees and save in 
  for (p in tif_paths){
    print(p)
    tile_name <- basename(p) %>% 
      str_remove(".tif")
    
    tree_canopy <- rast_retry(p)
    tree_canopy[tree_canopy < 3] <- 0
    
    # locate trees
    ttops <- locate_trees(tree_canopy, lmf(5)) 
    if(nrow(ttops) == 0){
      return(invisible(NULL))
    }
    
    # segment crowns
    crowns <- dalponte2016(tree_canopy, ttops)()
    names(crowns) <- "treeID"
    
    # crown vectors
    crown_vectors <- crowns %>% 
      as.polygons() %>% 
      st_as_sf() %>% 
      left_join(st_drop_geometry(ttops), by = "treeID") %>% 
      rename(height = Z)  %>% 
      mutate(tile = tile_name) 
    
    ttops <- ttops %>% 
      rename(height = Z) %>% 
      st_zm(drop = TRUE, what = "ZM")
    
    ensure_s3_prefix(bucket, glue("{city_folder}/scenarios/trees/tree-population/"))
    
    write_s3(ttops, glue("{bucket}/{city_folder}/scenarios/trees/tree-population/{tile_name}_tree-points.geojson"))
    write_s3(crowns, glue("{bucket}/{city_folder}/scenarios/trees/tree-population/{tile_name}_existing-tree-crowns.tif"))
    write_s3(crown_vectors, glue("{bucket}/{city_folder}/scenarios/trees/tree-population/{tile_name}_existing-tree-crowns.geojson"))
    
  }
  
  
  
}

# Process tree canopy to trees --------------------------------------------
# s3://wri-cities-tcm/data/pre-release/layers/TreeCanopyHeightCTCM/tif/
process_trees <- function(tree_canopy, city_folder, baseline_folder, t_id){
  
  tile <- tile_grid %>% 
    filter(tile_name == t_id)
  
  # Load raster into memory
  tree_canopy[tree_canopy < 3] <- 0
  tree_cover <- tree_canopy >= 3
  
  write_s3(tree_cover, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/tree-cover__baseline__baseline.tif"))
  
  # locate trees
  ttops <- locate_trees(tree_canopy, lmf(5)) 
  if(nrow(ttops) == 0){
    return(invisible(NULL))
  }
  
  # segment crowns
  crowns <- dalponte2016(tree_canopy, ttops)()
  names(crowns) <- "treeID"

  # crown vectors
  crown_vectors <- crowns %>%
    as.polygons() %>%
    st_as_sf() %>%
    left_join(st_drop_geometry(ttops), by = "treeID") %>%
    rename(height = Z)  %>%
    mutate(tile = t_id) %>%
    st_filter(tile_grid, .predicate = st_within)

  ttops <- ttops %>%
    rename(height = Z) %>%
    st_zm(drop = TRUE, what = "ZM")
  
  write_s3(ttops, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/tree-points__baseline__baseline.geojson"))
  write_s3(crowns, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/existing-tree-crowns__baseline__baseline.tif"))
  write_s3(crown_vectors, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/existing-tree-crowns__baseline__baseline.geojson"))
}


# Create pedestrian area --------------------------------------------------

# create_pedestrian_area <- function(lulc, open_urban_aws_http){
#   
#   utm <- st_crs(lulc)
#   
#   # tile bounds
#   tile_geom <- st_as_sf(as.polygons(ext(lulc)))
#   st_crs(tile_geom) <- utm
#   
#   # Load roads and filter to tile geometry
#   road_vectors <- sfarrow::st_read_parquet(
#     glue("{open_urban_aws_http}/roads/roads_all.parquet"),
#     wkt_filter = st_as_text(tile_geom),   
#     quiet = TRUE) %>% 
#     st_transform(utm) %>% 
#     st_filter(tile_geom)
#   
#   # Subset to low-traffic volume roads
#   lanes <- read_csv(glue("{open_urban_aws_http}/roads/average_lanes.csv"))
#   
#   ped_roads_list <- c("tertiary",
#                       "tertiary_link",
#                       "residential",
#                       "living_street")
#   
#   road_vectors <- road_vectors %>% 
#     select(highway, lanes) %>% 
#     mutate(lanes = as.integer(lanes)) %>% 
#     left_join(lanes, by = "highway") %>% 
#     mutate(lanes = coalesce(lanes, avg_lanes))
#   
#   ped_road_vectors <- road_vectors %>% 
#     filter(highway %in% ped_roads_list)
#   
#   # Buffer roads by lanes * 10 ft (3.048 m) 
#   # https://nacto.org/publication/urban-street-design-guide/street-design-elements/lane-width/#:~:text=wider%20lane%20widths.-,Lane%20widths%20of%2010%20feet%20are%20appropriate%20in%20urban%20areas,be%20used%20in%20each%20direction
#   # cap is flat to the terminus of the road
#   # join style is mitred so intersections are squared
#   if (utm$units == "us-ft"){
#     width = 10
#   } else if (utm$units == "ft"){
#     width = 10
#   } else if (utm$units == "m"){
#     width = 3.048
#   } 
#   
#   ped_roads_buff <- ped_road_vectors %>% 
#     st_buffer(dist = ped_road_vectors$lanes * (width / 2),
#               endCapStyle = "FLAT",
#               joinStyle = "MITRE") 
#   
#   ped_roads_area <- rasterize(ped_roads_buff, lulc, field = 1, background = 0)
#   
#   # 5-meter buffer 
#   pedestrian_area <- ped_roads_area %>% 
#     subst(0, NA) %>% 
#     buffer(5) %>% 
#     as.numeric()
#   
#   # Remove roads and water
#   pedestrian_area <- pedestrian_area * abs((floor(lulc / 100) %in% c(3, 5)) - 1)
#   return(pedestrian_area)
# }
# 

# Create plantable area ---------------------------------------------------

create_plantable_area <- function(lulc, pedestrian_area, binary_tree_cover, open_urban_aws_http, t){
  
  utm <- st_crs(lulc)
  
  # tile bounds
  tile_geom <- st_as_sf(as.polygons(ext(lulc)))
  st_crs(tile_geom) <- utm
  
  ## Buildings buffer ####
  # buildings buffer 5-m
  builds <- floor(lulc / 100) == 6
  
  builds_buff <- builds %>% 
    subst(0, NA) %>% 
    buffer(5)
  
  builds_buff <- abs(builds_buff - builds - 1) 
  
  ## intersections buffer ####
  # no trees within 9-m of intersection
  
  # Load roads and filter to bbox of tile geometry
  road_vectors <- st_read(glue("{open_urban_aws_http}/roads/roads_all.geojson"), quiet = TRUE) %>% 
      st_transform(utm) %>% 
      st_filter(tile_geom)
  
  
  if (nrow(road_vectors) == 0) {
    plantable_street <- lulc
    values(plantable_street) <- 0
    return(plantable_street)
  }
  
  # Dissolve road segments 
  dissolved_roads <- st_union(road_vectors)
  
  # Convert the dissolved result back into an sf object
  dissolved_roads_sf <- st_as_sf(data.frame(geometry = dissolved_roads)) %>% 
    st_cast("LINESTRING")
  
  # Find intersections of the dissolved roads
  intersections <- st_intersection(dissolved_roads_sf)
  
  # Keep only the points where roads intersect
  intersection_points <- intersections[st_geometry_type(intersections) == "POINT", ] %>% 
    st_union() %>% 
    st_as_sf() %>% 
    st_cast("POINT")
  
  intersection_buffer <- intersection_points %>% 
    st_buffer(dist = 9) %>% 
    rasterize(lulc, field = 0, background = 1)
  
  ## Plantable area ####
  # green space, built up other, barren, open space can be planted
  # water, roads, building, parking cannot
  plantable_lulc <- floor(lulc / 100) %in% c(1, 2)
  
  # remove building buffer ####
  plantable_lulc <- plantable_lulc * builds_buff
  
  # remove intersections buffer ####
  plantable_lulc <- plantable_lulc * intersection_buffer
  
  # Street plantable area
  plantable_street <- plantable_lulc * pedestrian_area
  
  # Remove major roads
  # Subset to low-traffic volume roads
  lanes <- suppressMessages(
    readr::read_csv(glue("{open_urban_aws_http}/roads/average_lanes.csv"))
  ) %>%
    dplyr::rename(
      avg_lanes = dplyr::coalesce(
        dplyr::any_of("avg_lanes"),
        dplyr::any_of("avg.lanes")
      )
    )
  
  
  major_roads_list <- c("motorway", "primary")
  
  road_vectors <- road_vectors %>% 
    select(highway, lanes) %>% 
    mutate(lanes = as.integer(lanes)) %>% 
    left_join(lanes, by = "highway") %>% 
    mutate(lanes = coalesce(lanes, avg_lanes))
  
  major_road_vectors <- road_vectors %>% 
    filter(highway %in% major_roads_list)
  
  if (nrow(major_road_vectors) != 0){
    
    # Buffer roads by lanes * 10 ft (3.048 m) 
    # https://nacto.org/publication/urban-street-design-guide/street-design-elements/lane-width/#:~:text=wider%20lane%20widths.-,Lane%20widths%20of%2010%20feet%20are%20appropriate%20in%20urban%20areas,be%20used%20in%20each%20direction
    # cap is flat to the terminus of the road
    # join style is mitred so intersections are squared
    if (utm$units == "us-ft"){
      width = 10
    } else if (utm$units == "ft"){
      width = 10
    } else if (utm$units == "m"){
      width = 3.048
    } 
    
    major_roads_mask <- major_road_vectors %>% 
      st_buffer(dist = major_road_vectors$lanes * (width / 2),
                endCapStyle = "FLAT",
                joinStyle = "MITRE") %>% 
      rasterize(lulc, background = NA)
    
    major_roads_distance <- distance(major_roads_mask)
    major_roads_distance <- (major_roads_distance > 0) & (major_roads_distance <= 5) 
    
    plantable_street <- plantable_street * (major_roads_distance == 0)
  }
  
  # Remove areas of existing tree cover
  plantable_street <- plantable_street * (binary_tree_cover < 1)
  
  ensure_s3_prefix(
    bucket,
    glue("{scenario_folder}/{t}/ccl_layers/")
  )
  
  return(plantable_street)
}


# Baseline tree processing per tile ---------------------------------------

# baseline folder is the output of the baseline CTCM run
baseline_processing <- function(t){
  print(t)
  
  ensure_s3_prefix(
    bucket,
    glue("{baseline_folder}/{t}/ccl_layers/")
  )

  # Load data
  tree_canopy <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_tree_canopy.tif"))
  lulc <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_open_urban.tif"))

  # Create and save binary tree cover
  # binary_tree_cover <- make_binary_tree_cover(tree_canopy)
  # write_s3(binary_tree_cover, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/tree-cover.tif"))

  # process_trees(tree_canopy, city_folder, baseline_folder, t)

  # Create pedestrian area
  # pedestrian_area <- create_pedestrian_area(lulc, open_urban_aws_http)
  # write_s3(pedestrian_area, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/pedestrian-areas.tif"))
  binary_tree_cover <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/tree-cover__baseline__baseline.tif"))
  pedestrian_area <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/pedestrian-areas__baseline__baseline.tif"))

  # Create plantable area
  plantable_street <- create_plantable_area(lulc, pedestrian_area, binary_tree_cover, open_urban_aws_http, t)
  write_s3(plantable_street, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/plantable-areas__trees__{scenario}.tif"))
  
  # Create available points to place trees
  available_pts <- as.points(subst(plantable_street, from = 0, to = NA)) %>%
    st_as_sf() %>%
    mutate(pt_id = row_number())

  write_s3(available_pts, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/plantable-points.geojson"))
}


# Create city-wide tree population ----------------------------------------

create_tree_population <- function(tiles_s3){
  
  keys <- list_s3_keys(
    bucket = "wri-cities-tcm",
    prefix = glue("{bucket}/{city_folder}/scenarios/trees/tree-population/")
  )
  
  tif_files <- keys[grepl("\\.tif$", keys, ignore.case = TRUE)]
  tree_paths <- paste0("s3://wri-cities-tcm/", tif_files)

  tree_paths <- glue("{aws_http}/{baseline_folder}/{tiles_s3}/ccl_layers/tree-points__baseline__baseline.geojson")
  
  # Safe reader (returns NULL if file missing)
  safe_read <- possibly(~ st_read(.x, quiet = TRUE), otherwise = NULL)
  
  trees <- map(tree_paths, safe_read) %>%
    compact() %>%       # drop NULLs
    bind_rows() 
  
  # Probabilities for tree height classes
  tree_structure <- tibble(
    tree_classes = c("small", "medium", "large"),
    tree_heights = c(ceiling(quantile(trees$height, 0.25)),
                     ceiling(quantile(trees$height, 0.50)),
                     ceiling(quantile(trees$height, 0.75))),
    weights = c(0.25, 0.50, 0.25)
  )
  
  # Filter crowns to only those with heights in tree structure
  crown_paths <- glue("{aws_http}/{baseline_folder}/{tiles_s3}/ccl_layers/existing-tree-crowns__baseline__baseline.geojson")
  crowns <- map(crown_paths, ~ {
    x <- safe_read(.x)
    if (!is.null(x)) {
      x %>% filter(height %in% tree_structure$tree_heights)
    } else {
      NULL
    }
  }) %>%
    compact() %>%
    bind_rows()
  
  ensure_s3_prefix(bucket, glue("{city_folder}/scenarios/trees/tree-population/"))
  
  write_s3(trees, glue("{bucket}/{city_folder}/scenarios/trees/tree-population/trees.geojson"))
  write_s3(crowns, glue("{bucket}/{city_folder}/scenarios/trees/tree-population/crowns.geojson"))
  write_s3(tree_structure, glue("{bucket}/{city_folder}/scenarios/trees/tree-population/tree-structure.csv"))
}


# Plant new trees ---------------------------------------------------------


# Gridcell is grid geometry
plant_in_gridcell <- function(grid_index, aoi_grid, target_coverage, min_dist,
                              trees, crowns, tree_structure){
  
  gridcell <- aoi_grid %>%
    filter(ID == grid_index)
  
  print(gridcell$ID)
  
  # Buffer gridcell by 10 meters
  gridcell_buffered <- gridcell %>% 
    st_buffer(dist = 10)
  
  buffered_tile_names <- gridcell %>% 
    st_intersection(buffered_tile_grid) %>% 
    pull(tile_name)
  
  unbuffered_tile_names <- gridcell %>% 
    st_intersection(tile_grid) %>% 
    pull(tile_name)
  
  # Existing trees
  existing_tree_points <- trees %>%
    st_filter(gridcell_buffered)
  existing_tree_coords <- st_coordinates(existing_tree_points)
  
  # new_tree_pts <- new_trees[0,]
  
  # Existing tree canopy
  canopy_paths <- glue("{aws_http}/{baseline_folder}/{unbuffered_tile_names}/raster_files/cif_tree_canopy.tif")
  canopy_height_existing <- load_and_merge(canopy_paths) 
  canopy_height_gridcell <- canopy_height_existing %>%
    crop(gridcell)
  
  # Plantable area for the gridcell
  plantable_paths <- glue("{aws_http}/{scenario_folder}/{unbuffered_tile_names}/ccl_layers/plantable-areas__trees__pedestrian-achievable-90pctl.tif")
  plantable_area <- load_and_merge(plantable_paths) %>% 
    crop(gridcell) %>% 
    subst(from = 0, to = NA)
  
  # Total plantable area for the gridcell, if NA make 0
  plantable_area_size <- global(cellSize(plantable_area) * plantable_area, "sum", na.rm = TRUE)[1,1] %>%
    replace_na(0)
  
  # Pedestrian area for the gridcell
  ped_area_paths <- glue("{aws_http}/{baseline_folder}/{unbuffered_tile_names}/ccl_layers/pedestrian-areas__baseline__baseline.tif")
  ped_area <- load_and_merge(ped_area_paths) %>% 
    crop(gridcell) %>% 
    subst(from = 0, to = NA)
  
  # Total pedestrian area for the gridcell, if NA make 0
  ped_area_size <- global(cellSize(ped_area) * ped_area, "sum", na.rm = TRUE)[1,1] %>%
    replace_na(0)
  
  # Current tree cover
  current_tree_cover_paths <- glue("{aws_http}/{baseline_folder}/{unbuffered_tile_names}/ccl_layers/tree-cover__baseline__baseline.tif")
  current_tree_cover <- load_and_merge(current_tree_cover_paths) %>% 
    crop(gridcell) %>% 
    subst(from = 0, to = NA) %>% 
    mask(ped_area)
  
  tree_cover_size <- global(cellSize(current_tree_cover) * current_tree_cover, "sum", na.rm = TRUE)[1,1] %>%
    replace_na(0)
  
  # Initialize tree points
  new_tree_pts <- st_sf(
    height   = numeric(),
    type     = character(),
    geometry = st_sfc(crs = st_crs(current_tree_cover))
  )
  
  # Metrics for gridcell
  # tree_cover_size <- beginning_tree_cover_size
  target_tree_cover_size <- ceiling(ped_area_size * target_coverage)
  prop_covered <- tree_cover_size / ped_area_size
  
  # If the current tree cover already exceeds the target OR if there is
  # no plantable area, skip this gridcell
  if (target_tree_cover_size <= tree_cover_size | plantable_area_size <= 0) {
    print("No trees planted")
    return(NULL)
  }
  
  # Find available positions
  available_pts <- as.points(plantable_area) %>% 
    st_as_sf() %>% 
    mutate(pt_id = row_number())
  
  # Plant new trees until the target is approximated
  while (tree_cover_size < target_tree_cover_size && nrow(available_pts) > 0) {
    
    # Sample a point available for tree planting
    candidate <- available_pts %>% sample_n(1)
    coord <- st_coordinates(candidate)
    
    # Check the distance from existing trees
    # If the point is too close to existing trees, remove it from the available 
    # points and choose another
    if (nrow(existing_tree_coords) > 0) {
      d <- RANN::nn2(existing_tree_coords, query = coord, k = 1)$nn.dists[1]
      if (d <= min_dist) {
        available_pts <- available_pts %>% filter(pt_id != candidate$pt_id)
        next
      }
    }
    
    # If the point meets the minimum distance requirement, simulate a tree
    
    # sample a tree height
    tree_height_class <- sample(tree_structure$tree_heights, 1, prob = tree_structure$weights)
    # sample a crown geometry from the tree height class
    crown_geom <- crowns %>% 
      filter(height == tree_height_class) %>% 
      sample_n(1)
    
    # Assign the point the tree height
    candidate <- candidate %>%
      mutate(height = as.numeric(tree_height_class),
             type = "new")
    
    # Add to new tree points
    new_tree_pts <- rbind(new_tree_pts, candidate)
    # Add to existing tree coordinates
    existing_tree_coords <- rbind(existing_tree_coords, coord)
    # Remove from available points
    available_pts <- available_pts %>% filter(pt_id != candidate$pt_id)
    
    # Rasterize crown
    crown_pixels <- rast_retry(glue("{aws_http}/{baseline_folder}/{crown_geom$tile}/ccl_layers/existing-tree-crowns__baseline__baseline.tif")) %>% 
      crop(crown_geom)
    crown_pixels <- crown_pixels == crown_geom$treeID
    crown_pixels <- subst(crown_pixels, from = 0, to = NA)
    
    height_pixels <- rast_retry(glue("{aws_http}/{baseline_folder}/{crown_geom$tile}/raster_files/cif_tree_canopy.tif")) %>% 
      crop(crown_geom) 
    tree_pixels <- mask(height_pixels, crown_pixels)
    
    px_coords <- xyFromCell(tree_pixels, which(!is.na(values(tree_pixels))))
    if (nrow(px_coords) == 0) next
    crown_centroid <- colMeans(px_coords)
    
    point_rast <- rasterize(vect(candidate), canopy_height_gridcell)
    pt_coords <- xyFromCell(point_rast, which(!is.na(values(point_rast))))
    
    dx <- pt_coords[1] - crown_centroid[1]
    dy <- pt_coords[2] - crown_centroid[2]
    
    shifted <- terra::shift(tree_pixels, dx = dx, dy = dy) %>% 
      resample(canopy_height_gridcell, method = "near")
    
    # Add new tree cover to binary tree raster
    current_tree_cover <- current_tree_cover | (shifted > 0)
    
    # Mask binary layer to ped area and calculate coverage
    current_tree_cover <- current_tree_cover * ped_area
    tree_cover_size <- global(cellSize(current_tree_cover) * current_tree_cover, "sum", na.rm = TRUE)[1,1] %>%
      replace_na(0)
    
    # update gridcell tree canopy
    if (hasValues(shifted)) {
      canopy_height_gridcell <- max(canopy_height_gridcell, shifted, na.rm = TRUE)
    } else {
      canopy_height_gridcell <- canopy_height_gridcell
    }
  }
  
  # Update tree canopy for all intersecting buffered tree canopy layers
  update_paths <- glue("{aws_http}/{scenario_folder}/{buffered_tile_names}/raster_files/tree_canopy.tif")
  
  for (p in update_paths) {
    r <- rast_retry(p)
    # Make canopy_height_gridcell the same extent as r
    canopy_height_gridcell <- canopy_height_gridcell %>% 
      extend(r) %>% 
      crop(r)
    
    updated_canopy <- max(r, canopy_height_gridcell, na.rm = TRUE)
    updated_p <- p %>% str_replace("https://wri-cities-tcm.s3.us-east-1.amazonaws.com",
                                   "wri-cities-tcm")
    write_s3(updated_canopy, updated_p)
  }
  
  return(new_tree_pts)
  
}

plant_in_gridcell_fast <- function(grid_index, aoi_grid, target_coverage, min_dist,
                                   trees, crowns, tree_structure) {
  
  # ---- gridcell selection (avoid dplyr overhead) ----
  gridcell <- aoi_grid[aoi_grid$ID == grid_index, ]
  print(gridcell$ID)
  
  # Buffer gridcell by 10 meters
  gridcell_buffered <- sf::st_buffer(gridcell, dist = 10)
  
  buffered_tile_names <- sf::st_intersection(gridcell, buffered_tile_grid)$tile_name
  unbuffered_tile_names <- sf::st_intersection(gridcell, tile_grid)$tile_name
  
  # Existing trees
  existing_tree_points <- sf::st_filter(trees, gridcell_buffered)
  existing_tree_coords <- sf::st_coordinates(existing_tree_points) # matrix (n x 2)
  
  # Existing tree canopy
  canopy_paths <- glue::glue(
    "{aws_http}/{baseline_folder}/{unbuffered_tile_names}/raster_files/cif_tree_canopy.tif"
  )
  canopy_height_existing <- load_and_merge(canopy_paths)
  canopy_height_gridcell <- terra::crop(canopy_height_existing, terra::vect(gridcell))
  
  # Plantable area for the gridcell
  plantable_paths <- glue::glue(
    "{aws_http}/{scenario_folder}/{unbuffered_tile_names}/ccl_layers/plantable-areas__trees__pedestrian-achievable-90pctl.tif"
  )
  plantable_area <- load_and_merge(plantable_paths) |>
    terra::crop(terra::vect(gridcell)) |>
    terra::subst(from = 0, to = NA)
  
  # Pedestrian area for the gridcell
  ped_area_paths <- glue::glue(
    "{aws_http}/{baseline_folder}/{unbuffered_tile_names}/ccl_layers/pedestrian-areas__baseline__baseline.tif"
  )
  ped_area <- load_and_merge(ped_area_paths) |>
    terra::crop(terra::vect(gridcell)) |>
    terra::subst(from = 0, to = NA)
  
  # Current tree cover
  current_tree_cover_paths <- glue::glue(
    "{aws_http}/{baseline_folder}/{unbuffered_tile_names}/ccl_layers/tree-cover__baseline__baseline.tif"
  )
  current_tree_cover <- load_and_merge(current_tree_cover_paths) |>
    terra::crop(terra::vect(gridcell)) |>
    terra::subst(from = 0, to = NA) |>
    terra::mask(ped_area)
  
  # ---- precompute cell areas once ----
  # (same method as before: sum(cellSize * binaryRaster))
  cell_area_template <- terra::cellSize(current_tree_cover)
  plantable_area_size <- terra::global(cell_area_template * plantable_area, "sum", na.rm = TRUE)[1, 1] |>
    tidyr::replace_na(0)
  ped_area_size <- terra::global(cell_area_template * ped_area, "sum", na.rm = TRUE)[1, 1] |>
    tidyr::replace_na(0)
  tree_cover_size <- terra::global(cell_area_template * current_tree_cover, "sum", na.rm = TRUE)[1, 1] |>
    tidyr::replace_na(0)
  
  target_tree_cover_size <- ceiling(ped_area_size * target_coverage)
  
  # Same skip logic
  if (target_tree_cover_size <= tree_cover_size | plantable_area_size <= 0) {
    print("No trees planted")
    return(NULL)
  }
  
  # ---- build available planting positions as cell indices (fast) ----
  # Equivalent to sampling from as.points(plantable_area) uniformly.
  plant_vals <- terra::values(plantable_area, mat = FALSE)
  avail_cells <- which(!is.na(plant_vals))
  rm(plant_vals)
  
  if (length(avail_cells) == 0) {
    print("No trees planted")
    return(NULL)
  }
  
  # ---- crown lookup optimization ----
  # Pre-split crowns by height so each draw is O(1) indexing instead of dplyr filter each time.
  crowns_by_h <- split(crowns, crowns$height)
  
  # ---- cache rasters per tile to avoid repeated S3 reads ----
  tile_cache <- new.env(parent = emptyenv())
  get_tile_rasters <- function(tile) {
    key <- as.character(tile)
    if (exists(key, envir = tile_cache, inherits = FALSE)) {
      return(get(key, envir = tile_cache, inherits = FALSE))
    }
    crowns_r <- terra::rast_retry(glue::glue(
      "{aws_http}/{baseline_folder}/{tile}/ccl_layers/existing-tree-crowns__baseline__baseline.tif"
    ))
    height_r <- terra::rast_retry(glue::glue(
      "{aws_http}/{baseline_folder}/{tile}/raster_files/cif_tree_canopy.tif"
    ))
    val <- list(crowns_r = crowns_r, height_r = height_r)
    assign(key, val, envir = tile_cache)
    val
  }
  
  # ---- collect outputs without rbind-in-loop ----
  new_pts_list <- vector("list", 0L)
  new_h_list <- numeric(0)
  new_type_list <- character(0)
  
  # ---- main planting loop ----
  while (tree_cover_size < target_tree_cover_size && length(avail_cells) > 0) {
    
    # sample an available cell index and remove it (same uniform sampling)
    j <- sample.int(length(avail_cells), 1L)
    cell <- avail_cells[[j]]
    avail_cells[[j]] <- avail_cells[[length(avail_cells)]]
    avail_cells <- avail_cells[-length(avail_cells)]
    
    # candidate coordinate at the cell center
    coord <- terra::xyFromCell(plantable_area, cell)
    coord <- matrix(coord, nrow = 1)  # for RANN query
    
    # Check min distance from existing trees (same as original)
    if (nrow(existing_tree_coords) > 0) {
      d <- RANN::nn2(existing_tree_coords, query = coord, k = 1)$nn.dists[1]
      if (d <= min_dist) next
    }
    
    # Sample tree height
    tree_height_class <- sample(tree_structure$tree_heights, 1, prob = tree_structure$weights)
    
    # Sample crown geometry from that height class
    crowns_h <- crowns_by_h[[as.character(tree_height_class)]]
    if (is.null(crowns_h) || nrow(crowns_h) == 0) next
    crown_geom <- crowns_h[sample.int(nrow(crowns_h), 1L), ]
    
    # Create candidate point sf (same CRS expectation as before)
    # NOTE: this matches the prior approach of an sf point at the sampled location.
    candidate_pt <- sf::st_sf(
      height = as.numeric(tree_height_class),
      type   = "new",
      geometry = sf::st_sfc(
        sf::st_point(as.numeric(coord[1, ])),
        crs = sf::st_crs(gridcell)
      )
    )
    
    
    # Record new point
    new_pts_list[[length(new_pts_list) + 1L]] <- sf::st_point(as.numeric(coord[1, ]))
    new_h_list   <- c(new_h_list, as.numeric(tree_height_class))
    new_type_list<- c(new_type_list, "new")
    
    # Update existing coords for next iterations (same behavior)
    existing_tree_coords <- rbind(existing_tree_coords, coord)
    
    # ---- rasterize/shift crown the same way, but with cached tile rasters ----
    tr <- get_tile_rasters(crown_geom$tile)
    
    crown_pixels <- terra::crop(tr$crowns_r, terra::vect(crown_geom))
    crown_pixels <- crown_pixels == crown_geom$treeID
    crown_pixels <- terra::subst(crown_pixels, from = 0, to = NA)
    
    height_pixels <- terra::crop(tr$height_r, terra::vect(crown_geom))
    tree_pixels <- terra::mask(height_pixels, crown_pixels)
    
    px_cells <- which(!is.na(terra::values(tree_pixels, mat = FALSE)))
    if (length(px_cells) == 0) next
    px_coords <- terra::xyFromCell(tree_pixels, px_cells)
    crown_centroid <- colMeans(px_coords)
    
    # Candidate point coordinates (as in original via rasterize -> xyFromCell)
    pt_coords <- coord[1, ]
    
    dx <- pt_coords[1] - crown_centroid[1]
    dy <- pt_coords[2] - crown_centroid[2]
    
    shifted <- terra::shift(tree_pixels, dx = dx, dy = dy) |>
      terra::resample(canopy_height_gridcell, method = "near")
    
    # Update cover and re-compute cover size (same method)
    current_tree_cover <- current_tree_cover | (shifted > 0)
    current_tree_cover <- current_tree_cover * ped_area
    
    tree_cover_size <- terra::global(cell_area_template * current_tree_cover, "sum", na.rm = TRUE)[1, 1] |>
      tidyr::replace_na(0)
    
    # Update canopy heights (same method)
    if (terra::hasValues(shifted)) {
      canopy_height_gridcell <- max(canopy_height_gridcell, shifted, na.rm = TRUE)
    }
    
    
  }
  
  # bind new points once
  if (length(new_pts_list) == 0) {
    return(NULL)
  }
  new_tree_pts <- sf::st_sf(
    height = new_h_list,
    type   = new_type_list,
    geometry = sf::st_sfc(new_pts_list, crs = sf::st_crs(gridcell))
  )
  
  
  # ---- Update tree canopy for intersecting buffered tiles (don’t mutate canopy_height_gridcell in-place) ----
  update_paths <- glue::glue(
    "{aws_http}/{scenario_folder}/{buffered_tile_names}/raster_files/tree_canopy.tif"
  )
  
  for (p in update_paths) {
    r <- terra::rast_retry(p)
    
    canopy_aligned <- canopy_height_gridcell |>
      terra::extend(r) |>
      terra::crop(r)
    
    updated_canopy <- max(r, canopy_aligned, na.rm = TRUE)
    
    updated_p <- stringr::str_replace(
      p,
      "https://wri-cities-tcm.s3.us-east-1.amazonaws.com",
      "wri-cities-tcm"
    )
    write_s3(updated_canopy, updated_p)
  }
  
  for (t in buffered_tile_names){
    
    tile <- buffered_tile_grid %>% 
      filter(tile_name == t)
    
    pts <- new_tree_pts %>% 
      st_filter(tile)
    
    # Skip if nothing to add for this tile (optional)
    if (is.null(pts) || nrow(pts) == 0) next
    
    path <- glue(
      "{aws_http}/{scenario_folder}/{t}/ccl_layers/new-tree-points__trees__{scenario}.geojson"
    )
    
    # Check existence using your existing helper (returns NULL on missing)
    existing <- tryCatch(st_read(path, quiet = TRUE), error = function(e) NULL)
    
    combined <- if (is.null(existing) || nrow(existing) == 0) {
      pts
    } else {
      rbind(existing, pts)
    }
    
    write_s3(combined, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/new-tree-points__trees__{scenario}.geojson"))
  }
  
  new_tree_pts
}


# Create city-wide grid ---------------------------------------------------

run_tree_scenario <- function(min_dist = 5) {
  
  # Make required objects visible to functions that were written expecting globals.
  list2env(
    list(
      city = city,
      tiles_aoi = tiles_aoi,
      tiles_s3 = tiles_s3,
      bucket = bucket,
      aws_http = aws_http,
      baseline_folder = baseline_folder,
      scenario_folder = scenario_folder,
      city_folder = city_folder,
      aoi = aoi,
      open_urban_aws_http = open_urban_aws_http
    ),
    envir = .GlobalEnv
  )
  
  
  map(tiles_s3, baseline_processing)
  create_tree_population(tiles_s3)
  
  # Achievable potential
  
  tree_grid_path <- glue("{open_urban_aws_http}/scenarios/street-trees/{city}-street-tree-pct-1km-grid.csv")
  
  # Get percentile value
  ped_area_tree_dist <- read_csv(tree_grid_path)
  target_coverage <- quantile(ped_area_tree_dist$`pct-tree`, 0.9, 
                              names = FALSE, na.rm = TRUE)
  
  # Create a grid over the tile geometry to iterate over for creating trees
  # intersect with AOI so only areas within the aoi are planted
  
  aoi_grid <- aoi %>% 
    st_make_grid(cellsize = c(100, 100), square = TRUE, what = "polygons") %>% 
    st_sf() %>% 
    st_intersection(aoi) %>% 
    st_filter(tile_grid) %>% 
    select(geometry) %>% 
    mutate(ID = row_number())
  
  # Copy existing tree canopy tiles into scenario folder to be updated
  tree_canopy_paths <- glue("{baseline_folder}/{tiles_s3}/raster_files/cif_tree_canopy.tif")
  scenario_tree_canopy_paths <- glue("{scenario_folder}/{tiles_s3}/raster_files/tree_canopy.tif")
  
  map(glue("{scenario_folder}/{tiles_s3}/raster_files/"), ~ ensure_s3_prefix(bucket, .x))
  s3_copy_vec(from = tree_canopy_paths, to = scenario_tree_canopy_paths, 
              from_bucket = bucket, to_bucket = bucket, overwrite = TRUE)
  
  # Load tree population data
  glue("{bucket}/{city_folder}/scenarios/trees/tree-population/trees.geojson")
  trees <- st_read(glue("{aws_http}/{city_folder}/scenarios/trees/tree-population/trees.geojson"))
  crowns <- st_read(glue("{aws_http}/{city_folder}/scenarios/trees/tree-population/crowns.geojson"))
  tree_structure <- read_csv(glue("{aws_http}/{city_folder}/scenarios/trees/tree-population/tree-structure.csv"))

  updated_trees <- map(aoi_grid$ID, 
                   ~ plant_in_gridcell_fast(.x, aoi_grid, 
                                       target_coverage = target_coverage, 
                                       min_dist = min_dist,
                                       trees, crowns, tree_structure)) %>% 
    compact() %>% 
    bind_rows() %>% 
    select(height, type)
  
  # Write new tree points
  write_s3(updated_trees, glue("{bucket}/{scenario_folder}/new-tree-points__trees__{scenario}.geojson"))
  
  # Copy final canopy to scenario folder
  for (t in tiles_aoi){
    # updated tree cover
    updated_tree <- rast_retry(glue("{aws_http}/{scenario_folder}/{t}/raster_files/tree_canopy.tif"))
    updated_tree_cover <- updated_tree >= 3
    write_s3(updated_tree_cover, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/tree-cover__trees__pedestrian-achievable-90pctl.tif"))
    
    # new tree cover
    existing_tree <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/tree-cover__baseline__baseline.tif"))
    new_tree <- updated_tree_cover - existing_tree
    write_s3(new_tree, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/new-tree-cover__trees__pedestrian-achievable-90pctl.tif"))
  }
}
