library(here)
library(terra)
library(sf)
library(tidyverse)
library(lidR)
library(RANN)
library(glue)
library(geoarrow)
library(sfarrow)


# # Specify variables -------------------------------------------------------
# 
# city <- "ZAF-Durban"
# aoi_name <- "inner_city_lap"
# 
# infra_name <- "trees"
# scenario_name <- "pedestrian-achievable-90pctl"
# 
# # Functions 
# 
# source(here("tiling-scripts", "utils.R"))

# # Sign in to AWS ----------------------------------------------------------
# 
# system("aws sso login --profile cities-data-dev")
# Sys.setenv(AWS_PROFILE = "cities-data-dev",
#            AWS_DEFAULT_REGION = "us-east-1",
#            AWS_SDK_LOAD_CONFIG = "1")
# 
# s3 <- paws::s3()
# bucket <- "wri-cities-tcm"
# aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"

# # Process baseline tile data ----------------------------------------------
# city_folder <- glue("city_projects/{city}/{aoi_name}")
# 
# open_urban_aws_http <- glue("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}")
# 
# baseline_name <- "baseline"
# 
# baseline_folder <- glue("{city_folder}/scenarios/baseline/{baseline_name}")
# scenario_folder <- glue("{city_folder}/scenarios/{infra_name}/{scenario_name}")
# 
# # Make scenario folder
# ensure_s3_prefix(bucket, scenario_folder)
# 
# # Get tile ids
# t <- s3$list_objects_v2(
#   Bucket = bucket,
#   Prefix = baseline_folder
# )
# 
# tiles <- list_tiles(bucket, baseline_folder)
# 
# if (aoi_name == "urban_extent"){
#   aoi <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/urban_extent_boundary.geojson"))
# } else {
#   aoi <- st_read(aoi_path)
# }
# 
# buffered_tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/tile_grid.geojson"))
# tile_grid <- st_read(glue("{aws_http}/{baseline_folder}/metadata/.qgis_data/unbuffered_tile_grid.geojson"))



# Make binary tree cover --------------------------------------------------

make_binary_tree_cover <- function(tree_canopy){
  binary_tree <- tree_canopy >= 3
  return(binary_tree)
}

# Process tree canopy to trees --------------------------------------------

process_trees <- function(tree_canopy, city_folder, baseline_folder, t_id){
  
  tile <- tile_grid %>% 
    filter(tile_name == t_id)
  
  # Load raster into memory
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
    mutate(tile = t_id) %>% 
    st_filter(tile_grid, .predicate = st_within)
  
  ttops <- ttops %>% 
    rename(height = Z) %>% 
    st_zm(drop = TRUE, what = "ZM")
  
  write_s3(ttops, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/tree-points.geojson"))
  write_s3(crowns, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/existing-tree-crowns.tif"))
  write_s3(crown_vectors, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/existing-tree-crowns.geojson"))
}


# Create pedestrian area --------------------------------------------------

create_pedestrian_area <- function(lulc, open_urban_aws_http){
  
  utm <- st_crs(lulc)
  
  # tile bounds
  tile_geom <- st_as_sf(as.polygons(ext(lulc)))
  st_crs(tile_geom) <- utm
  
  # Load roads and filter to tile geometry
  road_vectors <- sfarrow::st_read_parquet(
    glue("{open_urban_aws_http}/roads/roads_all.parquet"),
    wkt_filter = st_as_text(tile_geom),   
    quiet = TRUE) %>% 
    st_transform(utm) %>% 
    st_filter(tile_geom)
  
  # Subset to low-traffic volume roads
  lanes <- read_csv(glue("{open_urban_aws_http}/roads/average_lanes.csv"))
  
  ped_roads_list <- c("tertiary",
                      "tertiary_link",
                      "residential",
                      "living_street")
  
  road_vectors <- road_vectors %>% 
    select(highway, lanes) %>% 
    mutate(lanes = as.integer(lanes)) %>% 
    left_join(lanes, by = "highway") %>% 
    mutate(lanes = coalesce(lanes, avg_lanes))
  
  ped_road_vectors <- road_vectors %>% 
    filter(highway %in% ped_roads_list)
  
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
  
  ped_roads_buff <- ped_road_vectors %>% 
    st_buffer(dist = ped_road_vectors$lanes * (width / 2),
              endCapStyle = "FLAT",
              joinStyle = "MITRE") 
  
  ped_roads_area <- rasterize(ped_roads_buff, lulc, field = 1, background = 0)
  
  # 5-meter buffer 
  pedestrian_area <- ped_roads_area %>% 
    subst(0, NA) %>% 
    buffer(5) %>% 
    as.numeric()
  
  # Remove roads and water
  pedestrian_area <- pedestrian_area * abs((floor(lulc / 100) %in% c(3, 5)) - 1)
  return(pedestrian_area)
}


# Create plantable area ---------------------------------------------------

create_plantable_area <- function(lulc, pedestrian_area, binary_tree_cover, open_urban_aws_http){
  
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
  road_vectors <- st_read_parquet(glue("{open_urban_aws_http}/roads/roads_all.parquet")) %>% 
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
  
  # Remove areas of existing tree cover
  plantable_street <- plantable_street * (binary_tree_cover < 1)
  return(plantable_street)
}


# Baseline tree processing per tile ---------------------------------------

# baseline folder is the output of the baseline CTCM run
baseline_processing <- function(t_id){
  print(t_id)
  
  ensure_s3_prefix(
    bucket,
    glue("{baseline_folder}/{t_id}/ccl_layers/")
  )

  # Load data
  tree_canopy <- rast(glue("{aws_http}/{baseline_folder}/{t_id}/raster_files/cif_tree_canopy.tif"))
  lulc <- rast(glue("{aws_http}/{baseline_folder}/{t_id}/raster_files/cif_open_urban.tif"))

  # Create and save binary tree cover
  binary_tree_cover <- make_binary_tree_cover(tree_canopy)
  write_s3(binary_tree_cover, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/tree-cover.tif"))

  process_trees(tree_canopy, city_folder, baseline_folder, t_id)

  # Create pedestrian area
  pedestrian_area <- create_pedestrian_area(lulc, open_urban_aws_http)
  write_s3(pedestrian_area, glue("{bucket}/{baseline_folder}/{t_id}/ccl_layers/pedestrian-areas.tif"))

  # Create plantable area
  plantable_area <- create_plantable_area(lulc, pedestrian_area, binary_tree_cover, open_urban_aws_http)

  # dir.create(here(scenario_folder, tile), showWarnings = FALSE, recursive = TRUE)
  ensure_s3_prefix(
    bucket,
    glue("{scenario_folder}/{t_id}/ccl_layers/")
  )
  write_s3(plantable_area, glue("{bucket}/{scenario_folder}/{t_id}/ccl_layers/plantable-areas.tif"))

  # Create available points to place trees
  available_pts <- as.points(subst(plantable_area, from = 0, to = NA)) %>%
    st_as_sf() %>%
    mutate(pt_id = row_number())

  write_s3(available_pts, glue("{bucket}/{scenario_folder}/{t_id}/ccl_layers/plantable-points.geojson"))
}


# Create city-wide tree population ----------------------------------------

create_tree_population <- function(tiles){
  
  tree_paths <- glue("{aws_http}/{baseline_folder}/{tiles}/ccl_layers/tree-points.geojson")
  
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
  crown_paths <- paste(aws_http, baseline_folder, tiles, "ccl_layers", "existing-tree-crowns.geojson", sep = "/")
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
  
  ensure_s3_prefix(bucket, glue("{bucket}/{scenario_folder}/tree-population"))
  
  write_s3(trees, glue("{bucket}/{scenario_folder}/tree-population/trees.geojson"))
  write_s3(crowns, glue("{bucket}/{scenario_folder}/tree-population/crowns.geojson"))
  write_s3(tree_structure, glue("{bucket}/{scenario_folder}/tree-population/tree-structure.csv"))
}


# Plant new trees ---------------------------------------------------------


# Gridcell is grid geometry
plant_in_gridcell <- function(grid_index, target_coverage, min_dist){
  
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
  
  new_tree_pts <- new_trees[0,]
  
  # Existing tree canopy
  canopy_paths <- glue("{aws_http}/{baseline_folder}/{unbuffered_tile_names}/raster_files/cif_tree_canopy.tif")
  canopy_height_existing <- load_and_merge(canopy_paths) 
  canopy_height_gridcell <- canopy_height_existing %>%
    crop(gridcell)
  
  # Plantable area for the gridcell
  plantable_paths <- glue("{aws_http}/{scenario_folder}/{unbuffered_tile_names}/ccl_layers/plantable-areas.tif")
  plantable_area <- load_and_merge(plantable_paths) %>% 
    crop(gridcell) %>% 
    subst(from = 0, to = NA)
  
  # Total plantable area for the gridcell, if NA make 0
  plantable_area_size <- global(cellSize(plantable_area) * plantable_area, "sum", na.rm = TRUE)[1,1] %>%
    replace_na(0)
  
  # Pedestrian area for the gridcell
  ped_area_paths <- glue("{aws_http}/{baseline_folder}/{unbuffered_tile_names}/ccl_layers/pedestrian-areas.tif")
  ped_area <- load_and_merge(ped_area_paths) %>% 
    crop(gridcell) %>% 
    subst(from = 0, to = NA)
  
  # Total pedestrian area for the gridcell, if NA make 0
  ped_area_size <- global(cellSize(ped_area) * ped_area, "sum", na.rm = TRUE)[1,1] %>%
    replace_na(0)
  
  # Current tree cover
  current_tree_cover_paths <- glue("{aws_http}/{baseline_folder}/{unbuffered_tile_names}/ccl_layers/tree-cover.tif")
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
    crown_pixels <- rast(glue("{aws_http}/{baseline_folder}/{crown_geom$tile}/ccl_layers/existing-tree-crowns.tif")) %>% 
      crop(crown_geom)
    crown_pixels <- crown_pixels == crown_geom$treeID
    crown_pixels <- subst(crown_pixels, from = 0, to = NA)
    
    height_pixels <- rast(glue("{aws_http}/{baseline_folder}/{crown_geom$tile}/raster_files/cif_tree_canopy.tif")) %>% 
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
  
  # Add new tree points to layer
  new_trees <- rbind(new_trees, new_tree_pts)
  
  # Update tree canopy for all intersecting buffered tree canopy layers
  update_paths <- glue("{aws_http}/{scenario_folder}/{buffered_tile_names}/raster_files/tree_canopy.tif")
  for (p in update_paths) {
    r <- rast(p)
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

# Create city-wide grid ---------------------------------------------------

map(tiles, baseline_processing)
create_tree_population(tiles)

# Achievable potential
tree_grid_path <- glue("{open_urban_aws_http}/scenarios/street-trees/{city}-street-tree-pct-1km-grid.csv")

# Get percentile value
ped_area_tree_dist <- read_csv(tree_grid_path)
target_coverage <- quantile(ped_area_tree_dist$`pct-tree`, 0.9, 
                            names = FALSE, na.rm = TRUE)
min_dist <- 5

# Create a grid over the tile geometery to iterate over for creating trees
# intersect with AOI so only areas within the aoi are planted
aoi <- st_read(aoi_path)

aoi_grid <- aoi %>% 
  st_make_grid(cellsize = c(100, 100), square = TRUE, what = "polygons") %>% 
  st_sf() %>% 
  st_intersection(aoi) %>% 
  select(geometry) %>% 
  mutate(ID = row_number())

# Copy existing tree canopy tiles into scenario folder to be updated
tree_canopy_paths <- glue("{baseline_folder}/{tiles}/raster_files/cif_tree_canopy.tif")
scenario_tree_canopy_paths <- glue("{scenario_folder}/{tiles}/raster_files/tree_canopy.tif")

map(glue("{scenario_folder}/{tiles}/raster_files/"), ~ ensure_s3_prefix(bucket, .x))
s3_copy_vec(from = tree_canopy_paths, to = scenario_tree_canopy_paths, bucket, overwrite = TRUE)

# Load tree population data
trees <- st_read(glue("{aws_http}/{scenario_folder}/tree-population/trees.geojson"))
crowns <- st_read(glue("{aws_http}/{scenario_folder}/tree-population/crowns.geojson"))
tree_structure <- read_csv(glue("{aws_http}/{scenario_folder}/tree-population/tree-structure.csv"))

# Create an empty sf to store new trees
new_trees <- st_sf(
  height = numeric(0),
  type   = character(0),
  geometry = st_sfc(crs = st_crs(aoi))
)

new_trees <- map(aoi_grid$ID, 
                 ~ plant_in_gridcell(.x, target_coverage = target_coverage, min_dist = min_dist)) %>% 
  compact() %>% 
  bind_rows() %>% 
  select(height, type)

# Write new tree points
write_s3(new_trees, glue("{bucket}/{scenario_folder}/new-tree-points.geojson"))


