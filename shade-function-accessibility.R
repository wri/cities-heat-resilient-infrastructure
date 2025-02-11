# Load necessary packages
library(sf)
library(terra)
library(spatstat.geom)  # For handling spatial point patterns
library(dplyr)

# Step 1: Define parameters
resolution <- 1          # 1-meter resolution for final raster
square_size <- 5         # 5x5 meter squares
distance_threshold <- 100  # Minimum distance between squares
area_threshold <- 25 # Minimum area of shade, continguous
coverage_threshold <- 0.25  # 25% of each zone

# Step 2: Load a binary raster (where TRUE areas are unshaded)
binary_raster <- shaded 
bin2 <- project(shaded, "epsg:4326")
# zone <- st_transform(zone, 4326)

# All parks at least some shade

# Small parks -------------------------------------------------------------

# Pocket parks—1 acre (~ 0.4 hectares, 4046.86 m^2) or less
# 25% shade
small_parks <- park_vectors %>% 
  filter(area_sqm <= 4046.86)




# Large parks -------------------------------------------------------------

# Large parks—greater than 1 acre
# max distance to shade 100-m
large_parks <- park_vectors %>% 
  filter(area_sqm > 4046.86)

# Function to generate the 5x5 meter squares only in valid raster areas
# unshaded and not a pitch
shade_dist_area <- function(zone, binary_raster) {
  
  # Mask the shade raster by the zone boundary 
  zone_raster_mask <- mask(crop(binary_raster, zone), vect(zone)) > 0
  
  # get contiguous shade areas 
  # assess if any are greater than the minimum shade area threshold
  shade_areas <- zone_raster_mask %>% 
    patches(directions = 4, zeroAsNA = TRUE) %>% 
    as.polygons() %>% 
    st_as_sf() %>% 
    mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2")),
           park_id = zone$park_id) %>% 
    filter(area_sqm >= area_threshold)
  
  # If there is no shade, create shade
  if (nrow(shade_areas) == 0){
   
  } else if (nrow(shade_areas) == 0){
    valid_area2 <- tibble(valid_area = NA,
                          park_id = zone$park_id,
                          unshaded_pct = zone$unshaded_pct,
                          need = "Has no shade")

  } else {
    shade_dist <- distance(zone_raster_mask, shade_areas)

    # TPL uses a 10-minute (0.5 mile) walk to measure park accessibility
    # We'll use 5-minute (0.25 = 400 meters)

    valid_area <- shade_dist >= distance_threshold

    valid_area2 <- tibble(valid_area = sum(values(valid_area), na.rm = TRUE),
                          park_id = zone$park_id,
                          unshaded_pct = zone$unshaded_pct,
                          need = "Has some shade")
  }
  
}

# vd <- c()
vd <- st_sf(geometry = st_sfc(), crs = utm)
for (i in 1:nrow(zones)) {
  print(i)
  # zone <- slice(zones, i) %>% 
  #   st_transform(4326)
  zone <- slice(zones, i)
  
  a <- shade_dist_area(zone, binary_raster)
  
  if (!is.null(a)) {
    vd <- bind_rows(vd, a)
  }
}




tictoc::toc()
# Step 7: Rasterize the result with 1-meter resolution
raster_extent <- st_bbox(zones)  # Use the bounding box of all zones for raster extent
raster_template <- rast(extent = raster_extent, resolution = c(resolution, resolution), crs = st_crs(zones)$proj4string)

# Rasterize the 5x5 meter squares
rasterized <- terra::rasterize(vect(squares_per_zone), raster_template, field = 1, background = 0)

# Step 8: Plot the result
plot(rasterized)
