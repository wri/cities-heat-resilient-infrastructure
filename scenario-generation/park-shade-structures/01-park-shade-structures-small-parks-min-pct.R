park_shade_scenario <- function(city, scenario_name, structure_size, shade_pct, spacing,
                                min_shade_area){
  
  
  
  library(sf)
  library(tidyverse)
  library(terra)
  library(exactextractr)
  
  inputs_path <- here("data", city)
  infrastructure_path <- here(inputs_path, "scenarios", "park-shade-structures")
  
  # Create infrastructure_path
  if (!dir.exists(infrastructure_path)) {
    dir.create(infrastructure_path, recursive = TRUE)
  }
  
  
  # Load input data
  aoi <- st_read(here(inputs_path, "aoi.geojson"))
  utm_epsg <- st_crs(aoi)
  
  lulc <- rast(here(inputs_path, "open-urban.tif"))
  park_vectors <- st_read(here(inputs_path, "openspaces.geojson")) %>% 
    st_transform(utm_epsg) %>% 
    st_filter(aoi)
  
  
  # Find the file
  shadow_file <- list.files(
    path = here("data", city, "scenarios", "baseline"),
    pattern = "^Shadow.*1200D\\.tif$",
    full.names = TRUE
  )
  
  # Read the file (if it exists)
  if (length(shadow_file) > 0) {
    shadow_12pm <- rast(shadow_file)  
  } else {
    warning("No matching file found.")
  }
  
  
  # Park shade scenario
  # Parks 1 acre or smaller, pocket parks, get at least 25% shade
  # Large parks have shade within a 100-m walk
  # Pitches are excluded from available area for shade structures
  
  
  
  # Existing shade ----------------------------------------------------------
  
  # Combine tree shade and building shade
  shaded <- shadow_12pm < 1
  
  writeRaster(shaded, here(infrastructure_path, "shade_1200.tif"), overwrite = TRUE)
  
  # Created unshaded raster
  unshaded <- isFALSE(shaded) 
  
  
  # Parks -------------------------------------------------------------------
  
  
  # Parks vectors, contiguous areas dissolved
  open_spaces <- park_vectors %>% 
    filter(leisure != "pitch") %>% 
    # dissolve
    st_union() %>% 
    st_sf() %>% 
    # break apart
    st_cast("POLYGON") %>% 
    # cropping to AOI before calculating area
    st_intersection(aoi) %>% 
    mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2")),
           park_id = row_number())
  
  if (nrow(open_spaces) == 0) {
    stop("No parks found in the AOI.")
  }
  
  
  ## zonal statistics per park shaded/unshaded ####
  open_spaces <- open_spaces %>% 
    mutate(shaded_pct = exact_extract(shaded, geometry, "mean"),
           shaded_area = shaded_pct * area_sqm,
           unshaded_pct = 1 - shaded_pct,
           unshaded_area = unshaded_pct * area_sqm)
  
  # Get sports fields
  pitch_vectors <- park_vectors %>% 
    filter(leisure == "pitch") %>%  
    st_union() %>% 
    st_sf()
  
  # Parks with sport fields erased
  if (nrow(pitch_vectors) > 0){
    park_suitable_area_vector <- open_spaces %>% 
      # Erase sports fields
      st_difference(pitch_vectors) %>% 
      mutate(area_sqm_suitable = as.numeric(units::set_units(st_area(geometry), "m^2")))
  } else {
    park_suitable_area_vector <- open_spaces
  }
  
  st_write(park_suitable_area_vector, here(infrastructure_path, "parks.geojson"),
           append = FALSE, delete_dsn = TRUE)
  
  # Small parks -------------------------------------------------------------
  
  # https://srpshade.caddetails.com/products/square-hip-shades-4430/80366
  
  source(here("scenario-generation", "park-shade-structures", "shade-generating-functions.R"))
  
  # Pocket parks—1 acre (~ 0.4 hectares, 4046.86 m^2) or less
  # 25% shade
  small_parks <- park_suitable_area_vector %>% 
    filter(area_sqm <= 4046.86, 
           shaded_pct < 0.25) 
  
  # Create empty shade structure geometry
  shade_structures_all <- st_sf(geometry = st_sfc(), crs = utm_epsg)
  
  if (nrow(small_parks) > 0) {
    for (i in 1:nrow(small_parks)) {
      print(i)
      park <- slice(small_parks, i)
      shade_structures <- generate_squares_in_valid_area(
        park = park, 
        unshaded_raster = unshaded, 
        structure_size = structure_size, 
        shade_pct = shade_pct, 
        spacing = spacing
      )
      
      if (!is.null(shade_structures)) {
        shade_structures_all <- bind_rows(shade_structures, shade_structures_all)
      }
    }
  } else {
    message("No small parks needing shade.")
  }
  
  
  # Write file -------------------------------------------
  
  scenario_path <- here(infrastructure_path, scenario_name)
  dir.create(scenario_path, showWarnings = FALSE)
  
  st_write(shade_structures_all, here(scenario_path, "shade_structures.geojson"))
  
  
  # Create height raster ----------------------------------------------------
  
  # https://srpshade.caddetails.com/products/square-hip-shades-4430/80366
  # 8-ft height for shade structures
  structure_height <- 8 / 3.281 # convert to meters
  
  shade_structures_all <- shade_structures_all %>% 
    mutate(height = structure_height)
  
  cif_lulc <- rast(here(inputs_path, "cif_lulc.tif"))
  
  shade_structures_rast <- shade_structures_all %>% 
    rasterize(cif_lulc, field = "height", background = 0) 
  
  writeRaster(shade_structures_rast, here(scenario_path, "structures-as-trees.tif"))
  
  
}

