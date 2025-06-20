large_buildings_scenario_function <- function(scenario_name, infrastructure_path,
                                              area_threshold, cool_roof_albedo,
                                              aoi, lulc, albedo, build_vectors){
  
  scenario_path <- here("data", city, "scenarios", "cool-roofs", scenario_name)
  # Create directory
  dir.create(scenario_path, showWarnings = FALSE)
  
  # Resample albedo to 1-m
  albedo <- terra::resample(albedo, lulc, method = "bilinear") 
  
  writeRaster(albedo, here(scenario_path, "albedo_baseline.tif"), overwrite = TRUE)
  # albedo <- rast(here(scenario_path, "albedo_baseline.tif"))
  
  # Reclassify buildings only
  roofs <- (lulc >= 600) & (lulc < 700)
  
  # Load building polygons and filter to bbox of AOI
  build_vectors <- build_vectors %>% 
    st_transform(st_crs(lulc)) %>% 
    st_filter(aoi, .predicate = st_within) %>% 
    mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2")))
  # build_vectors <- st_read(here(scenario_path, "buildings_polygons.geojson"))
  # Calculate mean albedo of roofs
  mean_albedo <- exactextractr::exact_extract(albedo, build_vectors, 'median')
  
  # Attach to roof polygons
  build_vectors$mean_albedo <- mean_albedo
  
  st_write(build_vectors, here(scenario_path, "buildings_polygons.geojson"),
           delete_dsn = TRUE, append = FALSE)
  
  # Building area raster
  build_raster <- rasterize(build_vectors, lulc)
  build_raster <- subst(build_raster, NA, 0)
  
  writeRaster(build_raster, here(scenario_path, "buildings_areas.tif"), overwrite = TRUE)
  
  # Filter to area threshold
  large_roofs <- build_vectors %>% 
    filter(area_sqm >= area_threshold)
  
  if (nrow(large_roofs) == 0) {
    stop("No large buildings found in the AOI.")
  }
  
  # Filter out existing cool roofs
  large_roofs <- large_roofs %>% 
    filter(mean_albedo < cool_roof_albedo)
  
  st_write(large_roofs, here(scenario_path, paste0("updated-buildings", ".geojson")),
           delete_dsn = TRUE, append = FALSE)
  
  # Rasterize roofs
  large_roof_raster <- rasterize(large_roofs, lulc, field = 1, background = NA)
  
  # Update the albedo value of targeted roofs
  updated_albedo <- mask(albedo, large_roof_raster, updatevalue = cool_roof_albedo, inverse = TRUE) %>% 
    crop(albedo)
  writeRaster(updated_albedo, here(scenario_path, "albedo_cool_roofs_achievable.tif"), overwrite = TRUE)
  
  # Calculate albedo delta
  albedo_delta_bounds <- mean(values(updated_albedo), na.rm = TRUE) - mean(values(albedo), na.rm = TRUE)
  albedo_delta_aoi <- mean(values(mask(updated_albedo, aoi)), na.rm = TRUE) - mean(values(mask(albedo, aoi)), na.rm = TRUE)

  return(albedo_delta_bounds)
}