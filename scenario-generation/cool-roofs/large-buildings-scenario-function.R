large_buildings_scenario_function <- function(scenario_name, infrastructure_path,
                                              area_threshold, cool_roof_albedo,
                                              aoi, lulc, albedo, build_vectors){
  # Create directory
  dir.create(here(infrastructure_path, scenario_name), showWarnings = FALSE)
  
  # Resample albedo to 1-m
  albedo <- albedo %>% 
    resample(lulc)
  
  # Reclassify buildings only
  roofs <- (lulc >= 600) & (lulc < 700)
  
  # Load building polygons and filter to bbox of AOI
  build_vectors <- build_vectors %>% 
    st_transform(st_crs(lulc)) %>% 
    st_filter(aoi, .predicate = st_within) %>% 
    mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2")))
  
  # Filter to area threshold
  large_roofs <- build_vectors %>% 
    filter(area_sqm >= area_threshold)
  
  if (nrow(large_roofs) == 0) {
    stop("No large buildings found in the AOI.")
  }
  
  # Calculate mean albedo of roofs
  mean_albedo <- exactextractr::exact_extract(albedo, large_roofs, 'mean')
  
  # Attach to roof polygons
  large_roofs$mean_albedo <- mean_albedo
  
  # Filter out existing cool roofs
  large_roofs <- large_roofs %>% 
    filter(mean_albedo < cool_roof_albedo)
  
  st_write(large_roofs, here(infrastructure_path, scenario_name, paste0(scenario_name, ".geojson")))
  
  # Rasterize roofs
  large_roof_raster <- rasterize(large_roofs, lulc, field = 1, background = NA)
  
  # Update the albedo value of targeted roofs
  updated_albedo <- mask(albedo, large_roof_raster, updatevalue = cool_roof_albedo, inverse = TRUE) %>% 
    crop(albedo)
  writeRaster(updated_albedo, here(infrastructure_path, scenario_name, "updated-albedo.tif"))
  
  # Calculate albedo delta
  albedo_delta_bounds <- mean(values(updated_albedo), na.rm = TRUE) - mean(values(albedo), na.rm = TRUE)
  albedo_delta_aoi <- mean(values(mask(updated_albedo, aoi)), na.rm = TRUE) - mean(values(mask(albedo, aoi)), na.rm = TRUE)

  return(albedo_delta_bounds)
}