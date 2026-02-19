cool_roofs_scenario_function <- function(scenario_name, infrastructure_path,
                                              area_threshold, cool_roof_albedo,
                                              aoi, lulc, albedo, build_vectors,
                                              city_folder){
  
  baseline_path <- here("data", city_folder, "scenarios", "baseline")
  scenario_path <- here("data", city_folder, "scenarios", "cool-roofs", scenario_name)
  
  # Create directory
  dir.create(scenario_path, showWarnings = FALSE)
  
  # Resample albedo to 1-m
  albedo <- terra::resample(albedo, lulc, method = "bilinear") 
  # albedo <- rast(here(baseline_path, "albedo_baseline.tif"))
  
  writeRaster(albedo, here(baseline_path, "albedo_baseline.tif"), overwrite = TRUE)
  
  # Reclassify buildings only
  # roofs <- (lulc >= 600) & (lulc < 700)
  
  # Load building polygons and filter to bbox of AOI
  build_vectors <- build_vectors %>% 
    st_transform(st_crs(lulc)) %>% 
    # st_filter(aoi, .predicate = st_within) %>% 
    st_filter(aoi) %>% 
    mutate(area_sqm = as.numeric(units::set_units(st_area(geometry), "m^2")))

  # Calculate mean albedo of roofs
  mean_albedo <- exactextractr::exact_extract(albedo, build_vectors, 'median')
  
  # Attach to roof polygons
  build_vectors$mean_albedo <- mean_albedo
  
  st_write(build_vectors, here(baseline_path, "buildings_polygons.geojson"),
           delete_dsn = TRUE, append = FALSE)
  
  # Building area raster
  build_raster <- rasterize(build_vectors, lulc) %>% 
    crop(albedo)
  build_raster <- subst(build_raster, NA, 0)
  
  writeRaster(build_raster, here(baseline_path, "buildings_areas.tif"), overwrite = TRUE)
  
  non_build_raster <- abs(build_raster - 1)
  writeRaster(non_build_raster, here(baseline_path, "non_buildings_areas.tif"), overwrite = TRUE)
  
  # Filter to area threshold
  if (scenario_name == "all-buildings"){
    roofs <- build_vectors
  } else if (scenario_name == "large-buildings"){
    roofs <- build_vectors %>% 
      filter(area_sqm >= area_threshold)
  }
  
  
  if (nrow(roofs) == 0) {
    stop("No large buildings found in the AOI.")
  }
  
  # Filter out existing cool roofs
  roofs <- roofs %>% 
    filter(mean_albedo < cool_roof_albedo)
  
  st_write(roofs, here(scenario_path, paste0("updated-buildings", ".geojson")),
           delete_dsn = TRUE, append = FALSE)
  
  low_slope <- roofs %>% 
    filter(area_sqm > 821,
           mean_albedo <= 0.62) 
  
  high_slope <- roofs %>% 
    filter(area_sqm <= 821,
           mean_albedo <= 0.28)
  
  # Rasterize roofs
  low_roof_raster <- rasterize(low_slope, lulc, field = 1, background = NA) %>% 
    crop(albedo)
  high_roof_raster <- rasterize(high_slope, lulc, field = 1, background = NA) %>% 
    crop(albedo)
  
  # Update the albedo value of targeted roofs
  updated_albedo <- mask(albedo, low_roof_raster, updatevalue = 0.62, inverse = TRUE) 
  updated_albedo <- mask(updated_albedo, high_roof_raster, updatevalue = 0.28, inverse = TRUE) %>% 
    crop(albedo)
    
  writeRaster(updated_albedo, here(scenario_path, "albedo_cool_roofs_achievable.tif"), overwrite = TRUE)
  
  # Albedo difference
  diff_albedo <- updated_albedo - albedo
  writeRaster(diff_albedo, here(scenario_path, "albedo_achievable_vs_baseline.tif"), overwrite = TRUE)
  
  # Calculate albedo delta
  albedo_delta_bounds <- mean(values(updated_albedo), na.rm = TRUE) - mean(values(albedo), na.rm = TRUE)
  albedo_delta_aoi <- mean(values(mask(updated_albedo, aoi)), na.rm = TRUE) - mean(values(mask(albedo, aoi)), na.rm = TRUE)

  return(albedo_delta_bounds)
}