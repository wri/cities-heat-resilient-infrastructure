calc_cool_roofs_metrics <- function(city, scenario, cool_roof_albedo){

  library(terra)
  library(tidyverse)
  library(sf)
  library(here)
  
  scenario_path <- here("data", city, "scenarios", "cool-roofs", scenario)
  baseline_path <- here("data", city, "scenarios", "baseline")
  
  baseline_albedo <- rast(here(scenario_path, "albedo_baseline.tif"))
  scenario_albedo <- rast(here(scenario_path, 'albedo_cool_roofs_achievable.tif')) 
  
  # Load AOI
  aoi <- st_read(here("data", city, "boundaries.geojson"))
  
  # Load buildings
  build_vectors <- st_read(here(scenario_path, "buildings_polygons.geojson"))
  updated_builds <- st_read(here(scenario_path, "updated-buildings.geojson"))
  
  # Achievable albedo
  alb_target <- quantile(build_vectors$mean_albedo, 0.9)
  roof_raster <- rast(here(scenario_path, "buildings_areas.tif")) %>% 
    subst(0, NA)
  
  # Update the albedo value of targeted roofs
  achievable_albedo <- mask(baseline_albedo, roof_raster, updatevalue = alb_target, inverse = TRUE) %>% 
    crop(albedo)
  
  baseline_roof_alb <- mean(values(mask(baseline_albedo, roof_raster)), na.rm = TRUE)
  scenario_roof_alb <- mean(values(mask(scenario_albedo, roof_raster)), na.rm = TRUE)
  
  timestamps <- c("1200", "1500", "1800")
  
  baseline_Ta <- read_delim(here(baseline_path, "met_era5_hottest_days.txt"))
  scenario_Ta <- read_delim(here(scenario_path, "met_era5_hottest_days.txt"))
  
  # Initialize results list
  results <- tibble(
    "baseline_cool_roof_area" = sum(build_vectors %>% filter(mean_albedo >= cool_roof_albedo) %>% pull(area_sqm)),
    "scenario_cool_roof_area" = sum(updated_builds %>% pull(area_sqm)),
    "achievable_cool_roof_area" = sum(build_vectors %>% pull(area_sqm)),
    "change_cool_roof_area" = scenario_cool_roof_area - baseline_cool_roof_area,
    
    "achievable_cool_roof_reflectivity" = alb_target,
  
    "baseline_reflectivity" = mean(values(baseline_albedo), na.rm = TRUE) * 100,
    "scenario_reflectivity" = mean(values(scenario_albedo), na.rm = TRUE) * 100,
    "achievable_reflectivity" = mean(values(achievable_albedo), na.rm = TRUE) * 100,
    "change_reflectivity" = (scenario_reflectivity - baseline_reflectivity),
    
    "baseline_roof_reflectivity" = baseline_roof_alb * 100,
    "scenario_roof_reflectivity" = scenario_roof_alb * 100,
    "change_roof_reflectivity" = (scenario_roof_alb - baseline_roof_alb),
    
    "progress_cool_roofs" = (baseline_cool_roof_area + change_cool_roof_area) / achievable_cool_roof_area * 100,
    
    "baseline_mean_air_temp_1200" = (baseline_Ta %>% filter(it == 12) %>% pull(Tair)),
    "baseline_mean_air_temp_1500" = (baseline_Ta %>% filter(it == 15) %>% pull(Tair)),
    "baseline_mean_air_temp_1800" = (baseline_Ta %>% filter(it == 18) %>% pull(Tair)),
    "scenario_mean_air_temp_1200" = (scenario_Ta %>% filter(it == 12) %>% pull(Tair)),
    "scenario_mean_air_temp_1500" = (scenario_Ta %>% filter(it == 15) %>% pull(Tair)),
    "scenario_mean_air_temp_1800" = (scenario_Ta %>% filter(it == 18) %>% pull(Tair))
    ) %>% 
    pivot_longer(cols = everything(), names_to = "indicator_id")
  
  write_csv(results, here(scenario_path, "scenario-metrics.csv"))

}
