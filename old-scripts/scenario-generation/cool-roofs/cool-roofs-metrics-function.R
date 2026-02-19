calc_cool_roofs_metrics <- function(city, city_folder, scenario, cool_roof_albedo, aoi_name){

  library(terra)
  library(tidyverse)
  library(sf)
  library(here)
  
  scenario_path <- here("data", city_folder, "scenarios", "cool-roofs", scenario)
  baseline_path <- here("data", city_folder, "scenarios", "baseline")
  
  baseline_albedo <- rast(here(baseline_path, "albedo_baseline.tif"))
  scenario_albedo <- rast(here(scenario_path, 'albedo_cool_roofs_achievable.tif')) 
  
  # Load AOI
  aoi <- st_read(here("data", city_folder, "boundaries.geojson"))
  
  # Load buildings
  build_vectors <- st_read(here(baseline_path, "buildings_polygons.geojson"))
  updated_builds <- st_read(here(scenario_path, "updated-buildings.geojson"))
  
  # Achievable albedo
  alb_target <- quantile(build_vectors$mean_albedo, 0.9)
  roof_raster <- rast(here(baseline_path, "buildings_areas.tif")) %>% 
    subst(0, NA)
  
  # Update the albedo value of targeted roofs
  achievable_albedo <- mask(baseline_albedo, roof_raster, updatevalue = alb_target, inverse = TRUE) 
  
  baseline_roof_alb <- mean(values(mask(baseline_albedo, roof_raster)), na.rm = TRUE)
  scenario_roof_alb <- mean(values(mask(scenario_albedo, roof_raster)), na.rm = TRUE)
  
  timestamps <- c("1200", "1500", "1800")
  
  baseline_Ta <- read_delim(here(baseline_path, "met_era5_hottest_days.txt"))
  scenario_Ta <- read_delim(here(scenario_path, "met_era5_hottest_days.txt"))
  
  date <- baseline_Ta %>% 
    slice(1) %>% 
    mutate(date = paste(`%iy`, id, sep = "_")) %>% 
    pull(date)
  
  # UTCI difference
  utci_metrics <- tibble()
  
  for (time in timestamps) {
    
    scenario_utci_rast <- rast(here(scenario_path, 
                                    paste0("utci_", time, "_cool_roofs_achievable", ".tif")))
    
    # Mask UTCI to AOI
    baseline_utci_rast <- rast(here(baseline_path, paste0("utci_", time, "_baseline.tif"))) 
    scenario_utci_rast <- scenario_utci_rast %>% 
      crop(baseline_utci_rast)
    
    baseline_utci_rast <- baseline_utci_rast %>% 
      mask(aoi)
    scenario_utci_rast <- scenario_utci_rast %>% 
      mask(aoi)
    
    nonbuild_area_rast <- rast(here(baseline_path, "non_buildings_areas.tif")) %>% 
      crop(baseline_utci_rast)
    
    # Compute metrics
    baseline_utci_avg <- mean(values(mask(baseline_utci_rast, nonbuild_area_rast, maskvalues = 0) %>% 
                                       subst(from = 0, to = NA)), na.rm = TRUE)
    scenario_utci_avg <- mean(values(mask(scenario_utci_rast, nonbuild_area_rast, maskvalues = 0) %>% 
                                       subst(from = 0, to = NA)), na.rm = TRUE)
    utci_diff <- scenario_utci_avg - baseline_utci_avg
    
    # Store results
    metrics <- tibble(
      time = time,
      
      mean_utci_baseline_nonbuilding_areas = baseline_utci_avg,
      mean_utci_scenario_nonbuilding_areas = scenario_utci_avg,
      mean_utci_change_nonbuilding_areas = utci_diff
      
    ) %>%
      pivot_longer(
        cols = -c(time),
        names_to = "indicators_id",
        values_to = "value"
      ) %>%
      rowwise() %>% 
      mutate(
        metric = str_extract(indicators_id, "^[^_]+_[^_]+"),
        metric2 = str_replace(indicators_id, metric, ""),
        indicators_id = paste0(metric, "_", time, metric2)
      ) %>%
      select(-time, -metric, -metric2)
    
    
    utci_metrics <- bind_rows(utci_metrics, metrics)
  }
  
  # Initialize results list
  results <- tibble(
    "baseline_cool_roof_area" = sum(build_vectors %>% filter(mean_albedo >= cool_roof_albedo) %>% pull(area_sqm)),
    "scenario_cool_roof_area" = sum(updated_builds %>% pull(area_sqm)),
    "achievable_cool_roof_area" = sum(build_vectors %>% pull(area_sqm)),
    "change_cool_roof_area" = scenario_cool_roof_area - baseline_cool_roof_area,
    
    "achievable_cool_roof_reflectivity" = alb_target,
  
    "baseline_reflectivity" = mean(values(baseline_albedo), na.rm = TRUE),
    "scenario_reflectivity" = mean(values(scenario_albedo), na.rm = TRUE),
    "achievable_reflectivity" = mean(values(achievable_albedo), na.rm = TRUE),
    "change_reflectivity" = (scenario_reflectivity - baseline_reflectivity),
    
    "baseline_roof_reflectivity" = baseline_roof_alb,
    "scenario_roof_reflectivity" = scenario_roof_alb,
    "change_roof_reflectivity" = (scenario_roof_alb - baseline_roof_alb),

    "progress_reflectivity" = scenario_reflectivity / achievable_reflectivity,    
    "progress_cool_roofs" = (baseline_cool_roof_area + change_cool_roof_area) / achievable_cool_roof_area * 100,
    
    "baseline_mean_air_temp_1200" = (baseline_Ta %>% filter(it == 12) %>% pull(Tair)),
    "baseline_mean_air_temp_1500" = (baseline_Ta %>% filter(it == 15) %>% pull(Tair)),
    "baseline_mean_air_temp_1800" = (baseline_Ta %>% filter(it == 18) %>% pull(Tair)),
    "scenario_mean_air_temp_1200" = (scenario_Ta %>% filter(it == 12) %>% pull(Tair)),
    "scenario_mean_air_temp_1500" = (scenario_Ta %>% filter(it == 15) %>% pull(Tair)),
    "scenario_mean_air_temp_1800" = (scenario_Ta %>% filter(it == 18) %>% pull(Tair)),
    "change_mean_air_temp_1200" = scenario_mean_air_temp_1200 - baseline_mean_air_temp_1200,
    "change_mean_air_temp_1500" = scenario_mean_air_temp_1500 - baseline_mean_air_temp_1500,
    "change_mean_air_temp_1800" = scenario_mean_air_temp_1800 - baseline_mean_air_temp_1800
    ) %>% 
    pivot_longer(cols = everything(), names_to = "indicators_id", values_to = "value") %>% 
    bind_rows(utci_metrics) %>% 
    mutate(date = date,
           application_id = "ccl",
           cities_id = city,
           areas_of_interest_id = aoi_name,
           interventions_id = "cool_roofs",
           scenarios_id = paste("cool_roofs", str_replace(scenario, "-", "_"), sep = "_"))
  
  
  
  write_csv(results, here(scenario_path, "scenario-metrics.csv"))

}
