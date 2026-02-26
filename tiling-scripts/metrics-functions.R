library(terra)
library(tidyverse)
library(sf)
library(here)
library(glue)

source(here("tiling-scripts", "utils.R"))


# baseline
calc_baseline_metrics <- function(city, aoi_name, tiles_aoi){
  
  tiles_aoi <- tiles_aoi
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  met_file <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv")
  met_data <- read_csv(met_file, skip = 2) 
  
  date <- met_data %>%
    slice(1) %>%
    mutate(date = glue("{Year}_{Month}_{Day}")) %>%
    pull(date)
  
  aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  
  # Load AOI and clip layers
  aoi_path <- glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson")
  aoi <- st_read(aoi_path) 
  
  # Load pedestrian area raster
  ped_area_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/pedestrian-areas__baseline__baseline.tif")
  ped_area_rast <- load_and_merge(ped_area_paths) 
  
  aoi <- aoi %>%
    st_transform(st_crs(ped_area_rast))
  ped_area_rast <- ped_area_rast %>% 
    mask(aoi)

  pedestrian_area <- sum(values(ped_area_rast) != 0, na.rm = TRUE)
  
  # Load non-building area raster
  nonbuild_area_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
  nonbuild_area_rast <- load_and_merge(nonbuild_area_paths) 
  
  nonbuild_area_rast <- nonbuild_area_rast %>% 
    mask(aoi)
  
  nonbuild_area <- sum(values(nonbuild_area_rast) != 0, na.rm = TRUE)
  
  # Initialize results list
  results <- tibble()
  # infra_file_name <- "baseline"
  
  # scenario_base <- here(scenario_path, scenario)
  timestamps <- c("1200", "1500", "1800")
  
  for (time in timestamps) {
    
    # Load shade raster and mask to AOI
    baseline_shade_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-{time}__baseline__baseline.tif")
    baseline_shade_rast <- load_and_merge(baseline_shade_paths) 
    baseline_shade_rast <- baseline_shade_rast %>% 
      mask(aoi) > 0
    
    # Mask UTCI to AOI
    baseline_utci_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    baseline_utci_rast <- load_and_merge(baseline_utci_paths)
    baseline_utci_rast <- baseline_utci_rast %>% 
      mask(aoi)
    
    ped_area_rast <- ped_area_rast %>% 
      crop(baseline_utci_rast)
    
    nonbuild_area_rast <- nonbuild_area_rast %>% 
      crop(baseline_utci_rast)
    
    # Compute metrics
    mean_utci_baseline_aoi <- mean(values(baseline_utci_rast %>% 
                                                   subst(0, NA)), na.rm = TRUE)
    
    shade_cover_baseline_aoi <- mean(values(baseline_shade_rast), na.rm = TRUE) 
    
    mean_utci_baseline_pedestrian <- mean(values(mask(baseline_utci_rast, ped_area_rast, maskvalues = 0) %>% 
                                       subst(0, NA)), na.rm = TRUE)
    
    shade_cover_baseline_pedestrian <- mean(values(mask(baseline_shade_rast, ped_area_rast, maskvalues = 0), na.rm = TRUE)) 
    
    mean_utci_baseline_nonbuild <- mean(values(mask(baseline_utci_rast, nonbuild_area_rast, maskvalues = 0) %>% 
                                                   subst(0, NA)), na.rm = TRUE)
    
    shade_cover_baseline_nonbuild <- mean(values(mask(baseline_shade_rast, nonbuild_area_rast, maskvalues = 0), na.rm = TRUE)) 
    
    # Store results
    metrics <- tibble(
      mean_utci_baseline_aoi = mean_utci_baseline_aoi,
      shade_cover_baseline_aoi = shade_cover_baseline_aoi * 100,
      mean_utci_baseline_pedestrian = mean_utci_baseline_pedestrian,
      shade_cover_baseline_pedestrian = shade_cover_baseline_pedestrian * 100,
      mean_utci_baseline_nonbuild = mean_utci_baseline_nonbuild,
      shade_cover_baseline_nonbuild = shade_cover_baseline_nonbuild * 100
    ) %>%
      pivot_longer(
        cols = everything(),
        names_to = "indicators_id",
        values_to = "value"
      ) %>%
      rowwise() %>% 
      mutate(
        metric = str_extract(indicators_id, "^[^_]+_[^_]+"),
        metric2 = str_replace(indicators_id, metric, ""),
        indicators_id = paste0(metric, "_", time, metric2)
      ) %>%
      select(-metric, -metric2)
    
    
    results <- bind_rows(results, metrics)
  }
  
  # Load updated trees
  baseline_tree_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/tree-cover__baseline__baseline.tif")
  baseline_tree_rast <- load_and_merge(baseline_tree_paths) %>% 
    subst(from = NA, 0) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  
  # Tree pct
  baseline_tree_pct_aoi <- mean(values(baseline_tree_rast, na.rm = TRUE)) 
  baseline_tree_pct_pedestrian <- mean(values(mask(baseline_tree_rast, ped_area_rast, maskvalues = 0), na.rm = TRUE)) 
   
  # Number of trees
  baseline_tree_points <- map_dfr(tiles_aoi, function(time) {
    
    baseline_utci <- terra::rast(glue(
      "{aws_http}/{baseline_folder}/{time}/ccl_layers/utci-1200__baseline__baseline.tif"
    ))
    
    bbox_sf <- st_as_sf(st_as_sfc(st_bbox(baseline_utci)))
    
    st_read(glue(
      "{aws_http}/{baseline_folder}/{time}/ccl_layers/tree-points__baseline__baseline.geojson"
    ), quiet = TRUE) %>%
      st_filter(bbox_sf) }) %>%
    st_filter(aoi)
  
  
  # Extract raster values at each point
  pedestrian_vals <- terra::extract(ped_area_rast, vect(baseline_tree_points))
  baseline_tree_points$pedestrian <- pedestrian_vals[[2]]  # second column is the raster value
  
  baseline_tree_n_aoi <- nrow(baseline_tree_points)
  baseline_tree_n_pedestrian <- nrow(baseline_tree_points %>% filter(pedestrian == 1))
  
  tree_metrics <- 
    tibble(
      tree_cover_baseline_aoi = baseline_tree_pct_aoi * 100,
      tree_cover_baseline_pedestrian = baseline_tree_pct_pedestrian * 100,
      tree_n_baseline_aoi = baseline_tree_n_aoi,
      tree_n_baseline_pedestrian = baseline_tree_n_pedestrian) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "indicators_id",
      values_to = "value"
    )
  
  results <- bind_rows(results, tree_metrics) %>% 
    mutate(application_id = "ccl",
           cities_id = city,
           areas_of_interest_id = str_replace(aoi_name, "-", "_"),
           interventions_id = "baseline",
           scenarios_id = glue("baseline__baseline"),
           date = date,
           value = round(value, 2))
  
  # Save results
  ensure_s3_prefix("wri-cities-tcm", glue("city_projects/{city}/{aoi_name}/scenarios/metrics"))
  write_s3(results, glue("wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/metrics/metrics__baseline__baseline.csv"))
  
}

# trees
calc_street_tree_metrics <- function(city, aoi_name, tiles_aoi, scenario){
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/trees/{scenario}")
  
  met_file <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv")
  met_data <- read_csv(met_file, skip = 2) 
  
  date <- met_data %>%
    slice(1) %>%
    mutate(date = glue("{Year}_{Month}_{Day}")) %>%
    pull(date)
  
  aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  
  # Load AOI and clip layers
  aoi_path <- glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson")
  aoi <- st_read(aoi_path) 
  
  # Load pedestrian area raster
  ped_area_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/pedestrian-areas__baseline__baseline.tif")
  ped_area_rast <- load_and_merge(ped_area_paths) 
  
  aoi <- aoi %>%
    st_transform(st_crs(ped_area_rast))
  ped_area_rast <- ped_area_rast %>% 
    mask(aoi)
  
  pedestrian_area <- sum(values(ped_area_rast) != 0, na.rm = TRUE)
  
  # Load non-building area raster
  nonbuild_area_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
  nonbuild_area_rast <- load_and_merge(nonbuild_area_paths) 
  
  nonbuild_area_rast <- nonbuild_area_rast %>% 
    mask(aoi)
  
  nonbuild_area <- sum(values(nonbuild_area_rast) != 0, na.rm = TRUE)
  
  # Initialize results list
  results <- tibble()
  
  # scenario_base <- here(scenario_path, scenario)
  timestamps <- c("1200", "1500", "1800")
  
  # infra_file_name <- glue("trees_{scenario}")
  
  for (time in timestamps) {
    
    # Load shade raster and mask to AOI
    baseline_shade_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-{time}__baseline__baseline.tif")
    baseline_shade_rast <- load_and_merge(baseline_shade_paths) 
    baseline_shade_rast <- baseline_shade_rast %>% 
      mask(aoi) > 0
    
    scenario_shade_paths <- glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/shade-{time}__trees__{scenario}.tif")
    scenario_shade_rast <- load_and_merge(scenario_shade_paths) 
    scenario_shade_rast <- scenario_shade_rast %>% 
      mask(aoi) > 0
    
    # Mask UTCI to AOI
    baseline_utci_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    baseline_utci_rast <- load_and_merge(baseline_utci_paths)
    baseline_utci_rast <- baseline_utci_rast %>% 
      mask(aoi)
    
    scenario_utci_paths <- glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__trees__{scenario}.tif")
    scenario_utci_rast <- load_and_merge(scenario_utci_paths)
    scenario_utci_rast <- scenario_utci_rast %>% 
      mask(aoi)
    
    ped_area_rast <- ped_area_rast %>% 
      crop(baseline_utci_rast)
    
    nonbuild_area_rast <- nonbuild_area_rast %>% 
      crop(baseline_utci_rast)
    
    # Compute metrics
    # UTCI AOI
    mean_utci_baseline_aoi <- mean(values(baseline_utci_rast %>% 
                                            subst(0, NA)), na.rm = TRUE)
    mean_utci_scenario_aoi <- mean(values(scenario_utci_rast %>% 
                                            subst(0, NA)), na.rm = TRUE)
    utci_diff_aoi <- mean_utci_scenario_aoi - mean_utci_baseline_aoi
    
    # Shade AOI
    shade_cover_baseline_aoi <- mean(values(baseline_shade_rast), na.rm = TRUE)
    shade_cover_scenario_aoi <- mean(values(scenario_shade_rast), na.rm = TRUE)
    shade_diff_aoi <- shade_cover_scenario_aoi - shade_cover_baseline_aoi
    
    # UTCI pedestrian
    mean_utci_baseline_pedestrian <- mean(values(mask(baseline_utci_rast, ped_area_rast, maskvalues = 0) %>% 
                                                   subst(0, NA)), na.rm = TRUE)
    mean_utci_scenario_pedestrian <- mean(values(mask(scenario_utci_rast, ped_area_rast, maskvalues = 0) %>% 
                                                   subst(0, NA)), na.rm = TRUE)
    utci_diff_pedestrian <- mean_utci_scenario_pedestrian - mean_utci_baseline_pedestrian
    
    # Shade pedestrian
    shade_cover_baseline_pedestrian <- mean(values(mask(baseline_shade_rast, ped_area_rast, maskvalues = 0), na.rm = TRUE)) 
    shade_cover_scenario_pedestrian <- mean(values(mask(scenario_shade_rast, ped_area_rast, maskvalues = 0), na.rm = TRUE)) 
    shade_diff_pedestrian <- shade_cover_scenario_pedestrian - shade_cover_baseline_pedestrian
    
    # UTCI nonbuild
    mean_utci_baseline_nonbuild <- mean(values(mask(baseline_utci_rast, nonbuild_area_rast, maskvalues = 0) %>% 
                                                 subst(0, NA)), na.rm = TRUE)
    mean_utci_scenario_nonbuild <- mean(values(mask(scenario_utci_rast, nonbuild_area_rast, maskvalues = 0) %>% 
                                                 subst(0, NA)), na.rm = TRUE)
    utci_diff_nonbuild <- mean_utci_scenario_nonbuild - mean_utci_baseline_nonbuild
    
    # Shade nonbuild
    shade_cover_baseline_nonbuild <- mean(values(mask(baseline_shade_rast, nonbuild_area_rast, maskvalues = 0), na.rm = TRUE)) 
    shade_cover_scenario_nonbuild <- mean(values(mask(scenario_shade_rast, nonbuild_area_rast, maskvalues = 0), na.rm = TRUE)) 
    shade_diff_nonbuild <- shade_cover_scenario_nonbuild - shade_cover_baseline_nonbuild
    
    # Store results
    metrics <- tibble(
      
      mean_utci_baseline_aoi = mean_utci_baseline_aoi,
      mean_utci_scenario_aoi = mean_utci_scenario_aoi,
      mean_utci_change_aoi = utci_diff_aoi,
      
      shade_cover_baseline_aoi = shade_cover_baseline_aoi * 100,
      shade_cover_scenario_aoi = shade_cover_scenario_aoi * 100,
      shade_cover_change_aoi = shade_diff_aoi,
      
      mean_utci_baseline_pedestrian = mean_utci_baseline_pedestrian,
      mean_utci_scenario_pedestrian = mean_utci_scenario_pedestrian,
      mean_utci_change_pedestrian = utci_diff_pedestrian,
      
      shade_cover_baseline_pedestrian = shade_cover_baseline_pedestrian * 100,
      shade_cover_scenario_pedestrian = shade_cover_scenario_pedestrian * 100,
      shade_cover_change_pedestrian = shade_diff_pedestrian,
      
      mean_utci_baseline_nonbuilding_areas = mean_utci_baseline_nonbuild,
      mean_utci_scenario_nonbuilding_areas = mean_utci_scenario_nonbuild,
      mean_utci_change_nonbuilding_areas = utci_diff_nonbuild,
      
      shade_cover_baseline_nonbuilding_areas = shade_cover_baseline_nonbuild * 100,
      shade_cover_scenario_nonbuilding_areas = shade_cover_scenario_nonbuild * 100,
      shade_cover_change_nonbuilding_areas = shade_diff_nonbuild
      
    ) %>%
      pivot_longer(
        cols = everything(),
        names_to = "indicators_id",
        values_to = "value"
      ) %>%
      rowwise() %>% 
      mutate(
        metric = str_extract(indicators_id, "^[^_]+_[^_]+"),
        metric2 = str_replace(indicators_id, metric, ""),
        indicators_id = paste0(metric, "_", time, metric2)
      ) %>%
      select(-metric, -metric2)
    
    
    results <- bind_rows(results, metrics)
  }
  
  
  # Load updated trees
  baseline_tree_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/tree-cover__baseline__baseline.tif")
  baseline_tree_rast <- load_and_merge(baseline_tree_paths) %>% 
    subst(from = NA, 0) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  
  scenario_tree_paths <- glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/tree-cover__trees__{scenario}.tif")
  scenario_tree_rast <- load_and_merge(scenario_tree_paths) %>% 
    subst(from = NA, 0) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  
  # Tree pct
  tree_cover_baseline_aoi <- mean(values(baseline_tree_rast, na.rm = TRUE)) 
  tree_cover_scenario_aoi <- mean(values(scenario_tree_rast, na.rm = TRUE)) 
  tree_cover_change_aoi <- tree_cover_scenario_aoi - tree_cover_baseline_aoi
  
  tree_cover_baseline_pedestrian <- mean(values(mask(baseline_tree_rast, ped_area_rast, maskvalues = 0), na.rm = TRUE)) 
  tree_cover_scenario_pedestrian <- mean(values(mask(scenario_tree_rast, ped_area_rast, maskvalues = 0), na.rm = TRUE)) 
  tree_cover_change_pedestrian <- tree_cover_scenario_pedestrian - tree_cover_baseline_pedestrian
  
  # Number of trees
  # Baseline
  baseline_tree_points <- map_dfr(tiles_aoi, function(time) {
    baseline_utci <- rast(glue("{aws_http}/{baseline_folder}/{time}/ccl_layers/utci-1200__baseline__baseline.tif"))
    bbox_sf <- st_as_sf(st_as_sfc(st_bbox(baseline_utci)))
    st_read(glue("{aws_http}/{baseline_folder}/{time}/ccl_layers/tree-points__baseline__baseline.geojson"), quiet = TRUE) %>%
      st_filter(bbox_sf) }) %>%
    st_filter(aoi)
  
  # Extract raster values at each point
  pedestrian_vals <- terra::extract(ped_area_rast, vect(baseline_tree_points))
  baseline_tree_points$pedestrian <- pedestrian_vals[[2]]  # second column is the raster value
  tree_n_baseline_pedestrian <- nrow(baseline_tree_points %>% filter(pedestrian == 1))
  
  # Scenario
  scenario_tree_points <- st_read(glue("{aws_http}/{scenario_folder}/new-tree-points__trees__{scenario}.geojson"), 
                                  quiet = TRUE)
  tree_n_scenario_pedestrian <- nrow(scenario_tree_points)
  
  # Achievable potential
  aws_path <- paste0("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/OpenUrban/", city, "/scenarios/street-trees/", city, "-street-tree-pct-1km-grid.csv")
  ped_area_tree_dist <- read_csv(aws_path)
  target_coverage <- quantile(ped_area_tree_dist$`pct-tree`, 0.9, names = FALSE, na.rm = TRUE)
  
  tree_metrics <- 
    tibble(
      tree_cover_baseline_aoi = tree_cover_baseline_aoi * 100,
      tree_cover_scenario_aoi = tree_cover_scenario_aoi * 100,
      tree_cover_change_aoi = tree_cover_change_aoi * 100,
      
      tree_cover_baseline_pedestrian = tree_cover_baseline_pedestrian * 100,
      tree_cover_scenario_pedestrian = tree_cover_scenario_pedestrian * 100,
      tree_cover_change_pedestrian = tree_cover_change_pedestrian * 100,
      
      tree_n_baseline_pedestrian = tree_n_baseline_pedestrian,
      tree_n_change_pedestrian = tree_n_scenario_pedestrian,
      tree_n_scenario_pedestrian = tree_n_scenario_pedestrian + tree_n_baseline_pedestrian,
      
      tree_cover_achievable_pedestrian = target_coverage * 100,
      tree_cover_progress = tree_cover_scenario_pedestrian / tree_cover_achievable_pedestrian * 100
    ) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "indicators_id",
      values_to = "value"
    )
  
  results <- bind_rows(results, tree_metrics) %>% 
    mutate(application_id = "ccl",
           cities_id = city,
           areas_of_interest_id = str_replace(aoi_name, "-", "_"),
           interventions_id = "trees",
           scenarios_id = glue("trees__{scenario}"),
           date = date,
           value = round(value, 2))
  
  # Save results
  ensure_s3_prefix("wri-cities-tcm", glue("city_projects/{city}/{aoi_name}/scenarios/metrics"))
  write_s3(results, glue("wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/metrics/metrics__trees__{scenario}.csv"))
  
}

# cool roofs
calc_cool_roofs_metrics <- function(city, aoi_name, tiles_aoi, scenario){
  
  aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/cool-roofs/{scenario}")
  
  # Load albedo
  baseline_albedo_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/albedo__baseline__baseline.tif")
  baseline_albedo <- load_and_merge(baseline_albedo_paths) 
  
  scenario_albedo_paths <- glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/albedo__cool-roofs__{scenario}.tif")
  scenario_albedo <- load_and_merge(scenario_albedo_paths)
  
  # Load buildings
  building_area_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/building-areas__baseline__baseline.tif")
  buildings <- load_and_merge(building_area_paths) %>% 
    subst(0, NA)
  
  if (str_detect(city, "USA")) {
    
    # Load LULC
    lulc_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/raster_files/cif_open_urban.tif")
    lulc <- load_and_merge(lulc_paths) 
    
    rcl <- matrix(c(
      602, 1,
      610, 1,
      612, 1,
      622, 1,
      600, 2,
      601, 2,
      611, 2,
      620, 2,
      621, 2
    ), ncol = 2, byrow = TRUE)
    
    lulc_rc <- classify(lulc, rcl, others = 0)
    
    buildings_low <- lulc_rc == 1
    buildings_high <- lulc_rc == 2
    
    alb_target_low <- quantile(values(mask(baseline_albedo, buildings_low)), 0.9, na.rm = TRUE)
    alb_target_high <- quantile(values(mask(baseline_albedo, buildings_high)), 0.9, na.rm = TRUE)
    
    achievable_albedo <- ifel(
      buildings_low & baseline_albedo < alb_target_low,
      alb_target_low,
      baseline_albedo
    ) %>% ifel(
      buildings_high & baseline_albedo < alb_target_high,
      alb_target_high,
      baseline_albedo
    ) 
    
  } else {
    # Achievable albedo
    alb_target <- quantile(values(mask(baseline_albedo, buildings)), 0.9, na.rm = TRUE)
    
    achievable_albedo <- ifel(
      buildings & baseline_albedo < alb_target,
      alb_target,
      baseline_albedo
    ) 
  }
  
  
  # Load AOI and clip layers
  aoi_path <- glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson")
  aoi <- st_read(aoi_path) %>% 
    st_transform(st_crs(baseline_albedo))
  
  # Mask albedo to AOI
  baseline_albedo <- baseline_albedo %>% crop(aoi) %>% mask(aoi)
  scenario_albedo <- scenario_albedo %>% crop(aoi) %>% mask(aoi)
  achievable_albedo <- achievable_albedo %>% crop(aoi) %>% mask(aoi)
  buildings <- buildings %>% crop(aoi) %>% mask(aoi)

  # Mask albedo to buildings
  baseline_building_alb <- baseline_albedo %>% mask(buildings)
  scenario_building_alb <- scenario_albedo %>% mask(buildings)
  achievable_building_alb <- achievable_albedo %>% mask(buildings)

  # baseline_roof_alb <- mean(values(baseline_building_alb), na.rm = TRUE)
  # scenario_roof_alb <- mean(values(scenario_building_alb), na.rm = TRUE)
  
  timestamps <- c("1200", "1500", "1800")
  
  baseline_met_data <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"),
                          skip = 2)
  scenario_met_data <- read_csv(glue("{aws_http}/{scenario_folder}/metadata/met_files/reduced_temps.csv"),
                          skip = 2)
  
  date <- baseline_met_data %>%
    slice(1) %>%
    mutate(date = glue("{Year}_{Month}_{Day}")) %>%
    pull(date)
  
  # UTCI difference
  utci_metrics <- tibble()
  
  # Load non-building area raster
  nonbuild_area_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
  nonbuild_area_rast <- load_and_merge(nonbuild_area_paths) %>% 
    crop(aoi) %>% 
    mask(aoi)
  
  for (time in timestamps) {
    
    # Mask UTCI to AOI
    baseline_utci_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    baseline_utci_rast <- load_and_merge(baseline_utci_paths)
    baseline_utci_rast <- baseline_utci_rast %>% 
      crop(aoi) %>% 
      mask(aoi) 
    
    scenario_utci_paths <- glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__cool-roofs__{scenario}.tif")
    scenario_utci_rast <- load_and_merge(scenario_utci_paths)
    scenario_utci_rast <- scenario_utci_rast %>% 
      crop(aoi) %>% 
      mask(aoi)
    
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
  
  # pixel areas (in m²)
  px_area <- cellSize(baseline_albedo)
  nonbuild_area <- global(px_area * nonbuild_area_rast, "sum", na.rm = TRUE)[1, 1]
  
  # Cool roof vectors
  buildings_vect_paths <- glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/buildings__cool-roofs__all-buildings.geojson")
  buildings_vect <- map_df(buildings_vect_paths, ~ st_read(.)) %>% 
    distinct(id, .keep_all = TRUE)
  
  buildings_scenario <- buildings_vect %>% 
    filter(updated)
  
  cool_roofs_baseline <- buildings_vect %>% 
    filter(median_alb >= cool_roof_alb)
  cool_roofs_scenario <- buildings_vect %>% 
    filter(median_alb >= cool_roof_alb | updated)
  
  # Roof albedos
  baseline_roof_alb <- mean(buildings_vect$median_alb, na.rm = TRUE) 
  scenario_roof_alb <- buildings_vect %>% 
    mutate(updated_alb = case_when(updated ~ cool_roof_alb,
                                   ! updated ~ median_alb)) %>% 
    summarize(mean = mean(updated_alb, na.rm = TRUE)) %>% 
    pull(mean) 

  # total area
  baseline_cool_roof_area <- sum(cool_roofs_baseline$area_m2)
  scenario_cool_roof_area <- sum(cool_roofs_scenario$area_m2)
  technical_cool_roof_area <- sum(buildings_vect$area_m2)
  
  # Achievable targets
  alb_target_val      <- if (exists("alb_target")) alb_target else NA_real_
  alb_target_low_val  <- if (exists("alb_target_low")) alb_target_low else NA_real_
  alb_target_high_val <- if (exists("alb_target_high")) alb_target_high else NA_real_
  
  # Initialize results list
  results <- tibble(
    "baseline_cool_roof_area" = baseline_cool_roof_area,
    "scenario_cool_roof_area" = scenario_cool_roof_area,
    "technical_cool_roof_area" = technical_cool_roof_area,
    "change_cool_roof_area" = scenario_cool_roof_area - baseline_cool_roof_area,
    
    "achievable_cool_roof_reflectivity" = alb_target_val * 100,
    "achievable_cool_roof_reflectivity_low" = alb_target_low_val * 100,
    "achievable_cool_roof_reflectivity_high" = alb_target_high_val * 100,
    
    "baseline_reflectivity" = mean(values(baseline_albedo), na.rm = TRUE) * 100,
    "scenario_reflectivity" = mean(values(scenario_albedo), na.rm = TRUE) * 100,
    "achievable_reflectivity" = mean(values(achievable_albedo), na.rm = TRUE) * 100,
    "change_reflectivity" = (scenario_reflectivity - baseline_reflectivity),
    
    "baseline_roof_reflectivity" = baseline_roof_alb * 100,
    "scenario_roof_reflectivity" = scenario_roof_alb * 100,
    "change_roof_reflectivity" = (scenario_roof_reflectivity - baseline_roof_reflectivity),
    
    "progress_reflectivity" = scenario_reflectivity / achievable_reflectivity,    
    "progress_cool_roofs" = (baseline_cool_roof_area + change_cool_roof_area) / technical_cool_roof_area * 100,
    
    "baseline_mean_air_temp_1200" = (baseline_met_data %>% filter(Hour == 12) %>% pull(Temperature)),
    "baseline_mean_air_temp_1500" = (baseline_met_data %>% filter(Hour == 15) %>% pull(Temperature)),
    "baseline_mean_air_temp_1800" = (baseline_met_data %>% filter(Hour == 18) %>% pull(Temperature)),
    "scenario_mean_air_temp_1200" = (scenario_met_data %>% filter(Hour == 12) %>% pull(Temperature)),
    "scenario_mean_air_temp_1500" = (scenario_met_data %>% filter(Hour == 15) %>% pull(Temperature)),
    "scenario_mean_air_temp_1800" = (scenario_met_data %>% filter(Hour == 18) %>% pull(Temperature)),
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
  
  
  # Save results
  ensure_s3_prefix("wri-cities-tcm", glue("city_projects/{city}/{aoi_name}/scenarios/metrics"))
  write_s3(results, glue("wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/metrics/metrics__cool-roofs__{scenario}.csv"))
  
  
}


calc_shade_structures_metrics <- function(city, aoi_name, tiles_aoi, scenario){
  
  library(geoarrow)
  library(sfarrow)
  
  aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/shade-structures/{scenario}")
  
  # Load parks 
  parks <- st_read_parquet(glue("{aws_http}/{baseline_folder}/parks-polygons__baseline__baseline.parquet"))
  
  # Load AOI 
  aoi <- st_read(glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson"))
  
  # Load shade structures
  shade_structures <- st_read(glue("{aws_http}/{scenario_folder}/structures__shade-structures__all-parks.geojson"))
  
  # Met data
  met_data <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"),
                       skip = 2)
  
  date <- met_data %>%
    slice(1) %>%
    mutate(date = glue("{Year}_{Month}_{Day}")) %>%
    pull(date)
  
  timestamps <- c("1200", "1500", "1800")
  
  # Initialize results list
  results <- tibble()
  
  for (time in timestamps) {
    
    # Load existing UTCI raster & mask to parks
    baseline_utci_paths <- glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    baseline_utci_rast <- load_and_merge(baseline_utci_paths)
    baseline_utci_rast <- baseline_utci_rast %>% 
      crop(aoi) %>% 
      mask(parks) 
    
    scenario_utci_paths <- glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__shade-structures__{scenario}.tif")
    scenario_utci_rast <- load_and_merge(scenario_utci_paths)
    scenario_utci_rast <- scenario_utci_rast %>% 
      crop(aoi) %>% 
      mask(parks)
    
    # Load shade raster and combine tree and building shade
    baseline_shade_rast <- rast(here(baseline_path, baseline_shadow_files[str_detect(baseline_shadow_files, time)])) < 1
    baseline_shade_rast <- baseline_shade_rast %>% 
      mask(parks)
    
    scenario_shade_rast <- rast(here(scenario_path, scenario_shadow_files[str_detect(scenario_shadow_files, time)])) < 1
    scenario_shade_rast <- scenario_shade_rast %>% 
      mask(parks)
    
    # Compute metrics
    
    # percent shade in parks, 1 = shade
    baseline_park_shade_pct <- mean(values(baseline_shade_rast), na.rm = TRUE)
    scenario_park_shade_pct <- mean(values(scenario_shade_rast), na.rm = TRUE)
    change_park_shade_pct = scenario_park_shade_pct - baseline_park_shade_pct
    
    # area shade in parks
    park_base <- exactextractr::exact_extract(baseline_shade_rast, parks, 'mean')
    park_scenario <- exactextractr::exact_extract(scenario_shade_rast, parks, 'mean')
    parks <- parks %>% 
      st_transform(st_crs(scenario_shade_rast)) %>% 
      mutate(area_sqm = as.numeric(st_area(geometry)),
             baseline_shade_area = area_sqm * park_base,
             scenario_shade_area = area_sqm * park_scenario,
             baseline_shade_pct = baseline_shade_area / area_sqm)
    
    # utci in parks
    baseline_mean_utci_parks <- mean(values(baseline_utci), na.rm = TRUE)
    scenario_mean_utci_parks <- mean(values(scenario_utci), na.rm = TRUE)
    change_mean_utci_parks <- scenario_mean_utci_parks - baseline_mean_utci_parks
    
    # Store results
    metrics <- tibble(
      
      time = time,
      
      baseline_mean_utci_parks = baseline_mean_utci_parks,
      scenario_mean_utci_parks = scenario_mean_utci_parks,
      change_mean_utci_parks = change_mean_utci_parks,
      
      baseline_park_shade_pct = baseline_park_shade_pct * 100,
      scenario_park_shade_pct = scenario_park_shade_pct * 100,
      change_park_shade_pct = change_park_shade_pct * 100,
      
      baseline_park_shade = sum(parks$baseline_shade_area),
      scenario_park_shade = sum(parks$scenario_shade_area),
      change_park_area_shade = scenario_park_shade - baseline_park_shade,
      
      baseline_park_shade_cover = baseline_park_shade_pct,
      change_park_shade_cover = change_park_shade_pct,
      
    )
    
    results <- bind_rows(results, metrics)
  }
  
  results_long <- results %>%
    pivot_longer(
      cols = -c(time),
      names_to = "indicators_id",
      values_to = "value"
    ) %>%
    mutate(indicators_id = paste0(indicators_id, "_", time)) %>% 
    bind_rows(tribble(~ indicators_id, ~ value,
                      "new_shade_structures", nrow(shade_structures),
                      "achievable_park_shade_cover_1200", quantile(parks$baseline_shade_pct, 0.9) * 100)) %>% 
    filter(! indicators_id %in% c("baseline_park_shade_cover_1500", "baseline_park_shade_cover_1800",
                                  "change_park_shade_cover_1500", "change_park_shade_cover_1800")) 
  
  progress <- results_long %>% 
    filter(indicators_id %in% c("baseline_park_shade_cover_1200", 
                                "change_park_shade_cover_1200",
                                "achievable_park_shade_cover_1200")) %>% 
    select(indicators_id, value) %>% 
    pivot_wider(names_from = indicators_id) %>% 
    mutate(x = (baseline_park_shade_cover_1200 + change_park_shade_cover_1200) / achievable_park_shade_cover_1200) %>% 
    pull(x)
  
  results_long <- results_long %>% 
    bind_rows(tibble(indicators_id = "shade_structure_progress", value = progress * 100)) %>% 
    mutate(
      date = date,
      application_id = "ccl",
      cities_id = city,
      areas_of_interest_id = aoi_name,
      interventions_id = "park_shade",
      scenarios_id = paste("park_shade", str_replace(scenario, "-", "_"), sep = "_"),
    ) %>%
    select(-time)
  
  write_csv(results_long, here(scenario_path, "scenario-metrics.csv"))
  
}
