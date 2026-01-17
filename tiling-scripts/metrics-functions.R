library(terra)
library(tidyverse)
library(sf)
library(here)
library(glue)

source(here("tiling-scripts", "utils.R"))


# baseline
baseline_metrics <- function(city, aoi_name, tiles_aoi){
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  met_file <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv")
  met_data <- read_csv(met_file, skip = 2) 
  
  date <- met_data %>%
    slice(1) %>%
    mutate(date = glue("{Year}_{Month}_{Day}")) %>%
    pull(date)
  
  s3_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  
  # Load AOI and clip layers
  aoi_path <- glue("{s3_http}/{baseline_folder}/aoi__baseline__baseline.geojson")
  aoi <- st_read(aoi_path) 
  
  # Load pedestrian area raster
  ped_area_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/pedestrian-areas__baseline__baseline.tif")
  ped_area_rast <- load_and_merge(ped_area_paths) 
  
  aoi <- aoi %>%
    st_transform(st_crs(ped_area_rast))
  ped_area_rast <- ped_area_rast %>% 
    mask(aoi)

  pedestrian_area <- sum(values(ped_area_rast) != 0, na.rm = TRUE)
  
  # Load non-building area raster
  nonbuild_area_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
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
    baseline_shade_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-{time}__baseline__baseline.tif")
    baseline_shade_rast <- load_and_merge(baseline_shade_paths) 
    baseline_shade_rast <- baseline_shade_rast %>% 
      mask(aoi)
    
    # Mask UTCI to AOI
    baseline_utci_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
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
  baseline_tree_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/tree-cover__baseline__baseline.tif")
  baseline_tree_rast <- load_and_merge(baseline_tree_paths) %>% 
    subst(from = NA, 0) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  
  # Tree pct
  baseline_tree_pct_aoi <- mean(values(baseline_tree_rast, na.rm = TRUE)) 
  baseline_tree_pct_pedestrian <- mean(values(mask(baseline_tree_rast, ped_area_rast, maskvalues = 0), na.rm = TRUE)) 
   
  # Number of trees
  baseline_tree_points <- map_dfr(tiles_aoi, function(t) {
    
    baseline_utci <- terra::rast(glue(
      "{s3_http}/{baseline_folder}/{t}/ccl_layers/utci-1200__baseline__baseline.tif"
    ))
    
    bbox_sf <- st_as_sf(st_as_sfc(st_bbox(baseline_utci)))
    
    st_read(glue(
      "{s3_http}/{baseline_folder}/{t}/ccl_layers/tree-points__baseline__baseline.geojson"
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
  
  s3_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  
  # Load AOI and clip layers
  aoi_path <- glue("{s3_http}/{baseline_folder}/aoi__baseline__baseline.geojson")
  aoi <- st_read(aoi_path) 
  
  # Load pedestrian area raster
  ped_area_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/pedestrian-areas__baseline__baseline.tif")
  ped_area_rast <- load_and_merge(ped_area_paths) 
  
  aoi <- aoi %>%
    st_transform(st_crs(ped_area_rast))
  ped_area_rast <- ped_area_rast %>% 
    mask(aoi)
  
  pedestrian_area <- sum(values(ped_area_rast) != 0, na.rm = TRUE)
  
  # Load non-building area raster
  nonbuild_area_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
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
    baseline_shade_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-{time}__baseline__baseline.tif")
    baseline_shade_rast <- load_and_merge(baseline_shade_paths) 
    baseline_shade_rast <- baseline_shade_rast %>% 
      mask(aoi)
    
    scenario_shade_paths <- glue("{s3_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/shade-{time}__trees__{scenario}.tif")
    scenario_shade_rast <- load_and_merge(scenario_shade_paths) 
    scenario_shade_rast <- scenario_shade_rast %>% 
      mask(aoi)
    
    # Mask UTCI to AOI
    baseline_utci_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    baseline_utci_rast <- load_and_merge(baseline_utci_paths)
    baseline_utci_rast <- baseline_utci_rast %>% 
      mask(aoi)
    
    scenario_utci_paths <- glue("{s3_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__trees__{scenario}.tif")
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
  baseline_tree_paths <- glue("{s3_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/tree-cover__baseline__baseline.tif")
  baseline_tree_rast <- load_and_merge(baseline_tree_paths) %>% 
    subst(from = NA, 0) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  
  scenario_tree_paths <- glue("{s3_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/tree-cover__trees__{scenario}.tif")
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
  baseline_tree_points <- map_dfr(tiles_aoi, function(t) {
    baseline_utci <- rast(glue("{s3_http}/{baseline_folder}/{t}/ccl_layers/utci-1200__baseline__baseline.tif"))
    bbox_sf <- st_as_sf(st_as_sfc(st_bbox(baseline_utci)))
    st_read(glue("{s3_http}/{baseline_folder}/{t}/ccl_layers/tree-points__baseline__baseline.geojson"), quiet = TRUE) %>%
      st_filter(bbox_sf) }) %>%
    st_filter(aoi)
  
  # Extract raster values at each point
  pedestrian_vals <- terra::extract(ped_area_rast, vect(baseline_tree_points))
  baseline_tree_points$pedestrian <- pedestrian_vals[[2]]  # second column is the raster value
  tree_n_baseline_pedestrian <- nrow(baseline_tree_points %>% filter(pedestrian == 1))
  
  # Scenario
  scenario_tree_points <- st_read(glue("{s3_http}/{scenario_folder}/new-tree-points__trees__{scenario}.geojson"), 
                                  quiet = TRUE)
  tree_n_scenario_pedestrian <- nrow(scenario_tree_points)
  
  # Achievable potential
  aws_path <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/", city, "/scenarios/street-trees/", city, "-street-tree-pct-1km-grid.csv")
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
# shade