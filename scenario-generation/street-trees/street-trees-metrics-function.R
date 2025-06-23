# scenario <- "achievable-90pctl"
# infrastructure <- "street-trees"

calc_street_tree_metrics <- function(city, scenario, infrastructure, aoi_name){
  
  library(terra)
  library(tidyverse)
  library(sf)
  library(here)
  
  source(here("utils", "utci.R"))
  
  met_data <- list.files(here("data", city, "scenarios", "baseline"), pattern = "met", full.names = TRUE) %>%
    first() %>% 
    read_delim()
  
  date <- met_data %>% 
    slice(1) %>% 
    mutate(date = paste(`%iy`, id, sep = "_")) %>% 
    pull(date)
  
  baseline_path <- here("data", city, "scenarios", "baseline")
  scenario_path <- here("data", city, "scenarios", infrastructure, scenario)
  
  # Load AOI and clip layers
  aoi <- st_read(here("data", city, "boundaries.geojson"))
  
  # Load pedestrian area raster
  ped_area_rast <- rast(here(baseline_path, "pedestrian_areas.tif")) %>% 
    mask(aoi)
  pedestrian_area <- sum(values(ped_area_rast) != 0, na.rm = TRUE)
  
  # Initialize results list
  results <- tibble()
    
  # scenario_base <- here(scenario_path, scenario)
  timestamps <- c("1200", "1500", "1800")
  
  if (is.null(infrastructure)) {
    # Do nothing
  } else if (infrastructure == "street-trees"){
    infra_file_name <- "street_trees"
  } else if (infrastructure == "park-shade-structures"){
    infra_file_name <- "park_shade"
  } else if (infrastructure == "cool-roofs"){
    infra_file_name <- "cool_roofs"
  } 
  
  for (time in timestamps) {

    scenario_utci_rast <- rast(here(scenario_path, 
                                    paste0("utci_", time, "_", infra_file_name, "_achievable", ".tif")))
    
    # Load shade raster and mask to AOI
    baseline_shade_rast <- rast(here(baseline_path, paste0("shade_", time, "_baseline.tif"))) < 1
    scenario_shade_rast <- rast(here(scenario_path, paste0("shade_", time, "_", infra_file_name, "_achievable", ".tif"))) < 1
    scenario_shade_rast <- scenario_shade_rast %>% 
      crop(baseline_shade_rast)
    
    baseline_shade_rast <- baseline_shade_rast %>% 
      mask(aoi)
    scenario_shade_rast <- scenario_shade_rast %>% 
      mask(aoi)
    
    # Mask UTCI to AOI
    baseline_utci_rast <- rast(here(baseline_path, paste0("utci_", time, "_baseline.tif"))) 
    scenario_utci_rast <- scenario_utci_rast %>% 
      crop(baseline_utci_rast)
    
    baseline_utci_rast <- baseline_utci_rast %>% 
      mask(aoi)
    scenario_utci_rast <- scenario_utci_rast %>% 
      mask(aoi)
    
    ped_area_rast <- ped_area_rast %>% 
      crop(baseline_utci_rast)
    
    # Compute metrics
    baseline_utci_avg <- mean(values(mask(baseline_utci_rast, ped_area_rast, maskvalues = 0) %>% 
                                       subst(0, NA)), na.rm = TRUE)
    scenario_utci_avg <- mean(values(mask(scenario_utci_rast, ped_area_rast, maskvalues = 0) %>% 
                                       subst(0, NA)), na.rm = TRUE)
    utci_diff <- scenario_utci_avg - baseline_utci_avg
    
    baseline_shade_pct <- sum(values(mask(baseline_shade_rast, ped_area_rast, maskvalues = 0) 
                                     %>% subst(NA, 0)) != 0) / pedestrian_area
    scenario_shade_pct <- sum(values(mask(scenario_shade_rast, ped_area_rast, maskvalues = 0) 
                                     %>% subst(NA, 0)) != 0) / pedestrian_area
    shade_diff <- scenario_shade_pct - baseline_shade_pct
    
    # Store results
    metrics <- tibble(
      date = date,
      time = time,
      
      mean_utci_baseline_pedestrian = baseline_utci_avg,
      mean_utci_scenario_pedestrian = scenario_utci_avg,
      mean_utci_change_pedestrian = utci_diff,
      
      shade_cover_baseline_pedestrian = baseline_shade_pct * 100,
      shade_cover_scenario_pedestrian = scenario_shade_pct * 100,
      shade_cover_change_pedestrian = shade_diff * 100
    ) %>%
      pivot_longer(
        cols = -c(date, time),
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

    
    results <- bind_rows(results, metrics)
  }
  
  # Load updated trees
  baseline_tree_rast <- rast(here(baseline_path, "tree_cover_baseline.tif")) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  scenario_tree_rast <- rast(here(scenario_path, "tree_cover_achievable.tif")) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  
  # Tree pct
  baseline_tree_pct <- sum(values(mask(baseline_tree_rast, ped_area_rast, maskvalues = 0) %>% subst(NA, 0)) != 0) / pedestrian_area
  scenario_tree_pct <- sum(values(mask(scenario_tree_rast, ped_area_rast, maskvalues = 0) %>% subst(NA, 0)) != 0) / pedestrian_area
  tree_cover_diff <- scenario_tree_pct - baseline_tree_pct
  
  # Number of trees
  baseline_tree_points <- st_read(here(baseline_path, "tree_points_baseline.geojson")) %>% 
    st_transform(st_crs(ped_area_rast))
  
  # Extract raster values at each point
  pedestrian_vals <- terra::extract(ped_area_rast, vect(baseline_tree_points))
  baseline_tree_points$pedestrian <- pedestrian_vals[[2]]  # second column is the raster value
  
  baseline_tree_n <- nrow(baseline_tree_points %>% filter(pedestrian == 1))
  
  scenario_tree_points <- st_read(here(scenario_path, "tree_points_achievable.geojson"))
  scenario_tree_n <- nrow(scenario_tree_points)
  
  # Achievable potential
  aws_path <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/", city, "/scenarios/street-trees/", city, "-street-tree-pct-1km-grid.csv")
  ped_area_tree_dist <- read_csv(aws_path)
  target_coverage <- quantile(ped_area_tree_dist$`pct-tree`, 0.9, names = FALSE, na.rm = TRUE)
  
  tree_metrics <- 
    tibble(
      tree_cover_baseline_pedestrian = baseline_tree_pct * 100,
      tree_cover_scenario_pedestrian = scenario_tree_pct * 100,
      tree_cover_change_pedestrian = tree_cover_diff * 100,
      tree_n_baseline_pedestrian = baseline_tree_n,
      tree_n_scenario_pedestrian = scenario_tree_n + baseline_tree_n,
      tree_n_change_pedestrian = scenario_tree_n,
      tree_cover_achievable_pedestrian = target_coverage * 100
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
           interventions_id = str_replace(infrastructure, "-", "_"),
           scenarios_id = paste(interventions_id, str_replace(scenario, "-", "_"), sep = "_"))
  
  # Save results
  write_csv(results, here(scenario_path, "scenario-metrics.csv"))
  
}

  
  