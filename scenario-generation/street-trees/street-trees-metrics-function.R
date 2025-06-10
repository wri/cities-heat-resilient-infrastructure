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
  
  baseline_path <- here("data", city, "scenarios", "baseline")
  scenario_path <- here("data", city, "scenarios", infrastructure, scenario)
  
  # Load AOI and clip layers
  aoi <- st_read(here("data", city, "aoi.geojson"))
  
  # Load pedestrian area raster
  ped_area_rast <- rast(here(str_remove(scenario_path, "/[^/]+$"), "pedestrian-area.tif")) %>% 
    mask(aoi)
  pedestrian_area <- sum(values(ped_area_rast) != 0, na.rm = TRUE)
  
  # Initialize results list
  results <- tibble()
    
  # scenario_base <- here(scenario_path, scenario)
  timestamps <- list.files(scenario_path, pattern = "Tmrt") %>% 
    str_extract("(?<=Tmrt_).*(?=\\.tif)") %>% 
    unique()
  
  for (time in timestamps) {
    
    utci_path <- here(scenario_path, paste0("UTCI_", time, ".tif"))
    
    # Compute UTCI if the file doesn't already exist
    if (file.exists(utci_path)) {
      
      scenario_utci_rast <- rast(utci_path)  # Load existing UTCI raster
      
    } else {
      
      Tmrt <- rast(here(scenario_path, paste0("Tmrt_", time, ".tif")))
      scenario_utci_rast <- create_utci(mrt_rast = Tmrt, timestamp = time, met_data = met_data)
      
      writeRaster(scenario_utci_rast, here(scenario_path, paste0("UTCI_", time, ".tif")))
      
    }
    
    # Load shade raster and mask to AOI
    baseline_shade_rast <- rast(here(baseline_path, paste0("Shadow_", time, ".tif"))) < 1
    scenario_shade_rast <- rast(here(scenario_path, paste0("Shadow_", time, ".tif"))) < 1
    scenario_shade_rast <- scenario_shade_rast %>% 
      crop(baseline_shade_rast)
    
    diff_shadow <- scenario_shade_rast - baseline_shade_rast
    writeRaster(diff_shadow,
                here(scenario_path,
                     paste0("shade_diff_", time, ".tif")),
                overwrite = TRUE)
    
    baseline_shade_rast <- baseline_shade_rast %>% 
      mask(aoi)
    scenario_shade_rast <- scenario_shade_rast %>% 
      mask(aoi)
    
    # Mask UTCI to AOI
    baseline_utci_rast <- rast(here(baseline_path, paste0("UTCI_", time, ".tif"))) 
    scenario_utci_rast <- scenario_utci_rast %>% 
      crop(baseline_utci_rast)
    
    diff_utci <- scenario_utci_rast - baseline_utci_rast
    writeRaster(diff_utci,
                here(scenario_path,
                     paste0("utci_diff_", time, ".tif")),
                overwrite = TRUE)
    
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
      date = str_extract(time, "^[^_]+_[^_]+"),
      time = time,
      
      mean_utci_baseline_pedestrian = baseline_utci_avg,
      mean_utci_scenario_pedestrian = scenario_utci_avg,
      mean_utci_change_pedestrian = utci_diff,
      
      shade_cover_baseline_pedestrian = baseline_shade_pct * 100,
      shade_cover_scenario_pedestrian = scenario_shade_pct * 100,
      shade_cover_change_pedestrian = shade_diff
    ) %>%
      pivot_longer(
        cols = -c(date, time),
        names_to = "indicators_id",
        values_to = "value"
      ) %>%
      rowwise() %>% 
      mutate(
        timecode = str_extract(time, "\\d{4}(?=D)"),
        metric = str_extract(indicators_id, "^[^_]+_[^_]+"),
        metric2 = str_replace(indicators_id, metric, ""),
        indicators_id = paste0(metric, "_", timecode, metric2)
      ) %>%
      select(-time, -timecode, -metric, -metric2)

    
    results <- bind_rows(results, metrics)
  }
  
  # Load updated trees
  baseline_tree_rast <- rast(here("data", city, "scenarios", infrastructure, "existing-tree-cover.tif")) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  scenario_tree_rast <- rast(here(scenario_path, "scenario-tree-cover.tif")) %>% 
    crop(baseline_utci_rast) %>% 
    mask(aoi)
  
  # Tree pct
  baseline_tree_pct <- sum(values(mask(baseline_tree_rast, ped_area_rast, maskvalues = 0) %>% subst(NA, 0)) != 0) / pedestrian_area
  scenario_tree_pct <- sum(values(mask(scenario_tree_rast, ped_area_rast, maskvalues = 0) %>% subst(NA, 0)) != 0) / pedestrian_area
  tree_cover_diff <- scenario_tree_pct - baseline_tree_pct
  
  # Number of trees
  tree_points <- st_read(here(scenario_path, "scenario-tree-points.geojson"))
  
  baseline_tree_n <- nrow(tree_points %>% filter(type == "existing"))
  scenario_tree_n <- nrow(tree_points)
  new_trees <- nrow(tree_points %>% filter(type == "new"))
  
  # Achievable potential
  aws_path <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/", city, "/scenarios/street-trees/", city, "-street-tree-pct-1km-grid.csv")
  ped_area_tree_dist <- read_csv(aws_path)
  target_coverage <- quantile(ped_area_tree_dist$`pct-tree`, 0.9, names = FALSE, na.rm = TRUE)
  
  tree_metrics <- 
    tibble(
      tree_cover_baseline_pedestrian = baseline_tree_pct * 100,
      tree_cover_scenario_pedestrian = scenario_tree_pct * 100,
      tree_cover_change_pedestrian = tree_cover_diff,
      tree_n_baseline_pedestrian = baseline_tree_n,
      tree_n_scenario_pedestrian = scenario_tree_n,
      tree_n_change_pedestrian = new_trees,
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

  
  