library(terra)
library(tidyverse)
library(sf)
library(here)

source(here("tiling-scripts", "utils.R"))

baseline_metrics("BRA-Teresina", "accelerator_area")

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
  
  # Initialize results list
  results <- tibble()
  infra_file_name <- "baseline"
  
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
    
    # Compute metrics
    mean_utci_baseline_aoi <- mean(values(baseline_utci_rast %>% 
                                                   subst(0, NA)), na.rm = TRUE)
    
    shade_cover_baseline_aoi <- mean(values(baseline_shade_rast), na.rm = TRUE) 
    
    mean_utci_baseline_pedestrian <- mean(values(mask(baseline_utci_rast, ped_area_rast, maskvalues = 0) %>% 
                                       subst(0, NA)), na.rm = TRUE)
    
    shade_cover_baseline_pedestrian <- mean(values(mask(baseline_shade_rast, ped_area_rast, maskvalues = 0), na.rm = TRUE)) 
    
    # Store results
    metrics <- tibble(
      mean_utci_baseline_aoi = mean_utci_baseline_aoi,
      shade_cover_baseline_aoi = shade_cover_baseline_aoi * 100,
      mean_utci_baseline_pedestrian = mean_utci_baseline_pedestrian,
      shade_cover_baseline_pedestrian = shade_cover_baseline_pedestrian * 100
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
           time = time)
  
  # Save results
  ensure_s3_prefix("wri-cities-tcm", glue("city_projects/{city}/{aoi_name}/scenarios/metrics"))
  write_s3(results, glue("wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/metrics/metrics__baseline__baseline.csv"))
  
}




# trees
# cool roofs
# shade