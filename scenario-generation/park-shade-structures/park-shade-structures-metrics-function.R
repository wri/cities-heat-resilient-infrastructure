calc_street_park_shade_metrics <- function(city, scenario, aoi_name){

  library(terra)
  library(tidyverse)
  library(sf)
  library(here)
  
  baseline_path <- here("data", city, "scenarios", "baseline")
  infrastructure_path <- here("data", city, "scenarios", "park-shade-structures")
  scenario_path <- here(infrastructure_path, scenario)
  
  # Load parks 
  parks <- st_read(here(infrastructure_path, "parks.geojson"))
  
  timestamps <- c("1200", "1500", "1800")
  
  baseline_utci_files <- list.files(baseline_path, pattern = "utci") %>%
    discard(~ str_detect(.x, "vs"))
  baseline_shadow_files <- list.files(baseline_path, pattern = "^shade") %>%
    discard(~ str_detect(.x, "vs")) %>% 
    discard(~ str_detect(.x, "geojson")) 
  
  scenario_utci_files <- list.files(scenario_path, pattern = "utci") %>%
    discard(~ str_detect(.x, "vs"))
  scenario_shadow_files <- list.files(scenario_path, pattern = "^shade") %>%
    discard(~ str_detect(.x, "vs")) %>% 
    discard(~ str_detect(.x, "geojson")) 
  
  # Load AOI 
  aoi <- st_read(here("data", city, "boundaries.geojson"))
  
  # Load shade structures
  shade_structures <- st_read(here(scenario_path, "shade_structures.geojson"))
  
  # Met data
  met_data <- read_delim(here(baseline_path, "met_era5_hottest_days.txt"))
  
  date <- baseline_Ta %>% 
    slice(1) %>% 
    mutate(date = paste(`%iy`, id, sep = "_")) %>% 
    pull(date)
  
  # Initialize results list
  results <- tibble()
  
  for (t in timestamps) {
      
    # Load existing UTCI raster & mask to parks
    baseline_utci <- rast(here(baseline_path, baseline_utci_files[str_detect(baseline_utci_files, t)])) %>% 
      mask(parks)
    scenario_utci <- rast(here(scenario_path, scenario_utci_files[str_detect(scenario_utci_files, t)])) %>% 
      mask(parks)
    
    # Load shade raster and combine tree and building shade
    baseline_shade_rast <- rast(here(baseline_path, baseline_shadow_files[str_detect(baseline_shadow_files, t)])) < 1
    baseline_shade_rast <- baseline_shade_rast %>% 
      mask(parks)
    
    scenario_shade_rast <- rast(here(scenario_path, scenario_shadow_files[str_detect(scenario_shadow_files, t)])) < 1
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
      mutate(baseline_shade_area = area_sqm * park_base,
             scenario_shade_area = area_sqm * park_scenario)
    
    # utci in parks
    baseline_mean_utci_parks <- mean(values(baseline_utci), na.rm = TRUE)
    scenario_mean_utci_parks <- mean(values(scenario_utci), na.rm = TRUE)
    change_mean_utci_parks <- scenario_mean_utci_parks - baseline_mean_utci_parks
    
    # Store results
    metrics <- tibble(
      
      time = t,
      
      baseline_mean_utci_parks = baseline_mean_utci_parks,
      scenario_mean_utci_parks = scenario_mean_utci_parks,
      change_mean_utci_parks = change_mean_utci_parks,
      
      baseline_park_shade_pct = baseline_park_shade_pct * 100,
      scenario_park_shade_pct = scenario_park_shade_pct * 100,
      change_park_shade_pct = change_park_shade_pct,
      
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
                     "achievable_park_shade_cover_1200", quantile(parks$shaded_pct, 0.9) * 100)) %>% 
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
      cities = city,
      areas_of_interest_id = aoi_name,
      interventions_id = "park_shade",
      scenarios_id = paste("park_shade", str_replace(scenario, "-", "_"), sep = "_"),
    ) %>%
    select(-time)
  
  write_csv(results_long, here(scenario_path, "scenario-metrics.csv"))

}
