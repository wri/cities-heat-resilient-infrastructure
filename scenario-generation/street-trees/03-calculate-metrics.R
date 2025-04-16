################################################
# Choose your city & specify the scenario folder
################################################
# city <- "MEX-Monterrey"
# scenario_folder <- "achievable-90pctl"


# Calculate metrics -------------------------------------------------------

calc_metrics <- function(city, scenarios, infrastructure, aoi_name){
  
  results <- tibble()
  
  for (scenario in scenarios){
    
    met_data <- list.files(here("data", city, "scenarios"), pattern = "met", full.names = TRUE) %>%
      first() %>% 
      read_delim()
    
    source(here("scenario-generation", infrastructure, paste0(infrastructure, "-metrics-function.R")))
    
    scenario_results <- calc_street_park_shade_metrics(city = city,
                                                       scenario = scenario,
                                                       infrastructure = infrastructure,
                                                       met_data = met_data)
    
    results <- bind_rows(results, scenario_results)
    
  }
  
  results_long <- results %>% 
    mutate(aoi = aoi_name) %>% 
    relocate(aoi, .after = 4) %>% 
    pivot_longer(cols = !c(1:5))
  
  # Save results
  write_csv(results, here("data", city, "scenarios", infrastructure, "scenario-metrics.csv"))
  
}

