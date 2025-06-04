################################################
# Choose your city & specify the scenario folder
################################################
# city <- "MEX-Monterrey"
# scenario_folder <- "achievable-90pctl"


# Calculate metrics -------------------------------------------------------

calc_metrics <- function(city, scenarios, infrastructure, aoi_name){
  
  library(tidyverse)
  library(sf)
  
  results <- tibble()
  
  for (scenario in scenarios){
    
    met_data <- list.files(here("data", city, "scenarios", "baseline"), pattern = "met", full.names = TRUE) %>%
      first() %>% 
      read_delim()
    
    metrics <- source(here("scenario-generation", infrastructure, paste0(infrastructure, "-metrics-function.R")))
    
    scenario_results <- metrics$value(city = city,
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
  write_csv(results_long, here("data", city, "scenarios", infrastructure, "scenario-metrics.csv"))
  
}


