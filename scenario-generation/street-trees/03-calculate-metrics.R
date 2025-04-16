################################################
# Choose your city & specify the scenario folder
################################################
# city <- "MEX-Monterrey"
# scenario_folder <- "achievable-90pctl"


# Calculate metrics -------------------------------------------------------

calc_metrics <- function(city, scenarios, infrastructure){
  
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
  
  # Save results
  write_csv(results, here("data", city, "scenarios", infrastructure, "scenario-metrics.csv"))
  
}


x <- calc_metrics(city, scenarios = c("baseline", "sandbox"), "park-shade")
