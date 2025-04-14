################################################
# Choose your city & specify the scenario folder
################################################
# city <- "MEX-Monterrey"
# scenario_folder <- "achievable-90pctl"


# Calculate metrics -------------------------------------------------------

calc_metrics <- function(city, scenarios){
  
  for (scenario in scenarios){
    
    scenario_path <- here("data", city, "scenarios", "street-trees", scenario)
    met <- list.files(here("data", city, "scenarios"), pattern = "met", full.names = TRUE) %>%
      first() %>% 
      read_delim()
    
    source(here("scenario-generation", "street-trees", "street-tree-metrics-function.R"))
    
    calc_street_tree_metrics(city = city,
                             scenario_path = scenario_path,
                             met_data = met)
    
  }
  
}


