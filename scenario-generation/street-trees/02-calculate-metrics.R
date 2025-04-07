################################################
# Choose your city & specify the scenario folder
################################################
city <- "MEX-Monterrey"
scenario_folder <- "achievable-75pctl"


# Calculate metrics -------------------------------------------------------

scenario_path <- here("data", city, "scenarios", "street-trees", scenario_folder)
met <- read_delim(here("data", city, "scenarios", "met_20jan2022.txt"))

source(here("scenario-generation", "street-trees", "street-tree-metrics-function.R"))

calc_street_tree_metrics(city = city,
                         scenario_path = scenario_path,
                         met_data = met)