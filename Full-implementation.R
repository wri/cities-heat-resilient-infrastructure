library(here)


# Define parameters -------------------------------------------------------

# City parameters
#################
# city <- "ZAF-Cape_Town"
# city <- "BRA-Rio_de_Janeiro"
city <- "MEX-Monterrey"

if (city == "ZAF-Cape_Town"){
  aoi_file <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_Town/processed/citycentre_roi.geojson"
  aoi_name <- "business_district"
  utc_offset <- 2
} else if (city == "BRA-Rio_de_Janeiro"){
  aoi_file <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/BRA-Rio_de_janeiro/raw/boundaries/BRA-Rio_de_janeiro-DBE_low_emission_zone.geojson"
  aoi_name <- "low_emission_zone"
  utc_offset <- -3
} else if (city == "MEX-Monterrey"){
  aoi_file <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/MEX-Monterrey/solweig_09may24/aoi_monterrey.geojson"
  aoi_name <- "mitras_centro"
  utc_offset <- -6
} else {
  print("Please specifiy input parameters.")
}
  
# AOI parameters
#################
# URL for file in S3
# aoi_file <- ""
# aoi_name <- ""
  
# CTCM metadata
#################
author <- "elizabeth.wesley@wri.org" # User email

# UTC offset for the city
# utc_offset <-

# AOI buffer
buffer <- 100

# Year for albedo data
year <- "2024"


# Create directory --------------------------------------------------------

dir.create(here("data", city), showWarnings = FALSE)

# Run baseline ------------------------------------------------------------

source(here("scenario-generation", "baseline", "CTCM-baseline-function.R"))
run_CTCM_baseline(city, aoi_file, ctcm_run = "baseline", author, utc_offset, buffer)


# Get data ----------------------------------------------------------------


library(reticulate)
use_condaenv("chri", required = TRUE)
source_python(here("get-data.py"))

# Add the Python script folder to sys.path
script_dir <- here()  
py_run_string(sprintf("import sys; sys.path.append('%s')", script_dir))
get_data(city, aoi_file, buffer, year, script_dir)


# Generate scenarios ------------------------------------------------------

# street trees achievable
source(here("scenario-generation", "street-trees", "01-street-tree-potentials.R"))
street_tree_scenario(city, scenario = "achievable", scenario_name = "achievable-90pctl", 
                     percentile = 0.9, min_tree_dist = 5)

# cool roofs program (large roofs)
source(here("scenario-generation", "cool-roofs", "01-cool-roofs-potentials.R"))
cool_roof_scenario(city, scenario = "program", scenario_name = "large-buildings", 
                   infrastructure = "cool-roofs", area_threshold = 2000, cool_roof_albedo = 0.62)

# park shade program (100m max distance to shade)
source(here("scenario-generation", "park-shade-structures", "01-park-shade-structures-small-parks-min-pct.R"))
park_shade_scenario(city, scenario_name = "program-potential", 
                    structure_size = 5, shade_pct = 0.25, spacing = 5, 
                    min_shade_area = 25)



# Run CTCM on scenarios ---------------------------------------------------

# street trees
unlink(file.path("C:", "CTCM_data_setup", paste0(city, "-street-trees-achievable-90pctl")), recursive = TRUE)
unlink(file.path("C:", "CTCM_outcome", paste0(city, "-street-trees-achievable-90pctl")), recursive = TRUE)

source(here("scenario-generation", "street-trees", "02-run-CTCM-street-trees.R"))
run_CTCM_street_trees(city, author, utc_offset, scenario_name = "achievable-90pctl", buffer)

# Cool roofs
unlink(file.path("C:", "CTCM_data_setup", paste0(city, "-cool-roofs-large-buildings")), recursive = TRUE)
unlink(file.path("C:", "CTCM_outcome", paste0(city, "-cool-roofs-large-buildings")), recursive = TRUE)

source(here("scenario-generation", "cool-roofs", "02-run-CTCM-cool-roofs.R"))
run_CTCM_cool_roofs(city, author, utc_offset, scenario_name = "large-buildings", buffer)

# Park shade
unlink(file.path("C:", "CTCM_data_setup", paste0(city, "-park-shade-structures-program-potential-0")), recursive = TRUE)
unlink(file.path("C:", "CTCM_outcome", paste0(city, "-park-shade-structures-program-potential-0")), recursive = TRUE)

unlink(file.path("C:", "CTCM_data_setup", paste0(city, "-park-shade-structures-program-potential-1")), recursive = TRUE)
unlink(file.path("C:", "CTCM_outcome", paste0(city, "-park-shade-structures-program-potential-1")), recursive = TRUE)

source(here("scenario-generation", "park-shade-structures", "02-run-CTCM-park-shade-structures.R"))
run_CTCM_park_shade_structures(city, author, utc_offset, transmissivity = 3, 
                               scenario_name = "program-potential", buffer)

run_CTCM_park_shade_structures(city, author, utc_offset, transmissivity = 0, 
                               scenario_name = "program-potential", buffer)

source(here("scenario-generation", "park-shade-structures", "03-shade-structure-post-processing.R"))
shade_structure_post_processing(city)


# Upload files to s3 -------------------------------------------------------
source_python(here("upload-data.py"))

# Add the Python script folder to sys.path
script_dir <- here()  
py_run_string(sprintf("import sys; sys.path.append('%s')", script_dir))

upload_folder_to_s3(city, aoi_name, year)


# Calculate metrics -------------------------------------------------------

source(here("utils", "calc_UTCI.R"))

# Street trees
calc_UTCI(city, scenario = "achievable-90pctl", infrastructure = "street-trees")

source(here("scenario-generation", "street-trees", "street-trees-metrics-function.R"))
calc_street_tree_metrics(city, scenario = "achievable-90pctl", infrastructure = "street-trees", aoi_name)

# Park shade
calc_UTCI(city, scenario = "program-potential", infrastructure = "park-shade-structures")

calc_park_shade_metrics(city, scenario = "program-potential", aoi_name)

# Cool roofs
calc_UTCI(city, scenario = "large-buildings", infrastructure = "cool-roofs")

source(here("scenario-generation", "cool-roofs", "cool-roofs-metrics-function.R"))
calc_cool_roofs_metrics(city, scenario = "large-buildings", cool_roof_albedo = 0.62, aoi_name)
