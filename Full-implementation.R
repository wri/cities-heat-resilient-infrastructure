library(here)

########## IGNORE ########## 
# city <- "ZAF-Cape_Town"
# aoi_file <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_Town/test-aoi.geojson"
# aoi_name <- "test"
########## ########## #####

# Define parameters -------------------------------------------------------

# City parameters
city <- "BRA-Rio_de_Janeiro"
  
# AOI parameters
# URL for file in S3
aoi_file <- "https://wri-cities-heat.s3.us-east-1.amazonaws.com/BRA-Rio_de_janeiro/raw/boundaries/BRA-Rio_de_janeiro-DBE_low_emission_zone.geojson"
aoi_name <- "low-emission-zone"
  
# CTCM metadata
author <- "elizabeth.wesley@wri.org" # User email
utc_offset <- 2 # UTC offset for the city

# AOI buffer
buffer <- 100

# Year for albedo data
year <- "2024"


# Create directory --------------------------------------------------------

dir.create(here("data", city))

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
source(here("scenario-generation", "park-shade-structures", "01-park-shade-structures-max-distance.R"))
park_shade_scenario(city, scenario_name = "program-potential", 
                    structure_size = 5, shade_pct = 0.25, spacing = 5, 
                    min_shade_area = 25, max_dist_to_shade = 100)



# Run CTCM on scenarios ---------------------------------------------------

# street trees
source(here("scenario-generation", "street-trees", "02-run-CTCM-street-trees.R"))
run_CTCM_street_trees(city, author, utc_offset, scenario_name = "achievable-90pctl")

# Cool roofs
source(here("scenario-generation", "cool-roofs", "02-run-CTCM-cool-roofs.R"))
run_CTCM_cool_roofs(city, author, utc_offset, scenario_name = "large-buildings")

# Park shade
source(here("scenario-generation", "park-shade-structures", "02-run-CTCM-park-shade-structures.R"))
run_CTCM_park_shade_structures(city, author, utc_offset, transmissivity = 3, 
                               scenario_name = "program-potential")

unlink(file.path("C:", "CTCM_data_setup", paste0(city, "-park-shade-structures-", "program-potential")), 
       recursive = TRUE, force = TRUE)
unlink(file.path("C:", "CTCM_outcome", paste0(city, "-park-shade-structures-", "program-potential")), 
       recursive = TRUE, force = TRUE)

run_CTCM_park_shade_structures(city, author, utc_offset, transmissivity = 0, 
                               scenario_name = "program-potential")

source(here("scenario-generation", "park-shade-structures", "03-shade-structure-post-processing.R"))
shade_structure_post_processing()


# Calculate metrics -------------------------------------------------------


