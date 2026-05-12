# Login once (browser auth)
system("aws sso login --profile cities-data-dev")

# Function to pull fresh temp credentials from the SSO token
refresh_credentials <- function() {
  creds <- system2("aws",
                   args = c("configure", "export-credentials", "--profile", "cities-data-dev", "--format", "env-no-export"),
                   stdout = TRUE)
  
  for (line in creds) {
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) == 2) do.call(Sys.setenv, setNames(list(parts[2]), parts[1]))
  }
  Sys.setenv(AWS_DEFAULT_REGION = "us-east-1", AWS_SDK_LOAD_CONFIG = "1")
  message("Credentials refreshed at: ", Sys.time())
}

refresh_credentials()

# Then call refresh_credentials() between major steps in your script

library(glue)
library(tidyverse)
library(here)
library(sf)
library(terra)

source(here("tiling-scripts", "metrics-functions.R"))
source(here("tiling-scripts", "utils.R"))
source(here("tiling-scripts", "post-processing-functions.R"))

cities_to_run <- tribble(
  ~c, ~aoi_name, ~shade,
  "BRA-Rio_de_Janeiro", "low_emission_zone", FALSE,
  "MEX-Monterrey", "mitras_centro", FALSE,
  "BRA-Teresina", "accelerator_area_big", FALSE,
  "ARG-Buenos_Aires", "barrio_20", TRUE,
  "ZAF-Johannesburg", "jukskei-river", FALSE,
  "ZAF-Cape_Town", "business_district", TRUE,
  "IND-Bhopal", "tt_nagar", FALSE,
  "BRA-Campinas", "accelerator_area", FALSE,
  "BRA-Florianopolis", "accelerator_area", FALSE,
  "BRA-Fortaleza", "accelerator_area", FALSE,
  "BRA-Recife", "accelerator_area", FALSE
)

s <- tribble(
  ~infra,                  ~scenario,
  "baseline",              "baseline",
  "trees",                 "pedestrian-achievable-90pctl",
  "cool-roofs_trees",      "all-buildings_pedestrian-achievable-90pctl",
  "shade-structures",      "all-parks",
  "cool-roofs",            "all-buildings"
)


source(here("tiling-scripts", "metrics-functions.R"))

s3 <- paws::s3()

for (city in cities_to_run$c){
  refresh_credentials()
    # print(city)
    # city <- "ZAF-Cape_Town"
    # aoi_name <- "business_district"
    aoi_name <- cities_to_run |> 
      filter(c == city) |> 
      pull(aoi_name)
    
    # shade <- cities_to_run |> 
    #   filter(c == city) |> 
    #   pull(shade)
    
    bucket   <- "wri-cities-tcm"
    aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
    # open_urban_aws_http <- paste0("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/OpenUrban/", city)
    
    # country <- strsplit(city, "-")[[1]][1]
    
    city_folder     <- file.path("city_projects", city, aoi_name)
    # baseline_folder <- file.path(city_folder, "scenarios", "baseline", "baseline")
    
    for (i in seq_len(nrow(s))) {
      
      infra <- s$infra[i]
      scenario <- s$scenario[i]
      
      csv_url <- glue("{aws_http}/city_projects/{city}/{aoi_name}/scenarios/metrics/metrics__{infra}__{scenario}.csv")
      
      metrics <- tryCatch(
        read_csv(csv_url, show_col_types = FALSE),
        error = function(e) NULL
      )
      
      if (is.null(metrics)) {
        message(glue("SKIP: {city} | {infra} | {scenario} — not found"))
        next
      }
      
      metrics <- metrics |>
        mutate(indicators_id = str_replace_all(indicators_id, "pct__", "pct_"))
      
      
      write_s3(metrics, glue("wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/metrics/metrics__{infra}__{scenario}.csv"))
    }
    
    # aoi_path <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson")
    # 
    # tiles_s3 <- list_tiles(paste0("s3://", bucket, "/", baseline_folder))
    # 
    # tile_grid <- st_read(
    #   paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/unbuffered_tile_grid.geojson"),
    #   quiet = TRUE
    # ) |>
    #   filter(tile_name %in% tiles_s3)
    # 
    # buffered_tile_grid <- st_read(
    #   paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/tile_grid.geojson"),
    #   quiet = TRUE
    # ) %>%
    #   dplyr::filter(tile_name %in% tiles_s3)
    # 
    # utm <- st_crs(tile_grid)
    # 
    # aoi <- st_read(aoi_path, quiet = TRUE) |>
    #   st_transform(st_crs(utm))
    # 
    # tile_grid_aoi <- tile_grid |> 
    #   st_filter(aoi) 
    # tiles_aoi <- tile_grid_aoi$tile_name
    
    # infra <- "trees"
    # scenario <- "pedestrian-achievable-90pctl"
    
    # infra <- "shade-structures"
    # scenario <- "all-parks"
    
    # infra <- "cool-roofs_trees"
    # scenario <- "all-buildings_pedestrian-achievable-90pctl"
    # scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
    # print(glue("{city} baseline"))
    # calc_baseline_metrics(city, aoi_name, tiles_aoi)
    # 
    # print(glue("{city} street trees"))
    # calc_street_tree_metrics(city, aoi_name, tiles_aoi, infra = "trees", scenario = "pedestrian-achievable-90pctl")
    # 
    # print(glue("{city} cool roofs"))
    # calc_cool_roofs_metrics(city, aoi_name, tiles_aoi, infra = "cool-roofs", scenario = "all-buildings")
    # 
    # if (shade) {
    #   print(glue("{city} shade"))
    #   calc_shade_structures_metrics(city, aoi_name, tiles_aoi, infra = "shade-structures", scenario = "all-parks")
    # }
    #  
    # print(glue("{city} combo"))
    # calc_cool_roofs_trees_metrics(city, aoi_name, tiles_aoi, infra = "cool-roofs_trees", 
    #                               scenario = "all-buildings_pedestrian-achievable-90pctl")
  
  
}


