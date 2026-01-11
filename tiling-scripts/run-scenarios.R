#!/usr/bin/env Rscript
# run-scenarios.R
#
# Plan syntax:
#   CITY:scenario[flags],scenario[flags];CITY2:scenario[flags]
# Flags:
#   g = generate scenario data
#   d = download data
#   c = run CTCM
#   u = upload CTCM data 
#
# Example:
# EC2_TERMINATE_ON_COMPLETE=true Rscript run-scenarios.R \
#   --plan "BRA-Recife:trees[gdcu],cool-roofs[dcu],shade-structures[gdcu];BRA-Campinas:trees[cu]" \
#   --aoi_name accelerator_area \
#   --aoi_path "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson" \
#   --copy_from_extent false

suppressPackageStartupMessages({
  library(optparse)
  library(here)
  library(terra)
  library(sf)
  library(tidyverse)
  library(glue)
})

# -----------------------------
# CLI
# -----------------------------
option_list <- list(
  make_option("--plan", type = "character",
              help = "Per-city plan: CITY:scenario[gdcu],scenario[dcu];CITY2:scenario[cu] (required)"),
  make_option("--aoi_name", type = "character",
              help = "AOI name, e.g. accelerator_area (required)"),
  make_option("--aoi_path", type = "character",
              help = "AOI GeoJSON URL template. Use {city} and {aoi_name} (required)"),
  make_option("--copy_from_extent", type = "character",
              default = "false",
              help = "true/false. Copy baseline tiles from urban_extent (default: false)")
)

opts <- parse_args(OptionParser(option_list = option_list))

required <- c("plan", "aoi_name", "aoi_path")
missing <- required[!nzchar(unlist(opts[required]))]
if (length(missing) > 0) stop("Missing required argument(s): ", paste(missing, collapse = ", "))

aoi_name <- opts$aoi_name
aoi_path_template <- opts$aoi_path
copy_from_extent <- tolower(opts$copy_from_extent) %in% c("true", "t", "1", "yes", "y")

# -----------------------------
# Constants
# -----------------------------
bucket   <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
s3 <- paws::s3()

source(here("tiling-scripts", "utils.R"))

# -----------------------------
# Plan parser
# -----------------------------
parse_plan <- function(plan_str) {
  # returns: list(city = list(scenario = list(generate=, download=, ctcm=), ...), ...)
  # plan_str example:
  # "BRA-Recife:trees[gdcu],cool-roofs[dcu];BRA-Campinas:trees[cu]"
  plan_str <- trimws(plan_str)
  if (!nzchar(plan_str)) stop("--plan is empty")
  
  cities_part <- strsplit(plan_str, ";", fixed = TRUE)[[1]]
  cities_part <- trimws(cities_part)
  cities_part <- cities_part[nzchar(cities_part)]
  
  plan <- list()
  
  for (cp in cities_part) {
    # CITY:...
    sp <- strsplit(cp, ":", fixed = TRUE)[[1]]
    if (length(sp) < 2) stop("Bad city plan (missing ':'): ", cp)
    city <- trimws(sp[1])
    rest <- paste(sp[-1], collapse = ":")
    rest <- trimws(rest)
    
    scen_parts <- strsplit(rest, ",", fixed = TRUE)[[1]]
    scen_parts <- trimws(scen_parts)
    scen_parts <- scen_parts[nzchar(scen_parts)]
    
    scen_list <- list()
    
    for (sc in scen_parts) {
      # scenario[flags]
      m <- regexec("^([^\\[]+)\\[([a-zA-Z]+)\\]$", sc)
      r <- regmatches(sc, m)[[1]]
      if (length(r) == 0) stop("Bad scenario spec (expected scenario[flags]): ", sc)
      
      scenario <- trimws(r[2])
      flags <- tolower(r[3])
      
      scen_list[[scenario]] <- list(
        generate = grepl("g", flags, fixed = TRUE),
        download = grepl("d", flags, fixed = TRUE),
        ctcm     = grepl("c", flags, fixed = TRUE),
        upload  = grepl("u", flags, fixed = TRUE)
      )
    }
    
    plan[[city]] <- scen_list
  }
  
  plan
}

plan <- parse_plan(opts$plan)

message("Run plan:")
print(plan)

# -----------------------------
# Helpers
# -----------------------------
build_aoi_path <- function(city) {
  p <- aoi_path_template
  p <- gsub("\\{city\\}", city, p)
  p <- gsub("\\{aoi_name\\}", aoi_name, p)
  p
}

# -----------------------------
# Main loop: city -> scenarios
# -----------------------------
for (city in names(plan)) {
  
  message("\n============================================================")
  message("CITY: ", city)
  message("============================================================\n")
  
  open_urban_aws_http <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/", city)
  
  # derive aoi path
  aoi_path <- build_aoi_path(city)
  
  # folder structure
  city_folder     <- file.path("city_projects", city, aoi_name)
  baseline_folder <- file.path(city_folder, "scenarios", "baseline", "baseline")
  
  # tiles
  tiles_s3 <- list_tiles(paste0("s3://", bucket, "/", baseline_folder))
  if (length(tiles_s3) == 0) {
    warning("No tiles found for city ", city, " at ", baseline_folder, " — skipping city.")
    next
  }
  
  # grids
  buffered_tile_grid <- st_read(
    paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/tile_grid.geojson"),
    quiet = TRUE
  ) |>
    filter(tile_name %in% tiles_s3)
  
  tile_grid <- st_read(
    paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/unbuffered_tile_grid.geojson"),
    quiet = TRUE
  ) |>
    filter(tile_name %in% tiles_s3)
  
  # AOI
  aoi <- st_read(aoi_path, quiet = TRUE) |>
    st_transform(st_crs(tile_grid))
  
  # Tiles intersecting AOI (kept, though you still use tiles_s3 downstream) 
  buffered_tile_grid_aoi <- buffered_tile_grid |> 
    st_filter(aoi) 
  tile_grid_aoi <- tile_grid |> 
    st_filter(aoi) 
  tiles_aoi <- buffered_tile_grid_aoi$tile_name
  
  # optional baseline copy
  if (copy_from_extent) {
    tile_ids <- tile_grid |> st_filter(aoi) |> pull(tile_name)
    for (t in tile_ids) {
      from <- file.path("city_projects", city, "urban_extent", "baseline", "baseline", t)
      to   <- file.path(baseline_folder, t)
      ensure_s3_prefix(bucket, to)
      s3_copy_vec(from = from, to = to, bucket = bucket)
    }
  }
  
  # run each scenario requested for this city
  for (scenario_name in names(plan[[city]])) {
    
    steps <- plan[[city]][[scenario_name]]
    message("Scenario: ", scenario_name,
            "  [generate=", steps$generate,
            ", download=", steps$download,
            ", ctcm=", steps$ctcm, 
            ", ctcm=", steps$upload, "]")
    
    # ------------------ baseline ------------------
    if (scenario_name == "baseline") {
      if (steps$generate) {
        source(here("tiling-scripts", "baseline-layers.R"))
        save_baseline_layers()
      }
      next
    }
    
    # ------------------ trees ------------------
    if (scenario_name == "trees") {
      infra    <- "trees"
      scenario <- "pedestrian-achievable-90pctl"
      scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
      
      source(here("tiling-scripts", "trees-functions.R"))
      source(here("tiling-scripts", "CTCM-functions.R"))
      source(here("tiling-scripts", "post-processing-functions.R"))
      
      if (steps$generate) run_tree_scenario()
      if (steps$download) {
        download_tree_data(
          city            = city,
          infra           = infra,
          scenario        = scenario,
          baseline_folder = baseline_folder,
          scenario_folder = scenario_folder,
          tiles           = tiles_s3
        )
      }
      if (steps$ctcm) {
        run_tree_CTCM(city, infra, scenario, aoi_name)
      }
      if (steps$upload){
        upload_tcm_layers(city, infra, scenario, aoi_name)
        process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
      }
      
      next
    }
    
    # ------------------ cool roofs ------------------
    if (scenario_name == "cool-roofs") {
      infra   <- "cool-roofs"
      country <- strsplit(city, "-")[[1]][1]
      
      source(here("tiling-scripts", "cool-roofs-functions.R"))
      source(here("tiling-scripts", "CTCM-functions.R"))
      source(here("tiling-scripts", "post-processing-functions.R"))
      
      if (steps$generate) update_albedo()
      
      if (steps$download) {
        # whatever your “download” means for cool roofs:
        # - calc delta rasters (if needed)
        for (s in c("all-buildings", "large-buildings")) {
          calc_air_temp_delta(city, s, aoi)
        }
        download_cool_roof_data(city, aoi_name, scenario = "all-buildings", baseline_folder, tiles_s3)
      }
      
      if (steps$ctcm) {
        scenario <- "all-buildings"
        scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
        
        run_cool_roof_CTCM(city, infra, scenario, aoi_name = aoi_name)
      }
      if (steps$upload){
        upload_tcm_layers(city, infra, scenario, aoi_name)
        process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
      }
      
      next
    }
    
    # ------------------ shade structures ------------------
    if (scenario_name == "shade-structures") {
      infra    <- "shade-structures"
      scenario <- "all-parks"
      scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
      
      # IMPORTANT: define scenario_folder before source() (your earlier issue)
      source(here("tiling-scripts", "park-shade-functions.R"))
      source(here("scenario-generation", "park-shade-structures", "shade-generating-functions.R"))
      source(here("tiling-scripts", "CTCM-functions.R"))
      
      if (steps$generate) run_shade_scenario()
      
      if (steps$download) {
        scenario_tiles <- list_tiles(paste0("s3://", bucket, "/", scenario_folder))
        download_shade_data(
          city, infra, scenario,
          baseline_folder, scenario_folder,
          tiles = scenario_tiles,
          transmissivity = 3
        )
      }
      
      if (steps$ctcm) {
        # You’ll need to ensure these vars exist or are passed:
        # author, utc_offset, buffer, etc.
        run_CTCM_shade_structures(city_folder, author, utc_offset, transmissivity = 3,
                                  scenario_name = "program-potential", buffer)
        run_CTCM_shade_structures(city_folder, author, utc_offset, transmissivity = 0,
                                  scenario_name = "program-potential", buffer)
      }
      if (steps$upload){
        upload_tcm_layers(city, infra, scenario, aoi_name)
        process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
      }
      
      next
    }
    
    stop("Unknown scenario name in plan: ", scenario_name)
  }
  
  message("\nFinished city: ", city)
}

message("\nAll cities complete.")

# Optional: terminate instance when finished
if (Sys.getenv("EC2_TERMINATE_ON_COMPLETE") == "true") {
  message("Terminating EC2 instance...")
  system("sudo shutdown -h now")
}
