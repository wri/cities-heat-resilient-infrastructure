#!/usr/bin/env Rscript
# run-scenarios.R
#
# Plan syntax (block-style):
#
#   CITY@AOI|aoi=DEFAULT_OR_URL:
#     infra:scenario[flags],
#     infra:scenario[flags];
#   CITY2@AOI2|aoi="https://.../aoi.geojson":
#     infra:scenario[flags]
#
# Notes:
# - Each CITY@AOI block has exactly ONE AOI path spec (aoi=...).
# - aoi=DEFAULT uses --default_aoi_path_template (required).
# - aoi can be quoted with "..." or '...' to make it shell-friendly.
# - {CITY} and {AOI} placeholders are supported in BOTH DEFAULT template and custom aoi URLs.
#
# Flags:
#   g = generate scenario data
#   d = download data
#   c = run CTCM
#   u = upload CTCM data
#
# Example:
# EC2_TERMINATE_ON_COMPLETE=true Rscript run-scenarios.R \
#   --plan 'BRA-Campinas@accelerator_area|aoi=DEFAULT:
#             trees:pedestrian-achievable-90pctl[gdcu],
#             cool-roofs:all-buildings[dcu],
#             shade-structures:all-parks[gdcu];
#           ZAF-Cape_Town@corridors-of-excellence|aoi="DEFAULT":
#             trees:custom-scenario[gdcu]' \
#   --copy_baseline false

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
              help = "Block-style plan (required). See header in script."),
  make_option("--default_aoi_path_template", type = "character",
              default = "https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{CITY}/{AOI}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson",
              help = "Template used when a block specifies aoi=DEFAULT. Supports {CITY} and {AOI}. (required)"),
  make_option("--copy_baseline", type = "character",
              default = "false",
              help = "true/false. Copy baseline tiles from urban_extent (default: false)")
)

opts <- parse_args(OptionParser(option_list = option_list))

required <- c("plan")
missing <- required[!nzchar(unlist(opts[required]))]
if (length(missing) > 0) stop("Missing required argument(s): ", paste(missing, collapse = ", "))

default_aoi_path_template <- opts$default_aoi_path_template

copy_baseline_raw <- trimws(opts$copy_baseline)
copy_baseline <- !tolower(copy_baseline_raw) %in% c("false", "f", "0", "no", "n")
baseline_aoi_name <- if (copy_baseline) copy_baseline_raw else NULL


# -----------------------------
# Constants / deps
# -----------------------------
bucket   <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
s3 <- paws::s3()

source(here("tiling-scripts", "utils.R"))
source(here("tiling-scripts", "CTCM-functions.R"))
source(here("tiling-scripts", "post-processing-functions.R"))
source(here("tiling-scripts", "metrics-functions.R"))

# -----------------------------
# Helpers
# -----------------------------
strip_outer_quotes <- function(x) {
  x <- trimws(x)
  if (grepl('^".*"$', x)) return(sub('^"(.*)"$', "\\1", x))
  if (grepl("^'.*'$", x)) return(sub("^'(.*)'$", "\\1", x))
  x
}

resolve_aoi_path <- function(city, aoi_name, aoi_spec, default_template) {
  aoi_spec <- strip_outer_quotes(aoi_spec)
  tpl <- if (toupper(trimws(aoi_spec)) == "DEFAULT") default_template else aoi_spec
  tpl <- gsub("\\{CITY\\}", city, tpl)
  tpl <- gsub("\\{AOI\\}", aoi_name, tpl)
  tpl
}

# -----------------------------
# Plan parser (block-style)
# -----------------------------
parse_plan_blocks <- function(plan_str) {
  # Returns a tibble with one row per task:
  # city, aoi_name, aoi_spec, infra, scenario, generate, download, ctcm, upload
  plan_str <- trimws(plan_str)
  if (!nzchar(plan_str)) stop("--plan is empty")
  
  # Split blocks on ';' (assumes ';' not used inside AOI URLs)
  blocks <- strsplit(plan_str, ";", fixed = TRUE)[[1]] %>% trimws()
  blocks <- blocks[nzchar(blocks)]
  if (length(blocks) == 0) stop("No CITY@AOI blocks found in --plan")
  
  out <- list()
  block_id <- 0L
  
  for (b in blocks) {
    b <- trimws(b)
    block_id <- block_id + 1L
    
    # Expect: CITY@AOI|aoi=SPEC: <tasks>
    # Allow whitespace/newlines in tasks.
    m <- regexec("^([^@\\s]+)@([^|\\s:]+)\\s*\\|\\s*aoi\\s*=\\s*([^:]+)\\s*:\\s*([\\s\\S]*)$", b, perl = TRUE)
    r <- regmatches(b, m)[[1]]
    if (length(r) == 0) {
      stop(
        "Bad block header. Expected CITY@AOI|aoi=DEFAULT_OR_URL: ...\nGot:\n", b
      )
    }
    
    city     <- trimws(r[2])
    aoi_name <- trimws(r[3])
    aoi_spec <- trimws(r[4])
    body     <- trimws(r[5])
    
    if (!nzchar(city) || !nzchar(aoi_name) || !nzchar(aoi_spec)) {
      stop("Empty city/aoi/aoi= in block:\n", b)
    }
    if (!nzchar(body)) stop("No tasks found under block header for ", city, "@", aoi_name)
    
    # Remove newlines to simplify comma splitting
    body_one_line <- gsub("[\r\n]+", " ", body)
    task_strs <- strsplit(body_one_line, ",", fixed = TRUE)[[1]] %>% trimws()
    task_strs <- task_strs[nzchar(task_strs)]
    if (length(task_strs) == 0) stop("No tasks parsed for ", city, "@", aoi_name)
    
    for (tk in task_strs) {
      # infra:scenario[flags]
      m2 <- regexec("^([^:]+):([^\\[]+)\\[([A-Za-z]+)\\]$", tk)
      r2 <- regmatches(tk, m2)[[1]]
      if (length(r2) == 0) {
        stop("Bad task format. Expected infra:scenario[flags]. Got: ", tk,
             "\nIn block: ", city, "@", aoi_name)
      }
      
      infra    <- trimws(r2[2])
      scenario <- trimws(r2[3])
      flags    <- tolower(r2[4])
      
      out[[length(out) + 1]] <- tibble::tibble(
        block_id = block_id,
        city     = city,
        aoi_name = aoi_name,
        aoi_spec = aoi_spec,
        infra    = infra,
        scenario = scenario,
        generate = grepl("g", flags, fixed = TRUE),
        download = grepl("d", flags, fixed = TRUE),
        ctcm     = grepl("c", flags, fixed = TRUE),
        upload   = grepl("u", flags, fixed = TRUE)
      )
    }
  }
  
  dplyr::bind_rows(out)
}

tasks_df <- parse_plan_blocks(opts$plan) %>%
  mutate(
    aoi_path = purrr::pmap_chr(
      list(city, aoi_name, aoi_spec),
      ~ resolve_aoi_path(..1, ..2, ..3, default_template = default_aoi_path_template)
    )
  )

message("Run tasks:")
print(tasks_df)

# Group by CITY@AOI and keep plan order
groups <- tasks_df %>%
  arrange(block_id) %>%
  group_by(block_id, city, aoi_name, aoi_path) %>%
  group_split()

message("Group execution order:")
for (g in groups) message("  ", g$block_id[[1]], ": ", g$city[[1]], " @ ", g$aoi_name[[1]])


# -----------------------------
# Main loop: (city,aoi) -> tasks
# -----------------------------
for (g in groups) {
  
  city     <- g$city[[1]]
  aoi_name <- g$aoi_name[[1]]
  aoi_path <- g$aoi_path[[1]]
  
  message("\n============================================================")
  message("CITY: ", city)
  message("AOI : ", aoi_name)
  message("AOI PATH: ", aoi_path)
  message("============================================================\n")
  
  open_urban_aws_http <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/", city)
  
  # folder structure
  city_folder     <- file.path("city_projects", city, aoi_name)
  baseline_folder <- file.path(city_folder, "scenarios", "baseline", "baseline")
  
  # optional baseline copy
  if (copy_baseline) {
    
    # read tile grid from the BASELINE AOI you are copying from
    buffered_tile_grid <- st_read(
      paste0(
        aws_http, "/city_projects/", city, "/", baseline_aoi_name,
        "/scenarios/baseline/baseline/metadata/.qgis_data/tile_grid.geojson"
      ),
      quiet = TRUE
    )
    
    aoi <- st_read(aoi_path, quiet = TRUE) %>%
      st_transform(st_crs(buffered_tile_grid))
    
    tile_ids <- buffered_tile_grid %>% st_filter(aoi) %>% dplyr::pull(tile_name)
    
    from_base <- file.path("city_projects", city, baseline_aoi_name, "scenarios", "baseline", "baseline")
    to_base   <- file.path("city_projects", city, aoi_name,         "scenarios", "baseline", "baseline")
    
    ensure_s3_prefix(bucket, to_base)
    
    src_base <- sprintf("s3://%s/%s/", bucket, sub("^/+", "", from_base))
    dst_base <- sprintf("s3://%s/%s/", bucket, sub("^/+", "", to_base))
    
    # 1) Copy everything EXCEPT the AOI geojson and EXCEPT all tiles
    message("SYNC (non-tile baseline) ", src_base, " -> ", dst_base)
    system2("aws", c(
      "s3", "sync",
      src_base, dst_base,
      "--exclude", "aoi__baseline__baseline.geojson",
      "--exclude", "tile_*/*",
      "--only-show-errors"
    ))
    
    # 2) Copy only the tiles we want
    tile_ids <- unique(as.character(tile_ids))
    for (t in tile_ids) {
      from_prefix <- file.path(from_base, t)
      to_prefix   <- file.path(to_base,   t)
      
      ensure_s3_prefix(bucket, to_prefix)
      
      src <- sprintf("s3://%s/%s/", bucket, sub("^/+", "", from_prefix))
      dst <- sprintf("s3://%s/%s/", bucket, sub("^/+", "", to_prefix))
      
      message("SYNC (tile) ", src, " -> ", dst)
      system2("aws", c(
        "s3", "sync",
        src, dst,
        "--only-show-errors"
      ))
    }
  }
  
  
  # tiles
  tiles_s3 <- list_tiles(paste0("s3://", bucket, "/", baseline_folder))
  if (length(tiles_s3) == 0) {
    warning("No tiles found for city ", city, " at ", baseline_folder, " — skipping group.")
    next
  }
  
  # grids
  buffered_tile_grid <- st_read(
    paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/tile_grid.geojson"),
    quiet = TRUE
  ) %>%
    dplyr::filter(tile_name %in% tiles_s3)
  
  tile_grid <- st_read(
    paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/unbuffered_tile_grid.geojson"),
    quiet = TRUE
  ) %>%
    dplyr::filter(tile_name %in% tiles_s3)
  
  utm <- st_crs(tile_grid)
  
  # AOI
  aoi <- st_read(aoi_path, quiet = TRUE) %>%
    st_transform(utm)
  
  # Tiles intersecting AOI
  buffered_tile_grid_aoi <- buffered_tile_grid %>%
    st_filter(aoi)
  tile_grid_aoi <- tile_grid %>%
    st_filter(aoi)
  tiles_aoi <- tile_grid_aoi$tile_name
  
  # run each task requested for this (city,aoi)
  for (i in seq_len(nrow(g))) {
    
    infra    <- g$infra[[i]]
    scenario <- g$scenario[[i]]
    steps <- list(
      generate = g$generate[[i]],
      download = g$download[[i]],
      ctcm     = g$ctcm[[i]],
      upload   = g$upload[[i]]
    )
    
    message("Task: ", infra, ":", scenario,
            "  [g=", steps$generate,
            ", d=", steps$download,
            ", c=", steps$ctcm,
            ", u=", steps$upload, "]")
    
    # ------------------ baseline ------------------
    if (infra == "baseline" && scenario == "baseline") {
      if (steps$generate) {
        source(here("tiling-scripts", "baseline-layers.R"))
        save_baseline_layers(utm)
        calc_baseline_metrics(city, aoi_name, tiles_aoi = tiles_aoi)
      }
      next
    }
    
    # ------------------ trees ------------------
    if (infra == "trees") {
      
      scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
      
      source(here("tiling-scripts", "trees-functions.R"))
      
      if (steps$generate) run_tree_scenario()
      
      if (steps$download) {
        download_tree_data(
          city            = city,
          infra           = infra,
          scenario        = scenario,
          baseline_folder = baseline_folder,
          scenario_folder = scenario_folder,
          tiles           = tiles_aoi
        )
      }
      
      if (steps$ctcm) {
        run_tree_CTCM(city, infra, scenario, aoi_name)
      }
      
      if (steps$upload) {
        upload_tcm_layers(city, infra, scenario, aoi_name)
        process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
        calc_street_tree_metrics(city, aoi_name, tiles_aoi, scenario)
      }
      
      next
    }
    
    # ------------------ cool roofs ------------------
    if (infra == "cool-roofs") {
      
      country <- strsplit(city, "-")[[1]][1]
      
      scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
      
      source(here("tiling-scripts", "cool-roofs-functions.R"))
      
      if (steps$generate) update_albedo()
      
      if (steps$download) {
        download_cool_roof_data(
          city            = city,
          aoi_name        = aoi_name,
          scenario        = scenario,
          baseline_folder = baseline_folder,
          tiles           = tiles_aoi
        )
      }
      
      if (steps$ctcm) {
        run_cool_roof_CTCM(city, infra, scenario, aoi_name = aoi_name)
      }
      
      if (steps$upload) {
        upload_tcm_layers(city, infra, scenario, aoi_name)
        process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
      }
      
      next
    }
    
    # ------------------ shade structures ------------------
    if (infra == "shade-structures") {
      
      scenario_folder <- file.path(city_folder, "scenarios", infra, scenario)
      
      source(here("tiling-scripts", "park-shade-functions.R"))
      source(here("scenario-generation", "park-shade-structures", "shade-generating-functions.R"))
      
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
        # Ensure these exist in your environment or wire them into CLI / plan:
        # author, utc_offset, buffer, transmissivity, etc.
        run_CTCM_shade_structures(city_folder, author, utc_offset, transmissivity = 3,
                                  scenario_name = scenario, buffer)
        run_CTCM_shade_structures(city_folder, author, utc_offset, transmissivity = 0,
                                  scenario_name = scenario, buffer)
      }
      
      if (steps$upload) {
        upload_tcm_layers(city, infra, scenario, aoi_name)
        process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
      }
      
      next
    }
    
    stop("Unknown infra in plan: ", infra)
  }
  
  message("\nFinished group: ", city, " @ ", aoi_name)
}

message("\nAll groups complete.")

# Optional: terminate instance when finished
if (Sys.getenv("EC2_TERMINATE_ON_COMPLETE") == "true") {
  message("Terminating EC2 instance...")
  system("sudo shutdown -h now")
}
