# Recompute UTCI/UTCIcat rasters + metrics CSVs into city_projects_CORRECT.
#
# Phase 2 of the UTCI vpd-bug fix (see the plan file for full context). This is
# a standalone script, NOT run-scenarios.R -- it does not modify run-scenarios.R,
# which remains the production driver for normal future runs.
#
# HARD SAFETY REQUIREMENT: nothing under s3://wri-cities-tcm/city_projects/ (the
# original bucket) may be modified, ever, by this script. bucket_prefix is the
# single source of truth below and is asserted before the main loop starts.

library(glue)
library(tidyverse)
library(here)
library(sf)
library(terra)

source(here("tiling-scripts", "utils.R"))
source(here("tiling-scripts", "post-processing-functions.R"))
source(here("tiling-scripts", "metrics-functions.R"))
source(here("tiling-scripts", "utci-recompute-functions.R"))

# ---------------------------------------------------------------------------
# Constants -- bucket_prefix is the ONLY place the target prefix is set.
# ---------------------------------------------------------------------------

bucket        <- "wri-cities-tcm"          # bucket NAME, required by post-processing-functions.R's
                                            # lexically-scoped global `bucket` reference
bucket_prefix <- "city_projects_CORRECT"   # single source of truth for the target prefix
aws_http      <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
s3            <- paws::s3()                # required by utils.R's write_s3()/list_s3_keys() (lexically-scoped global)

stopifnot(bucket_prefix == "city_projects_CORRECT")  # fail loudly on any accidental edit

# Login once (browser auth), then refresh on an interval during the run.
system("aws sso login --profile cities-data-dev")

refresh_credentials <- function() {
  creds <- system2("aws",
                   args = c("configure", "export-credentials", "--profile", "cities-data-dev", "--format", "env-no-export"),
                   stdout = TRUE)
  for (line in creds) {
    eq_pos <- regexpr("=", line, fixed = TRUE)
    if (eq_pos > 0) {
      key <- substr(line, 1, eq_pos - 1)
      value <- substr(line, eq_pos + 1, nchar(line))
      do.call(Sys.setenv, setNames(list(value), key))
    }
  }
  Sys.setenv(AWS_DEFAULT_REGION = "us-east-1", AWS_SDK_LOAD_CONFIG = "1")
  message("Credentials refreshed at: ", Sys.time())
}
refresh_credentials()

# ---------------------------------------------------------------------------
# The 38 AOIs needing recompute (the 42 copied to city_projects_CORRECT minus
# the 2 GPU-pipeline AOIs -- confirmed unaffected by this bug, left untouched
# entirely -- and the 2 USA-Boston placeholders, which have no UTCI/met data
# and whose metrics were already separately copied).
# ---------------------------------------------------------------------------

aois <- c(
  "ARG-Buenos_Aires/barrio_20",
  "ARG-Buenos_Aires/cildenez_padre_rodolfo_ricciardelli",
  "ARG-Buenos_Aires/urban_extent",
  "ARG-Ushuaia/urban_extent",
  "BRA-Belo_Horizonte/urban_extent",
  "BRA-Campinas/accelerator_area",
  "BRA-Campinas/urban_extent",
  "BRA-Florianopolis/accelerator_area",
  "BRA-Florianopolis/urban_extent",
  "BRA-Fortaleza/accelerator_area",
  "BRA-Fortaleza/urban_extent",
  "BRA-Recife/accelerator_area",
  "BRA-Recife/urban_extent",
  "BRA-Rio_de_Janeiro/low_emission_zone",
  "BRA-Rio_de_Janeiro/urban_extent",
  "BRA-Teresina/accelerator_area",
  "BRA-Teresina/accelerator_area_big",
  "BRA-Teresina/urban_extent",
  "CHN-Chengdu/data",
  "CHN-Chengdu/urban-analysis-area",
  "CHN-Chengdu/urban-ring-townships",
  "CHN-Chengdu/urban_extent",
  "IND-Bhopal/tt_nagar",
  "IND-Bhopal/urban_extent",
  "KEN-Nairobi/mathare_area",
  "KEN-Nairobi/urban_extent",
  "MEX-Hermosillo/centro_y_norte_triangle",
  "MEX-Hermosillo/urban_extent",
  "MEX-Monterrey/mitras_centro",
  "MEX-Monterrey/urban_extent",
  "USA-Boston/air_temp",
  "USA-Boston/east_boston",
  "ZAF-Cape_Town/business_district",
  "ZAF-Cape_Town/urban_extent",
  "ZAF-Durban/corridors_of_excellence",
  "ZAF-Durban/inner_city_lap",
  "ZAF-Johannesburg/jukskei-river",
  "ZAF-Johannesburg/urban_extent"
)
stopifnot(length(aois) == 38)

EXCLUDED_AOIS <- c(
  "KEN-Nairobi/ctcm_gpu", "IND-Bhopal/tt_nagar_gpu",
  "USA-Boston/urban_extent", "USA-Boston/urban_extent_v1_AT"
)
stopifnot(!any(aois %in% EXCLUDED_AOIS))

# ---------------------------------------------------------------------------
# Logging / resumability
# ---------------------------------------------------------------------------

log_dir             <- here("recompute_logs")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
progress_log_path   <- file.path(log_dir, "progress.csv")
date_audit_log_path <- file.path(log_dir, "date_audit.csv")

log_progress <- function(city, aoi_name, infra, scenario, status, error_msg = NA) {
  row <- tibble::tibble(
    timestamp = as.character(Sys.time()),
    city = city, aoi_name = aoi_name, infra = infra, scenario = scenario,
    status = status, error_msg = error_msg
  )
  write_header <- !file.exists(progress_log_path)
  readr::write_csv(row, progress_log_path, append = !write_header, col_names = write_header)
}

already_done <- function(city, aoi_name, infra, scenario) {
  if (!file.exists(progress_log_path)) return(FALSE)
  prog <- readr::read_csv(progress_log_path, show_col_types = FALSE)
  any(prog$city == city & prog$aoi_name == aoi_name &
        prog$infra == infra & prog$scenario == scenario & prog$status == "DONE")
}

# ---------------------------------------------------------------------------
# Discovery: find the REAL infra/scenario combos that exist for an AOI, with
# actual tcm_results + met file data -- do not assume the canonical 5 apply
# uniformly to every AOI.
# ---------------------------------------------------------------------------

list_s3_subfolders <- function(s3_url, profile = "cities-data-dev") {
  s3_url <- sub("/?$", "/", s3_url)
  lines <- system2("aws", c("s3", "ls", s3_url, "--profile", profile), stdout = TRUE, stderr = TRUE)
  m <- regmatches(lines, regexec("^[[:space:]]*PRE[[:space:]]+([^/]+)/$", lines))
  names <- vapply(m, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
  unique(stats::na.omit(names))
}

# Only these infra names are understood by the recompute/post-processing/
# metrics code paths. Anything else (dev/test scenario folders, staging
# folders, non-standard layouts -- confirmed to exist for a few AOIs, e.g.
# ARG-Ushuaia/urban_extent's "test_subarea/first_scenario") is skipped, never
# guessed at.
KNOWN_INFRA <- c("baseline", "trees", "cool-roofs", "shade-structures",
                  "cool-roofs_trees", "trees_cool-roofs")

discover_combos <- function(city, aoi_name, bucket_prefix) {
  scenarios_url <- glue("s3://{bucket}/{bucket_prefix}/{city}/{aoi_name}/scenarios/")
  infras <- list_s3_subfolders(scenarios_url)
  infras <- infras[infras != "metrics"]  # the flat metrics/ dir sits alongside infra folders

  unknown_infra <- setdiff(infras, KNOWN_INFRA)
  if (length(unknown_infra) > 0) {
    message("  [skip] non-standard infra folder(s), not recognized: ", paste(unknown_infra, collapse = ", "))
  }
  infras <- intersect(infras, KNOWN_INFRA)

  combos <- list()
  for (infra in infras) {
    scenario_names <- list_s3_subfolders(glue("{scenarios_url}{infra}/"))

    # Baseline must be the literal scenario name "baseline" -- every other
    # part of this pipeline (load_met_for_scenario, the ccl_layers/utci-*
    # __baseline__baseline.tif naming convention, process_tcm_layers'
    # baseline-folder assumption) hardcodes that exact convention. A few AOIs
    # have a baseline infra folder with a non-"baseline" scenario name (e.g.
    # BRA-Belo_Horizonte/urban_extent's "citywide") -- these don't fit the
    # pipeline's own assumptions and are skipped rather than guessed at.
    if (infra == "baseline") {
      non_standard <- setdiff(scenario_names, "baseline")
      if (length(non_standard) > 0) {
        message("  [skip] non-standard baseline scenario name(s): ", paste(non_standard, collapse = ", "))
      }
      scenario_names <- intersect(scenario_names, "baseline")
    }

    for (scenario in scenario_names) {
      scenario_folder <- glue("{bucket_prefix}/{city}/{aoi_name}/scenarios/{infra}/{scenario}")
      tiles <- list_tiles(glue("s3://{bucket}/{scenario_folder}"))
      if (length(tiles) == 0) {
        message("  [skip] ", infra, "/", scenario, " has zero tiles -- skipping")
        next
      }

      # Confirm there's at least one Tmrt file somewhere under this scenario
      # (don't care which subpath yet -- recompute_utci_for_infra_scenario
      # handles t0/t3 and met_era5_hottest_days/reduced_temps branching itself).
      scenario_keys <- list_s3_keys(bucket, glue("{scenario_folder}/"))
      has_tmrt <- any(grepl("/Tmrt_.*\\.tif$", scenario_keys))
      if (!has_tmrt) {
        message("  [skip] ", infra, "/", scenario, " has tiles but no Tmrt data -- skipping (SKIPPED_NO_DATA)")
        next
      }

      combos[[length(combos) + 1]] <- tibble::tibble(infra = infra, scenario = scenario, tiles = list(tiles))
    }
  }

  if (length(combos) == 0) return(tibble::tibble(infra = character(0), scenario = character(0), tiles = list()))
  dplyr::bind_rows(combos)
}

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------

for (idx in seq_along(aois)) {
  aoi <- aois[idx]
  city <- str_split_1(aoi, "/")[1]
  aoi_name <- str_split_1(aoi, "/")[2]

  message("\n============================================================")
  message("AOI: ", city, "/", aoi_name, "  (", idx, "/", length(aois), ")")
  message("============================================================")

  combos <- discover_combos(city, aoi_name, bucket_prefix)
  if (nrow(combos) == 0) {
    log_progress(city, aoi_name, NA, NA, "SKIPPED_NO_DATA")
    next
  }

  # Baseline must run first -- every scenario's post-processing step reads
  # baseline's ccl_layers/utci-* as its reference.
  combos <- combos %>% arrange(infra != "baseline")

  # tiles_aoi for the metrics functions = the baseline AOI's own tile list.
  baseline_row <- combos %>% filter(infra == "baseline")
  tiles_aoi <- if (nrow(baseline_row) > 0) baseline_row$tiles[[1]] else character(0)

  for (i in seq_len(nrow(combos))) {
    infra <- combos$infra[[i]]
    scenario <- combos$scenario[[i]]
    tiles <- combos$tiles[[i]]

    if (already_done(city, aoi_name, infra, scenario)) {
      message("  [resume] already DONE: ", infra, "/", scenario, " -- skipping")
      next
    }

    message("  Processing: ", infra, "/", scenario, " (", length(tiles), " tiles)")

    result <- tryCatch({
      met <- load_met_for_scenario(bucket_prefix, city, aoi_name, infra, scenario, aws_http)

      recompute_utci_for_infra_scenario(bucket_prefix, city, aoi_name, infra, scenario,
                                         tiles, met, date_audit_log_path)

      baseline_folder <- glue("{bucket_prefix}/{city}/{aoi_name}/scenarios/baseline/baseline")
      scenario_folder <- glue("{bucket_prefix}/{city}/{aoi_name}/scenarios/{infra}/{scenario}")

      if (infra == "shade-structures") {
        shade_structure_post_processing(baseline_folder, scenario_folder, infra, scenario)
      } else if (infra != "baseline") {
        process_tcm_layers(baseline_folder, infra, scenario, scenario_folder)
      }

      switch(infra,
        "baseline"          = calc_baseline_metrics(city, aoi_name, tiles_aoi, bucket_prefix = bucket_prefix),
        "trees"             = calc_street_tree_metrics(city, aoi_name, tiles_aoi, infra, scenario, bucket_prefix = bucket_prefix),
        "cool-roofs"        = calc_cool_roofs_metrics(city, aoi_name, tiles_aoi, infra, scenario, bucket_prefix = bucket_prefix),
        "shade-structures"  = calc_shade_structures_metrics(city, aoi_name, tiles_aoi, infra, scenario, bucket_prefix = bucket_prefix),
        "cool-roofs_trees"  = ,
        "trees_cool-roofs"  = calc_cool_roofs_trees_metrics(city, aoi_name, tiles_aoi, infra, scenario, bucket_prefix = bucket_prefix),
        stop("Unknown infra encountered during recompute: ", infra)
      )

      log_progress(city, aoi_name, infra, scenario, "DONE")
      TRUE
    }, error = function(e) {
      log_progress(city, aoi_name, infra, scenario, "FAILED", conditionMessage(e))
      message("  FAILED: ", city, "/", aoi_name, " ", infra, "/", scenario, " -- ", conditionMessage(e))
      FALSE
    })
  }

  if (idx %% 5 == 0) refresh_credentials()
}

message("\nAll AOIs processed. See ", progress_log_path, " and ", date_audit_log_path, " for results.")
