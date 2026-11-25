# Surgical UTCI/UTCIcat recompute, targeting city_projects_CORRECT only.
#
# Reuses create_utci()/utci_risk_cat() from utils/utci.R unchanged. Deliberately
# does NOT call save_baseline_layers() or upload_tcm_layers() (tiling-scripts/
# baseline-layers.R, CTCM-functions.R) -- both hardcode the "city_projects"
# bucket prefix internally with no override, and upload_tcm_layers() additionally
# expects a freshly-completed local CTCM run directory, which does not apply here
# (Tmrt/Shadow are already correct and already present in city_projects_CORRECT;
# only the downstream UTCI/vpd math needs to be redone).
#
# Every function here takes bucket_prefix explicitly -- never defaults to
# "city_projects" -- so this module cannot silently write to the original bucket.

library(terra)
library(tidyverse)
library(glue)
library(lubridate)

source(here::here("tiling-scripts", "utils.R"))
source(here::here("utils", "utci.R"))

# ---------------------------------------------------------------------------
# Date helpers (mandatory per-tile date-match verification)
# ---------------------------------------------------------------------------

# Parses the "{year}_{doy}" stamp out of a Tmrt_{year}_{doy}_{time}D.tif
# filename, matching the exact convention used elsewhere in the pipeline
# (utils.R::find_shadow_stamp()'s regex, and baseline-layers.R's Tmrt_{stamp}
# naming).
extract_date_from_tmrt_filename <- function(path) {
  fname <- basename(path)
  m <- regmatches(
    fname,
    regexpr("(?<=Tmrt_)[0-9]{4}_[0-9]{1,3}(?=_[0-9]{3,4}D\\.tif$)", fname, perl = TRUE)
  )
  if (length(m) == 0 || !nzchar(m)) {
    stop("Could not parse date stamp out of Tmrt filename: ", fname, call. = FALSE)
  }
  m
}

# Derives the same "{year}_{doy}" token from a met_data tibble, computed
# exactly the same way CTCM-functions.R::upload_tcm_layers() already does
# (Year/Month/Day -> yday), so the two tokens are directly comparable.
derive_date_from_met <- function(met_data) {
  year  <- met_data$Year  %>% unique()
  month <- met_data$Month %>% as.integer() %>% str_pad(2, pad = "0") %>% unique()
  day   <- met_data$Day   %>% as.integer() %>% str_pad(2, pad = "0") %>% unique()

  if (length(year) != 1 || length(month) != 1 || length(day) != 1) {
    stop("Met file does not resolve to a single unambiguous date (Year/Month/Day not unique).",
         call. = FALSE)
  }

  date_ymd <- glue("{year}-{month}-{day}")
  # as.character(): glue() returns a "glue"-classed object, which identical()
  # would never treat as equal to the plain-character token extracted from a
  # Tmrt filename even when the text matches -- plain character avoids that.
  as.character(glue("{year}_{yday(date_ymd)}"))
}

# ---------------------------------------------------------------------------
# Met file loading (user-specified rule, zero ambiguity)
# ---------------------------------------------------------------------------

# Baseline, trees, and shade-structures scenarios ALWAYS use the baseline
# scenario's own met_era5_hottest_days.csv -- never a per-scenario copy of it,
# even if one happens to exist, to remove any ambiguity about which copy is
# authoritative. cool-roofs / cool-roofs_trees (and alias trees_cool-roofs)
# use their OWN scenario's reduced_temps.csv instead.
load_met_for_scenario <- function(bucket_prefix, city, aoi_name, infra, scenario,
                                   aws_http = "https://wri-cities-tcm.s3.us-east-1.amazonaws.com") {

  is_reduced_temps <- infra %in% c("cool-roofs", "cool-roofs_trees", "trees_cool-roofs")

  met_url <- if (is_reduced_temps) {
    glue("{aws_http}/{bucket_prefix}/{city}/{aoi_name}/scenarios/{infra}/{scenario}/metadata/met_files/reduced_temps.csv")
  } else {
    glue("{aws_http}/{bucket_prefix}/{city}/{aoi_name}/scenarios/baseline/baseline/metadata/met_files/met_era5_hottest_days.csv")
  }

  met_data <- readr::read_csv(met_url, skip = 2, show_col_types = FALSE)
  met_date <- derive_date_from_met(met_data)

  list(met_data = met_data, met_date = met_date, met_file_path = met_url)
}

# ---------------------------------------------------------------------------
# Per-tile UTCI/UTCIcat recompute
# ---------------------------------------------------------------------------

# tcm_results_subpath is one of:
#   "tcm_results/met_era5_hottest_days"            (baseline, trees)
#   "tcm_results/met_era5_hottest_days/t0" or "/t3" (shade-structures, per transmissivity)
#   "tcm_results/reduced_temps"                     (cool-roofs, cool-roofs_trees)
#
# When write_baseline_ccl_layers = TRUE (baseline only), also writes the
# ccl_layers/utci-{time}__baseline__baseline.tif + utci-cat-* copies for this
# tile, mirroring baseline-layers.R:186-204 -- every scenario's post-processing
# step reads these as its baseline reference, and baseline has no separate
# "process" step of its own to produce them otherwise.
recompute_utci_for_tile <- function(bucket_prefix, city, aoi_name, infra, scenario, tile,
                                     tcm_results_subpath, met, date_audit_log_path,
                                     write_baseline_ccl_layers = FALSE,
                                     bucket = "wri-cities-tcm",
                                     aws_http = "https://wri-cities-tcm.s3.us-east-1.amazonaws.com") {

  scenario_folder <- glue("{bucket_prefix}/{city}/{aoi_name}/scenarios/{infra}/{scenario}")
  tile_prefix      <- glue("{scenario_folder}/{tile}/{tcm_results_subpath}/")

  keys <- list_s3_keys(bucket, tile_prefix)
  tmrt_keys <- keys[grepl("/Tmrt_.*\\.tif$", keys, ignore.case = TRUE)]

  if (length(tmrt_keys) == 0) {
    stop("No Tmrt_*.tif files found under s3://", bucket, "/", tile_prefix, call. = FALSE)
  }

  times_processed <- character(0)

  for (key in tmrt_keys) {
    fname <- basename(key)
    time  <- sub(".*_(\\d+)D\\.tif$", "\\1", fname)

    # ---- MANDATORY date-match check: hard-fail on any mismatch -----------
    tmrt_date <- extract_date_from_tmrt_filename(fname)
    if (!identical(tmrt_date, met$met_date)) {
      stop(glue(
        "DATE MISMATCH -- refusing to compute UTCI.\n",
        "  AOI:      {city}/{aoi_name}\n",
        "  Infra:    {infra} / Scenario: {scenario}\n",
        "  Tile:     {tile}\n",
        "  Time:     {time}\n",
        "  Tmrt file:      {fname}  -> date = {tmrt_date}\n",
        "  Met file used:  {met$met_file_path}  -> date = {met$met_date}\n",
        "This tile's Tmrt raster was generated for a different date than the met\n",
        "data about to be applied to it. Fix the mismatch (confirm which is wrong)\n",
        "before re-running. No files were written for this tile/time."
      ), call. = FALSE)
    }

    log_date_audit(date_audit_log_path, city, aoi_name, infra, scenario, tile, time,
                    tmrt_date, met$met_date, "MATCH")

    # ---- UTCI / UTCIcat recompute (unchanged math) ------------------------
    tmrt_http <- glue("{aws_http}/{tile_prefix}{fname}")
    mrt <- rast_retry(tmrt_http)

    utci       <- create_utci(mrt, time, met$met_data)
    utci_class <- utci_risk_cat(utci)

    utci_out    <- glue("{bucket}/{tile_prefix}UTCI_{met$met_date}_{time}D.tif")
    utcicat_out <- glue("{bucket}/{tile_prefix}UTCIcat_{met$met_date}_{time}D.tif")
    write_s3(utci, utci_out)
    write_s3(utci_class, utcicat_out)

    if (write_baseline_ccl_layers) {
      ccl_prefix <- glue("{bucket}/{scenario_folder}/{tile}/ccl_layers/")
      write_s3(utci,       glue("{ccl_prefix}utci-{time}__baseline__baseline.tif"))
      write_s3(utci_class, glue("{ccl_prefix}utci-cat-{time}__baseline__baseline.tif"))
    }

    times_processed <- c(times_processed, time)
  }

  invisible(times_processed)
}

# ---------------------------------------------------------------------------
# Date-audit logging (append-only, positive proof of every check)
# ---------------------------------------------------------------------------

log_date_audit <- function(date_audit_log_path, city, aoi_name, infra, scenario, tile, time,
                            tmrt_date, met_date, status) {
  row <- tibble::tibble(
    timestamp = as.character(Sys.time()),
    city = city, aoi_name = aoi_name, infra = infra, scenario = scenario,
    tile = tile, time = time, tmrt_date = tmrt_date, met_date = met_date, status = status
  )
  write_header <- !file.exists(date_audit_log_path)
  readr::write_csv(row, date_audit_log_path, append = !write_header, col_names = write_header)
}

# ---------------------------------------------------------------------------
# Orchestration-facing dispatcher: one infra/scenario, all its tiles
# ---------------------------------------------------------------------------

# Dispatches recompute_utci_for_tile() across all tiles for one (infra,
# scenario), per infra type. Does NOT call process_tcm_layers() /
# shade_structure_post_processing() -- the caller (recompute-utci-correct.R)
# does that afterward, since those are already flexible (take folder paths as
# parameters) and need no modification.
recompute_utci_for_infra_scenario <- function(bucket_prefix, city, aoi_name, infra, scenario,
                                               tiles, met, date_audit_log_path) {

  is_shade <- infra == "shade-structures"
  is_reduced_temps <- infra %in% c("cool-roofs", "cool-roofs_trees", "trees_cool-roofs")

  if (is_shade) {
    subpaths <- c("tcm_results/met_era5_hottest_days/t0", "tcm_results/met_era5_hottest_days/t3")
  } else if (is_reduced_temps) {
    subpaths <- "tcm_results/reduced_temps"
  } else {
    subpaths <- "tcm_results/met_era5_hottest_days"
  }

  write_ccl <- infra == "baseline"
  scenario_folder <- glue("{bucket_prefix}/{city}/{aoi_name}/scenarios/{infra}/{scenario}")

  for (subpath in subpaths) {
    # Shade-structures may legitimately have only one of t0/t3 actually run.
    # Check at the scenario level (across all tiles) before looping tiles, so
    # a genuinely absent subpath is skipped gracefully rather than hard-failing
    # on the first tile. A subpath that exists but is missing data for one
    # specific tile is still a real, stop()-worthy anomaly (handled inside
    # recompute_utci_for_tile()).
    scenario_keys <- list_s3_keys("wri-cities-tcm", glue("{scenario_folder}/"))
    any_tmrt <- length(grep(glue("{subpath}/Tmrt_"), scenario_keys, fixed = FALSE)) > 0

    if (!any_tmrt) {
      message("  [skip] no Tmrt files anywhere under ", subpath, " for ", city, "/", aoi_name,
              " ", infra, "/", scenario, " -- skipping this subpath entirely")
      next
    }

    for (tile in tiles) {
      recompute_utci_for_tile(
        bucket_prefix, city, aoi_name, infra, scenario, tile,
        tcm_results_subpath = subpath,
        met = met,
        date_audit_log_path = date_audit_log_path,
        write_baseline_ccl_layers = write_ccl
      )
    }
  }

  invisible(TRUE)
}
