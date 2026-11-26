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
  filename <- if (is_reduced_temps) "reduced_temps.csv" else "met_era5_hottest_days.csv"
  folder <- if (is_reduced_temps) {
    glue("{bucket_prefix}/{city}/{aoi_name}/scenarios/{infra}/{scenario}/metadata")
  } else {
    glue("{bucket_prefix}/{city}/{aoi_name}/scenarios/baseline/baseline/metadata")
  }

  # Most AOIs keep met files under metadata/met_files/; a few (confirmed for
  # ARG-Buenos_Aires/urban_extent) have the file directly under metadata/
  # instead, with no met_files/ subfolder at all. Try the standard path first,
  # fall back to the flat layout rather than hard-failing.
  candidate_urls <- c(
    glue("{aws_http}/{folder}/met_files/{filename}"),
    glue("{aws_http}/{folder}/{filename}")
  )

  met_data <- NULL
  met_url <- NULL
  last_err <- NULL
  for (url in candidate_urls) {
    met_data <- tryCatch(readr::read_csv(url, skip = 2, show_col_types = FALSE),
                          error = function(e) { last_err <<- e; NULL })
    if (!is.null(met_data)) { met_url <- url; break }
  }
  if (is.null(met_data)) {
    stop("Could not find met file at any known path. Tried:\n",
         paste(candidate_urls, collapse = "\n"), "\n", conditionMessage(last_err), call. = FALSE)
  }

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

  # Only fetched/used when write_baseline_ccl_layers = TRUE, to check whether
  # the ccl_layers/ copies (written right after UTCI_/UTCIcat_, see below)
  # already exist too -- an extra S3 call, but only for baseline, and avoids
  # treating a tile as "done" from the tcm_results listing alone when a prior
  # interrupted run left the ccl_layers copies missing.
  ccl_keys <- if (write_baseline_ccl_layers) {
    list_s3_keys(bucket, glue("{scenario_folder}/{tile}/ccl_layers/"))
  } else {
    character(0)
  }

  times_processed <- character(0)

  for (key in tmrt_keys) {
    fname <- basename(key)
    time  <- sub(".*_(\\d+)D\\.tif$", "\\1", fname)

    # ---- Skip if already computed: UTCI_/UTCIcat_ for this exact met_date
    # already sit under this tile's tcm_results subpath (from the SAME keys
    # listing used to find Tmrt files above -- no extra S3 call needed). This
    # makes the recompute idempotent/resumable at the tile+time level, not
    # just the AOI/infra/scenario level, so re-running (e.g. after an
    # interruption) never redoes tiles that already finished -- important for
    # AOIs with thousands of tiles (e.g. CHN-Chengdu) where redoing finished
    # work would be hugely wasteful.
    utci_out_key    <- glue("{tile_prefix}UTCI_{met$met_date}_{time}D.tif")
    utcicat_out_key <- glue("{tile_prefix}UTCIcat_{met$met_date}_{time}D.tif")
    already_done <- utci_out_key %in% keys && utcicat_out_key %in% keys
    if (already_done && write_baseline_ccl_layers) {
      ccl_prefix_check <- glue("{scenario_folder}/{tile}/ccl_layers/")
      already_done <- glue("{ccl_prefix_check}utci-{time}__baseline__baseline.tif") %in% ccl_keys &&
        glue("{ccl_prefix_check}utci-cat-{time}__baseline__baseline.tif") %in% ccl_keys
    }
    if (already_done) {
      times_processed <- c(times_processed, time)
      next
    }

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

  write_ccl <- infra == "baseline"
  scenario_folder <- glue("{bucket_prefix}/{city}/{aoi_name}/scenarios/{infra}/{scenario}")

  if (is_shade) {
    # t0/t3 are genuinely distinct transmissivity scenarios (not a naming
    # fallback), both always under met_era5_hottest_days/ -- checked once at
    # the scenario level, same as before: shade-structures may legitimately
    # have only one of t0/t3 actually run.
    for (subpath in c("tcm_results/met_era5_hottest_days/t0", "tcm_results/met_era5_hottest_days/t3")) {
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
    return(invisible(TRUE))
  }

  # Non-shade: the "expected" subpath follows the infra's own met-file
  # convention, but a handful of older-processed AOIs have their raw
  # Tmrt/Shadow data sitting under the OTHER convention's folder name
  # (confirmed for BRA-Fortaleza/accelerator_area's cool-roofs scenario:
  # reduced_temps.csv's date matches the Tmrt files sitting under
  # met_era5_hottest_days/ exactly) -- a historical pipeline-naming quirk, not
  # a data problem. The mandatory date-match check inside
  # recompute_utci_for_tile() remains the real safety net regardless of which
  # folder name is actually used, so this looks in whichever folder EACH tile
  # actually has data in, rather than assuming one folder name applies
  # scenario-wide.
  subpaths <- if (is_reduced_temps) {
    c("tcm_results/reduced_temps", "tcm_results/met_era5_hottest_days")
  } else {
    c("tcm_results/met_era5_hottest_days", "tcm_results/reduced_temps")
  }

  for (tile in tiles) {
    tile_keys <- list_s3_keys("wri-cities-tcm", glue("{scenario_folder}/{tile}/"))
    chosen_subpath <- NULL
    for (subpath in subpaths) {
      if (length(grep(glue("{subpath}/Tmrt_"), tile_keys, fixed = FALSE)) > 0) {
        chosen_subpath <- subpath
        break
      }
    }

    if (is.null(chosen_subpath)) {
      message("  [skip] ", tile, " has no Tmrt files under any known subpath (tried: ",
              paste(subpaths, collapse = ", "), ") -- skipping this tile")
      next
    }

    recompute_utci_for_tile(
      bucket_prefix, city, aoi_name, infra, scenario, tile,
      tcm_results_subpath = chosen_subpath,
      met = met,
      date_audit_log_path = date_audit_log_path,
      write_baseline_ccl_layers = write_ccl
    )
  }

  invisible(TRUE)
}
