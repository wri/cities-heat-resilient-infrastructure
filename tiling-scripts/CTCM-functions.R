# Download CTCM data for scenarios
library(here)
library(tidyverse)
library(terra)
library(sf)
library(yaml)

download_tree_data <- function(city, infra, scenario, baseline_folder, scenario_folder, tiles){
  
  # Create folder
  ctcm_setup_path <- file.path("~", "CTCM_data_setup")
  template <- file.path(ctcm_setup_path, "ZZZ_template_city")
  
  # Create setup folder for new run
  run_setup_folder <- file.path(ctcm_setup_path, glue("{city}_{infra}_{scenario}"))
  R.utils::copyDirectory(template, run_setup_folder, overwrite = TRUE)
  
  unlink(file.path(run_setup_folder, "primary_data", "raster_files", "tile_00001"),
         recursive = TRUE,
         force = TRUE)
  
  for (t in tiles){
    
    # copy baseline files
    baseline_tile <- glue("{baseline_folder}/{t}")
    scenario_tile <- glue("{scenario_folder}/{t}")
    base_files <- c(
      # Baseline layers
      glue("{baseline_tile}/raster_files/cif_albedo_cloud_masked.tif"),
      glue("{baseline_tile}/raster_files/cif_dem.tif"),
      glue("{baseline_tile}/raster_files/cif_dsm_ground_build.tif"),
      glue("{baseline_tile}/raster_files/cif_lulc.tif"),
      glue("{baseline_tile}/raster_files/cif_open_urban.tif"),
      
      # scenario layers
      glue("{scenario_tile}/raster_files/tree_canopy.tif")
    )
    
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = base_files,
      local_dir = glue("{run_setup_folder}/primary_data/raster_files/{t}")
    )
    
    intermediate_files <- c(
      # intermediate layers
      glue("{baseline_tile}/processed_data/ctcm_wallaspect.tif"),
      glue("{baseline_tile}/processed_data/ctcm_wallheight.tif")
    )
    
    dir.create(glue("{run_setup_folder}/processed_data/{t}"), recursive = TRUE)
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = intermediate_files,
      local_dir = glue("{run_setup_folder}/processed_data/{t}")
    )
  }
  
  met_file <- glue("{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv")
  download_s3_files(
    bucket    = "wri-cities-tcm",
    s3_keys   = met_file,
    local_dir = glue("{run_setup_folder}/primary_data/met_files")
  )
}

download_cool_roof_data <- function(city, aoi_name, scenario, baseline_folder, tiles){
  
  scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/cool-roofs/{scenario}")
  
  # Create folder
  ctcm_setup_path <- file.path("~", "CTCM_data_setup")
  template <- file.path(ctcm_setup_path, "ZZZ_template_city")
  
  # Create setup folder for new run
  run_setup_folder <- file.path(ctcm_setup_path, glue("{city}_cool-roofs_{scenario}"))
  R.utils::copyDirectory(template, run_setup_folder, overwrite = TRUE)
  
  unlink(file.path(run_setup_folder, "primary_data", "raster_files", "tile_00001"),
         recursive = TRUE,
         force = TRUE)
  
  for (t in tiles){
    
    # copy baseline files
    baseline_tile <- glue("{baseline_folder}/{t}")
    scenario_tile <- glue("{scenario_folder}/{t}")
    base_files <- c(
      # Baseline layers
      glue("{baseline_tile}/raster_files/cif_dem.tif"),
      glue("{baseline_tile}/raster_files/cif_dsm_ground_build.tif"),
      glue("{baseline_tile}/raster_files/cif_lulc.tif"),
      glue("{baseline_tile}/raster_files/cif_open_urban.tif"),
      glue("{baseline_tile}/raster_files/cif_tree_canopy.tif"),
      
      # scenario layers
      glue("{scenario_tile}/ccl_layers/albedo__cool-roofs__{scenario}.tif")
      
    )
    
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = base_files,
      local_dir = glue("{run_setup_folder}/primary_data/raster_files/{t}")
    )

    intermediate_files <- c(
      # intermediate layers
      glue("{baseline_tile}/processed_data/ctcm_wallaspect.tif"),
      glue("{baseline_tile}/processed_data/ctcm_wallheight.tif")
    )
    
    dir.create(glue("{run_setup_folder}/processed_data/{t}"), recursive = TRUE)
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = intermediate_files,
      local_dir = glue("{run_setup_folder}/processed_data/{t}")
    )
    
    svf_files  <- c("svf.tif","svfaveg.tif","svfE.tif","svfEaveg.tif","svfEveg.tif",
                    "svfN.tif","svfNaveg.tif","svfNveg.tif","svfS.tif","svfSaveg.tif",
                    "svfSveg.tif","svfveg.tif","svfW.tif","svfWaveg.tif","svfWveg.tif")
    svf_files  <- glue("{baseline_tile}/processed_data/ctcm_svfs/{svf_files}")
    
    dir.create(glue("{run_setup_folder}/processed_data/{t}/ctcm_svfs"), recursive = TRUE)
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = svf_files,
      local_dir = glue("{run_setup_folder}/processed_data/{t}/ctcm_svfs")
    )
    
  }
  
  met_file <- glue("{scenario_folder}/reduced_temps.txt")
  download_s3_files(
    bucket    = "wri-cities-tcm",
    s3_keys   = met_file,
    local_dir = glue("{run_setup_folder}/primary_data/met_files")
  )
}


download_shade_data <- function(city, infra, scenario, baseline_folder, scenario_folder, tiles){
  
  # Create folder
  ctcm_setup_path <- file.path("~", "CTCM_data_setup")
  template <- file.path(ctcm_setup_path, "ZZZ_template_city")
  
  # Create setup folder for new run
  run_setup_folder <- file.path(ctcm_setup_path, glue("{city}_{infra}_{scenario}"))
  R.utils::copyDirectory(template, run_setup_folder, overwrite = TRUE)
  
  unlink(file.path(run_setup_folder, "primary_data", "raster_files", "tile_00001"),
         recursive = TRUE,
         force = TRUE)
  
  for (t in tiles){
    
    # copy baseline files
    baseline_tile <- glue("{baseline_folder}/{t}")
    scenario_tile <- glue("{scenario_folder}/{t}")
    base_files <- c(
      # Baseline layers
      glue("{baseline_tile}/raster_files/cif_albedo_cloud_masked.tif"),
      glue("{baseline_tile}/raster_files/cif_dem.tif"),
      glue("{baseline_tile}/raster_files/cif_dsm_ground_build.tif"),
      glue("{baseline_tile}/raster_files/cif_lulc.tif"),
      glue("{baseline_tile}/raster_files/cif_open_urban.tif"),
      
      # scenario layers
      glue("{scenario_tile}/ccl_layers/structures-as-trees.tif")
    )
    
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = base_files,
      local_dir = glue("{run_setup_folder}/primary_data/raster_files/{t}")
    )
    
    intermediate_files <- c(
      # intermediate layers
      glue("{baseline_tile}/processed_data/ctcm_wallaspect.tif"),
      glue("{baseline_tile}/processed_data/ctcm_wallheight.tif")
    )
    
    dir.create(glue("{run_setup_folder}/processed_data/{t}"), recursive = TRUE)
    download_s3_files(
      bucket    = "wri-cities-tcm",
      s3_keys   = intermediate_files,
      local_dir = glue("{run_setup_folder}/processed_data/{t}")
    )
  }
  
  met_file <- glue("{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv")
  download_s3_files(
    bucket    = "wri-cities-tcm",
    s3_keys   = met_file,
    local_dir = glue("{run_setup_folder}/primary_data/met_files")
  )
}

upload_CTCM_results_to_s3 <- function(
    city,
    infra,
    scenario,
    aoi_name,
    quiet           = FALSE
) {
  
  source(here("utils", "utci.R"))
  
  tcm_results_dir <-  "tcm_results/met_era5_hottest_days"
  primary_dir     <-  "primary_data/raster_files"
  processed_dir   <-  "processed_data"
  
  scenario_folder <- file.path("city_projects", city, aoi_name, "scenarios", infra, scenario)
  name <- glue("{city}_{infra}_{scenario}")
  
  bucket_prefix <- glue("s3://wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}")
  
  # Met data for UTCI (if needed)
  met_dir <- file.path(results_dir, "primary_data/met_files")
  
  if (dir.exists(met_dir)) {
    system2(
      "aws",
      c(
        "s3", "sync",
        paste0(met_dir, "/"),
        paste0(bucket_prefix, "/metadata/")
      ),
      stdout = if (quiet) FALSE else "",
      stderr = ""
    )
  } else {
    message("  (no ", met_dir, ", skipping)")
  }
  
  met <- read_csv(
    glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}/metadata/met_files/met_era5_hottest_days.csv"),
    skip = 2)
  year <- met$Year %>% unique()
  month <- met$Month %>% as.integer() %>% str_pad(2, pad = "0") %>% unique()
  day <- met$Day %>% as.integer() %>% str_pad(2, pad = "0") %>% unique()
  
  date_ymd <- glue("{year}-{month}-{day}")
  date_doy <- yday(date_ymd)
  
  date <- glue("{year}_{date_doy}")
  
  results_dir <- file.path("~", "CTCM_outcome", 
                           name, glue("{name}_{scenario}_{infra}"))
  
  # Find tile directories (e.g., tile_00001/)
  tile_dirs <- list.dirs(path.expand(file.path(results_dir, tcm_results_dir)),
                         recursive = FALSE, full.names = TRUE)
  
  if (length(tile_dirs) == 0) {
    stop("No tile directories found in ", tcm_results_dir)
  }
  
  for (tile_dir in tile_dirs) {
    
    tile <- basename(tile_dir)
    message("Processing tile: ", tile)
    
    # ---- raster_files ----
    src_primary <- file.path(results_dir, primary_dir, tile)
    if (dir.exists(src_primary)) {
      message("  Copying ", src_primary, " -> ",
              file.path(bucket_prefix, tile, "raster_files"))
      
      system2(
        "aws",
        c(
          "s3", "sync",
          paste0(src_primary, "/"),
          paste0(bucket_prefix, "/", tile, "/raster_files/")
        ),
        stdout = if (quiet) FALSE else "",
        stderr = ""
      )
    } else {
      message("  (no ", src_primary, ", skipping)")
    }
    
    # ---- processed_data ----
    src_processed <- file.path(results_dir, processed_dir, tile)
    if (dir.exists(src_processed)) {
      message("  Copying ", src_processed, " -> ",
              file.path(bucket_prefix, tile, "processed_data"))
      
      system2(
        "aws",
        c(
          "s3", "sync",
          paste0(src_processed, "/"),
          paste0(bucket_prefix, "/", tile, "/processed_data/")
        ),
        stdout = if (quiet) FALSE else "",
        stderr = ""
      )
    } else {
      message("  (no ", src_processed, ", skipping)")
    }
    
    # ---- tcm_results ----
    message("  Copying ", tile_dir, " -> ",
            file.path(bucket_prefix, tile, "tcm_results/met_era5_hottest_days"))
    
    system2(
      "aws",
      c(
        "s3", "sync",
        paste0(tile_dir, "/"),
        paste0(
          bucket_prefix, "/", tile,
          "/tcm_results/met_era5_hottest_days/"
        )
      ),
      stdout = if (quiet) FALSE else "",
      stderr = ""
    )
    
    files <- list.files(tile_dir, recursive = TRUE, full.names = FALSE)
    
    has_utci <- any(grepl("utci", files, ignore.case = TRUE))
    
    if (!has_utci){
      
      tile_files <- list.files(
        tile_dir,
        pattern = "\\.tif$",
        recursive = TRUE,
        full.names = TRUE
      )
      
      mrt_files <- tile_files[grepl("mrt", mrt_files, ignore.case = TRUE)]
      
      for (f in mrt_files){
        
        time <- sub(".*_(\\d+)D\\.tif$", "\\1", f)
        mrt <- rast(f)
        utci <- create_utci(mrt, time, met)
        utci_class <- utci_risk_cat(utci)
        
        out_path <- glue("wri-cities-tcm/{scenario_folder}/{tile}/tcm_results/met_era5_hottest_days/UTCI_{date}_{time}D.tif")
        write_s3(utci, out_path)
        
        out_path2 <- glue("wri-cities-tcm/{scenario_folder}/{tile}/tcm_results/met_era5_hottest_days/UTCIcat_{date}_{time}D.tif")
        write_s3(utci_class, out_path2)
        
      }
      
    }
    
    message("")
  }
  
  invisible(TRUE)
}


run_tree_CTCM <- function(city, infra, scenario, aoi_name){
  
  baseline_yaml <- read_yaml(glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/{baseline_folder}/metadata/config_method_parameters.yml"))
  
  run_setup_folder <- file.path("~", "CTCM_data_setup", glue("{city}_{infra}_{scenario}"))
  
  # Modify yaml file
  yaml_path <- file.path(run_setup_folder, "config_method_parameters.yml")
  scenario_yaml <- read_yaml(yaml_path)
  
  # Scenario
  scenario_yaml[[1]]$scenario_id <- scenario
  scenario_yaml[[1]]$infra_id <- infra
  
  # Processing AOI
  scenario_yaml[[2]]$seasonal_utc_offset <- baseline_yaml[[2]]$seasonal_utc_offset
  scenario_yaml[[2]]$city <- "None"
  scenario_yaml[[2]]$aoi_bounds$epsg_code <- baseline_yaml[[2]]$aoi_bounds$epsg_code
  scenario_yaml[[2]]$aoi_bounds$west <- baseline_yaml[[2]]$aoi_bounds$west
  scenario_yaml[[2]]$aoi_bounds$south <- baseline_yaml[[2]]$aoi_bounds$south
  scenario_yaml[[2]]$aoi_bounds$east <- baseline_yaml[[2]]$aoi_bounds$east
  scenario_yaml[[2]]$aoi_bounds$east <- baseline_yaml[[2]]$aoi_bounds$east
  
  # MetFiles
  scenario_yaml[[3]]$MetFiles[[1]]$filename <- "met_era5_hottest_days.txt"
  
  # CustomTiffFilenames
  scenario_yaml[[4]]$albedo_cloud_masked_tif_filename <- "cif_albedo_cloud_masked.tif"
  scenario_yaml[[4]]$dem_tif_filename <- "cif_dem.tif"
  scenario_yaml[[4]]$dsm_tif_filename <- "cif_dsm_ground_build.tif"
  scenario_yaml[[4]]$lulc_tif_filename <- "cif_lulc.tif"
  scenario_yaml[[4]]$open_urban_tif_filename <- "cif_open_urban.tif"
  scenario_yaml[[4]]$tree_canopy_tif_filename <- "tree_canopy.tif"
  
  # PreparedIntermediateFilenames
  scenario_yaml[[5]]$wall_aspect_filename <- "ctcm_wallaspect.tif"
  scenario_yaml[[5]]$wall_height_filename <- "ctcm_wallheight.tif"
  
  # MethodAttributes
  # Set sampling_local_hours as a verbatim string
  if (!is.null(scenario_yaml[[6]]$utci_output)) {
    scenario_yaml[[6]]$utci_output <- TRUE
  }
  scenario_yaml[[6]]$solweig$sampling_local_hours <- "12,15,18"
  class(scenario_yaml[[6]]$solweig$sampling_local_hours) <- "verbatim"
  
  # Define custom handler to write without quotes
  verbatim_handler <- function(x) {
    x  # return string directly with no quotes
  }
  
  # Write YAML with custom handler
  write_yaml(scenario_yaml, yaml_path, handlers = list(verbatim = verbatim_handler))
  
  # Run CTCM bat script
  library(withr)
  
  with_dir(run_setup_folder, {
    system(file.path(run_setup_folder, "b_run_CTCM_processing.sh"), wait = TRUE)
  })

  # Then continue with the rest of your R script
  message("CTCM processing complete. Copying ouput files to scenario folders...")
  
  upload_CTCM_results_to_s3(city, infra, scenario, aoi_name)
  
}

run_cool_roof_CTCM <- function(){
  baseline_yaml <- read_yaml(glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/{baseline_folder}/metadata/config_method_parameters.yml"))
  
  run_setup_folder <- file.path("~", "CTCM_data_setup", glue("{city}_{infra}_{scenario}"))
  
  # Modify yaml file
  yaml_path <- file.path(run_setup_folder, "config_method_parameters.yml")
  scenario_yaml <- read_yaml(yaml_path)
  
  # Scenario
  scenario_yaml[[1]]$scenario_id <- scenario
  scenario_yaml[[1]]$infra_id <- infra
  
  # Processing AOI
  scenario_yaml[[2]]$seasonal_utc_offset <- baseline_yaml[[2]]$seasonal_utc_offset
  scenario_yaml[[2]]$city <- "None"
  scenario_yaml[[2]]$aoi_bounds$epsg_code <- baseline_yaml[[2]]$aoi_bounds$epsg_code
  scenario_yaml[[2]]$aoi_bounds$west <- baseline_yaml[[2]]$aoi_bounds$west
  scenario_yaml[[2]]$aoi_bounds$south <- baseline_yaml[[2]]$aoi_bounds$south
  scenario_yaml[[2]]$aoi_bounds$east <- baseline_yaml[[2]]$aoi_bounds$east
  scenario_yaml[[2]]$aoi_bounds$east <- baseline_yaml[[2]]$aoi_bounds$east

  # MetFiles
  scenario_yaml[[3]]$MetFiles[[1]]$filename <- "reduced_temps.txt"
  
  # CustomTiffFilenames
  scenario_yaml[[4]]$albedo_cloud_masked_tif_filename <- "cif_albedo_cloud_masked.tif"
  scenario_yaml[[4]]$dem_tif_filename <- "cif_dem.tif"
  scenario_yaml[[4]]$dsm_tif_filename <- "cif_dsm_ground_build.tif"
  scenario_yaml[[4]]$lulc_tif_filename <- "cif_lulc.tif"
  scenario_yaml[[4]]$open_urban_tif_filename <- "cif_open_urban.tif"
  scenario_yaml[[4]]$tree_canopy_tif_filename <- "cif_tree_canopy.tif"
  
  # PreparedIntermediateFilenames
  scenario_yaml[[5]]$skyview_factor_filename <- "ctcm_svfs"
  scenario_yaml[[5]]$wall_aspect_filename <- "ctcm_wallaspect.tif"
  scenario_yaml[[5]]$wall_height_filename <- "ctcm_wallheight.tif"
  
  # MethodAttributes
  # Set sampling_local_hours as a verbatim string
  if (!is.null(scenario_yaml[[6]]$utci_output)) {
    scenario_yaml[[6]]$utci_output <- TRUE
  }
  scenario_yaml[[6]]$solweig$sampling_local_hours <- "12,15,18"
  class(scenario_yaml[[6]]$solweig$sampling_local_hours) <- "verbatim"
  
  # Define custom handler to write without quotes
  verbatim_handler <- function(x) {
    x  # return string directly with no quotes
  }
  
  # Write YAML with custom handler
  write_yaml(scenario_yaml, yaml_path, handlers = list(verbatim = verbatim_handler))
  
  # Run CTCM bat script
  library(withr)
  
  with_dir(run_setup_folder, {
    system(file.path(run_setup_folder, "b_run_CTCM_processing.sh"), wait = TRUE)
  })
  
  # Then continue with the rest of your R script
  message("CTCM processing complete. Copying ouput files to scenario folders...")
  
  upload_CTCM_results_to_s3(city, infra, scenario, aoi_name)
  
}

run_shade_structures_CTCM <- function(){
  baseline_yaml <- read_yaml(glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/{baseline_folder}/metadata/config_method_parameters.yml"))
  
  run_setup_folder <- file.path("~", "CTCM_data_setup", glue("{city}_{infra}_{scenario}"))
  
  # Modify yaml file
  yaml_path <- file.path(run_setup_folder, "config_method_parameters.yml")
  scenario_yaml <- read_yaml(yaml_path)
  
  # Scenario
  scenario_yaml[[1]]$scenario_id <- scenario
  scenario_yaml[[1]]$infra_id <- infra
  
  # Processing AOI
  scenario_yaml[[2]]$seasonal_utc_offset <- baseline_yaml[[2]]$seasonal_utc_offset
  scenario_yaml[[2]]$city <- "None"
  scenario_yaml[[2]]$aoi_bounds$epsg_code <- baseline_yaml[[2]]$aoi_bounds$epsg_code
  scenario_yaml[[2]]$aoi_bounds$west <- baseline_yaml[[2]]$aoi_bounds$west
  scenario_yaml[[2]]$aoi_bounds$south <- baseline_yaml[[2]]$aoi_bounds$south
  scenario_yaml[[2]]$aoi_bounds$east <- baseline_yaml[[2]]$aoi_bounds$east
  scenario_yaml[[2]]$aoi_bounds$east <- baseline_yaml[[2]]$aoi_bounds$east
  
  # MetFiles
  scenario_yaml[[3]]$MetFiles[[1]]$filename <- "met_era5_hottest_days.txt"
  
  # CustomTiffFilenames
  scenario_yaml[[4]]$albedo_cloud_masked_tif_filename <- "cif_albedo_cloud_masked.tif"
  scenario_yaml[[4]]$dem_tif_filename <- "cif_dem.tif"
  scenario_yaml[[4]]$dsm_tif_filename <- "cif_dsm_ground_build.tif"
  scenario_yaml[[4]]$lulc_tif_filename <- "cif_lulc.tif"
  scenario_yaml[[4]]$open_urban_tif_filename <- "cif_open_urban.tif"
  scenario_yaml[[4]]$tree_canopy_tif_filename <- "trees_as_structures.tif"
  
  # PreparedIntermediateFilenames
  scenario_yaml[[5]]$wall_aspect_filename <- "ctcm_wallaspect.tif"
  scenario_yaml[[5]]$wall_height_filename <- "ctcm_wallheight.tif"
  
  # MethodAttributes
  # Set sampling_local_hours as a verbatim string
  if (!is.null(scenario_yaml[[6]]$utci_output)) {
    scenario_yaml[[6]]$utci_output <- TRUE
  }
  scenario_yaml[[6]]$solweig$sampling_local_hours <- "12,15,18"
  class(scenario_yaml[[6]]$solweig$sampling_local_hours) <- "verbatim"
  
  # Define custom handler to write without quotes
  verbatim_handler <- function(x) {
    x  # return string directly with no quotes
  }
  
  # Write YAML with custom handler
  write_yaml(scenario_yaml, yaml_path, handlers = list(verbatim = verbatim_handler))
  
  # Run CTCM bat script
  library(withr)
  
  with_dir(run_setup_folder, {
    system(file.path(run_setup_folder, "b_run_CTCM_processing.sh"), wait = TRUE)
  })
  
  # Then continue with the rest of your R script
  message("CTCM processing complete. Copying ouput files to scenario folders...")
  
  upload_CTCM_results_to_s3(city, infra, scenario, aoi_name)
  
}