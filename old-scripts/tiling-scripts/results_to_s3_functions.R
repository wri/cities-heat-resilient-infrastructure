source(here("utils", "utci.R"))

upload_CTCM_results_to_s3 <- function(
    city,
    infra,
    scenario,
    aoi_name,
    quiet           = FALSE
) {
  
  tcm_results_dir <-  "tcm_results/met_era5_hottest_days"
  primary_dir     <-  "primary_data/raster_files"
  processed_dir   <-  "processed_data"
  
  scenario_folder <- file.path("city_projects", city, aoi_name, "scenarios", infra, scenario)
  name <- glue("{city}_{infra}_{scenario}")
  
  bucket_prefix <- glue("s3://wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}")
  
  # Met data for UTCI (if needed)
  met <- read_csv(
    glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/metadata/met_files/met_era5_hottest_days.csv"),
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
