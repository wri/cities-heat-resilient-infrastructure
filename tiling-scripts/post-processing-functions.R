
library(glue)
library(tidyverse)
library(terra)

process_tcm_layers <- function(baseline_folder, infra, scenario, scenario_folder, tiles_aoi){
  
  aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  # tiles <- list_tiles(glue("s3://wri-cities-tcm/{scenario_folder}"))
  
  if (infra == "cool-roofs") {
    results_dir <- "reduced_temps"
  } else {
    results_dir <- "met_era5_hottest_days"
  }
  
  for (t in tiles_aoi){
    
    files <- list_s3_keys("wri-cities-tcm", glue("{scenario_folder}/{t}/tcm_results/{results_dir}"))
    
    for (h in c("1200", "1500", "1800")){
      base_utci <- rast(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/utci-{h}__baseline__baseline.tif"))
      base_shade <- rast(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/shade-{h}__baseline__baseline.tif"))
      
      utci_cat_key <- files[grepl(paste0("/UTCIcat_.*_", h, "D\\.tif$"), files)]
      utci_key     <- files[grepl(paste0("/UTCI_.*_", h, "D\\.tif$"), files)]

      utci_cat <- terra::rast(glue("{aws_http}/{utci_cat_key}")) %>%
        crop(base_utci)
      utci     <- terra::rast(glue("{aws_http}/{utci_key}")) %>%
        crop(base_utci)

      diff_utci <- crop(utci, base_utci) - base_utci

      write_s3(utci, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/utci-{h}__{infra}__{scenario}.tif"))
      write_s3(utci_cat, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/utci-cat-{h}__{infra}__{scenario}.tif"))
      write_s3(diff_utci, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/utci-{h}__{infra}__{scenario}__vs-baseline.tif"))

      if (scenario != "cool-roofs"){
        shade_key   <- files[grepl(paste0("/Shadow_.*_", h, "D\\.tif$"), files)]
        shade   <- terra::rast(glue("{aws_http}/{shade_key}")) %>% 
          crop(base_utci)
        
        shade_recat <- ifel(
          is.na(shade), NA,
          ifel(shade == 0, 1,
               ifel(shade == 1, 0, 2))
        )
        
        write_s3(shade_recat, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/shade-{h}__{infra}__{scenario}.tif"))
        
        shade <- shade < 1

        diff_shade <- crop(shade, base_shade) - base_shade
        write_s3(diff_shade, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/shade-{h}__{infra}__{scenario}__vs-baseline.tif"))
      }
    }
    
  }
  
}

shade_structure_post_processing <- function(baseline_folder, 
                                            scenario_folder, 
                                            infra,
                                            scenario,
                                            tiles){
  
  library(terra)
  source(here("utils", "utci.R"))
  
  times <- c("1200", "1500", "1800")
  
  # Input paths
  results_dir_t3 <- file.path("~", "CTCM_outcome", 
                              glue("{city}_{infra}_{scenario}_t3"), 
                              glue("{city}_{infra}_{scenario}_t3_{scenario}_{infra}"))
  results_dir_t0 <- file.path("~", "CTCM_outcome", 
                              glue("{city}_{infra}_{scenario}_t0"), 
                              glue("{city}_{infra}_{scenario}_t0_{scenario}_{infra}"))
  
  for (tile in tiles){
    
    tile_results_dir_t3 <- file.path(results_dir_t3, tile, "tcm_results")
    tile_results_dir_t0 <- file.path(results_dir_t0, tile, "tcm_results")
    
    for (time in times) {
      # Input file paths
      shadow_file <- list.files(tile_results_dir_t3, 
                                pattern = str_c("Shadow.*", time),
                                recursive = TRUE) %>%
        str_subset("aux", negate = TRUE)
      
      utci_file <- list.files(tile_results_dir_t0, 
                              pattern = str_c("UTCI.*", time),
                              recursive = TRUE) %>%
        str_subset("aux", negate = TRUE) %>% 
        str_subset("cat", negate = TRUE)
      
      # Read rasters
      baseline_utci <- rast(glue("{aws_http}/{baseline_folder}/{tile}/ccl_layers/utci-{time}__baseline__baseline.tif")) 
      
      struct_t3_mask <- rast(here(tile_results_dir_t3, shadow_file)) %>% 
        crop(baseline_utci, mask = TRUE)
      
      struct_t0_utci <- rast(here(tile_results_dir_t0, utci_file)) %>% 
        crop(baseline_utci, mask = TRUE)
      
      baseline_shadow <- rast(glue("{aws_http}/{baseline_folder}/{tile}/ccl_layers/shade-{time}__baseline__baseline.tif")) 
      struct_t0_shadow <- rast(here(tile_results_dir_t0, shadow_file)) %>% 
        crop(baseline_utci, mask = TRUE)
      
      struct_t0_shadow_recat <- ifel(
        is.na(struct_t0_shadow), NA,
        ifel(struct_t0_shadow == 0, 1,
             ifel(struct_t0_shadow == 1, 0, 2))
      )
      
      # Create mask: only keep areas where t3 mask is between 0 and 1
      struct_shade_mask <- struct_t3_mask > 0 & struct_t3_mask < 1
      
      # Apply conditional replacement
      utci_composite <- ifel(struct_shade_mask, struct_t0_utci, baseline_utci)
      shadow_composite <- ifel(struct_shade_mask, struct_t0_shadow_recat, baseline_shadow)
      
      # shade diff
      struct_t0_shadow_recat <- struct_t0_shadow_recat < 1
      shade_diff <- shadow_composite - baseline_shadow
      
      # utci diff
      utci_diff <- utci_composite - baseline_utci
      
      # utci category
      utci_cat <- utci_risk_cat(utci_composite)
      
      # Write to output
      write_s3(utci_composite, 
               glue("{bucket}/{scenario_folder}/{tile}/ccl_layers/utci-{time}__{infra}__{scenario}.tif"))
      write_s3(utci_cat, 
               glue("{bucket}/{scenario_folder}/{tile}/ccl_layers/utci-cat-{time}__{infra}__{scenario}.tif"))
      write_s3(utci_diff, 
               glue("{bucket}/{scenario_folder}/{tile}/ccl_layers/utci-{time}__{infra}__{scenario}__vs-baseline.tif"))
      
      write_s3(shadow_composite, 
               glue("{bucket}/{scenario_folder}/{tile}/ccl_layers/shade-{time}__{infra}__{scenario}.tif"))
      write_s3(shade_diff, 
               glue("{bucket}/{scenario_folder}/{tile}/ccl_layers/shade-{time}__{infra}__{scenario}__vs-baseline.tif"))
      
      message("Composite rasters written for time: ", time)
    }
    
  }
  
  
}

  