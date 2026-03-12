
library(glue)
library(tidyverse)
library(terra)

source(here("tiling-scripts", "utils.R"))

process_tcm_layers <- function(baseline_folder, infra, scenario, scenario_folder){
  
  aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  
  if (infra %in% c("cool-roofs", "cool-roofs_trees")) {
    results_dir <- "reduced_temps"
  } else {
    results_dir <- "met_era5_hottest_days"
  }
  
  tiles <- list_tiles(glue("s3://wri-cities-tcm/{scenario_folder}"))
  
  for (t in tiles){
    
    files <- list_s3_keys("wri-cities-tcm", glue("{scenario_folder}/{t}/tcm_results/{results_dir}"))
    
    for (h in c("1200", "1500", "1800")){
      base_utci <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/utci-{h}__baseline__baseline.tif"))
      base_shade <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/shade-{h}__baseline__baseline.tif"))
      
      utci_cat_key <- files[grepl(paste0("/UTCIcat_.*_", h, "D\\.tif$"), files)]
      utci_key     <- files[grepl(paste0("/UTCI_.*_", h, "D\\.tif$"), files)]

      utci_cat <- rast_retry(glue("{aws_http}/{utci_cat_key}")) %>%
        crop(base_utci)
      utci     <- rast_retry(glue("{aws_http}/{utci_key}")) %>%
        crop(base_utci)

      diff_utci <- crop(utci, base_utci) - base_utci

      write_s3(utci, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/utci-{h}__{infra}__{scenario}.tif"))
      write_s3(utci_cat, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/utci-cat-{h}__{infra}__{scenario}.tif"))
      write_s3(diff_utci, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/utci-{h}__{infra}__{scenario}__vs-baseline.tif"))

      if (scenario != "cool-roofs"){
        shade_key   <- files[grepl(paste0("/Shadow_.*_", h, "D\\.tif$"), files)]
        shade   <- rast_retry(glue("{aws_http}/{shade_key}")) %>% 
          crop(base_utci)
        
        shade_recat <- ifel(
          is.na(shade), NA,
          ifel(shade == 0, 1,
               ifel(shade == 1, 0, 2))
        )
        
        write_s3(shade_recat, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/shade-{h}__{infra}__{scenario}.tif"))
        
        shade <- shade_recat > 0
        base_shade <- base_shade > 0

        diff_shade <- crop(shade, base_shade) - base_shade
        write_s3(diff_shade, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/shade-{h}__{infra}__{scenario}__vs-baseline.tif"))
      }
      
      shade <- rast_retry(glue("{aws_http}/{scenario_folder}/{t}/ccl_layers/shade-{h}__{infra}__{scenario}.tif")) > 0
      shade_dist <- distance(shade %>% subst(0, NA))
      write_s3(shade_dist, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/shade-distance-{h}__{infra}__{scenario}.tif"))
    }
    
  }
  
}

shade_structure_post_processing <- function(baseline_folder, 
                                            scenario_folder, 
                                            infra,
                                            scenario){
  
  library(terra)
  source(here("utils", "utci.R"))
  
  times <- c("1200", "1500", "1800")
  
  # Input paths
  # results_dir_t3 <- file.path("~", "CTCM_outcome", 
  #                             glue("{city}_{infra}_{scenario}_t3"), 
  #                             glue("{city}_{infra}_{scenario}_t3_{scenario}_{infra}"))
  # results_dir_t0 <- file.path("~", "CTCM_outcome", 
  #                             glue("{city}_{infra}_{scenario}_t0"), 
  #                             glue("{city}_{infra}_{scenario}_t0_{scenario}_{infra}"))
  
  tiles <- list_tiles(glue("s3://wri-cities-tcm/{scenario_folder}"))
  
  for (t in tiles){
    
    # tile_results_dir_t3 <- file.path(results_dir_t3, t, "tcm_results")
    tile_results_dir_t3 <- glue("{scenario_folder}/{t}/tcm_results/met_era5_hottest_days/t3")
    tile_results_dir_t0 <- glue("{scenario_folder}/{t}/tcm_results/met_era5_hottest_days/t0")
    
    for (time in times) {
      
      files_t3 <- list_s3_keys("wri-cities-tcm", tile_results_dir_t3)
      files_t0 <- list_s3_keys("wri-cities-tcm", tile_results_dir_t0)
      
      # Base UTCI
      base_utci <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/utci-{time}__baseline__baseline.tif"))
      
      # t3 shade
      shade_key   <- files_t3[grepl(paste0("/Shadow_.*_", time, "D\\.tif$"), files_t3)]
      struct_t3_mask   <- rast_retry(glue("{aws_http}/{shade_key}")) %>% 
          crop(base_utci, mask = TRUE)
      
      # t0 utci
      utci_key   <- files_t0[grepl(paste0("/UTCI_.*_", time, "D\\.tif$"), files_t0)]
      struct_t0_utci   <- rast_retry(glue("{aws_http}/{utci_key}")) %>% 
          crop(base_utci, mask = TRUE)
      
      # struct_t3_mask <- rast_retry(here(tile_results_dir_t3, shadow_file)) %>% 
      #   crop(baseline_utci, mask = TRUE)
      
      # struct_t0_utci <- rast_retry(here(tile_results_dir_t0, utci_file)) %>% 
      #   crop(baseline_utci, mask = TRUE)
      
      base_shade <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/shade-{time}__baseline__baseline.tif")) 
      struct_t0_shadow <- rast_retry(glue("{aws_http}/{tile_results_dir_t0}/{basename(shade_key)}")) %>% 
        crop(base_utci, mask = TRUE)
      
      struct_t0_shadow_recat <- ifel(
        is.na(struct_t0_shadow), NA,
        ifel(struct_t0_shadow == 0, 1,
             ifel(struct_t0_shadow == 1, 0, 2))
      )
      
      # Create mask: only keep areas where t3 mask is between 0 and 1
      struct_shade_mask <- struct_t3_mask > 0 & struct_t3_mask < 1
      
      # Apply conditional replacement
      utci_composite <- ifel(struct_shade_mask, struct_t0_utci, base_utci)
      shadow_composite <- ifel(struct_shade_mask, struct_t0_shadow_recat, base_shade)
      
      # shade_recat <- ifel(
      #   is.na(shadow_composite), NA,
      #   ifel(shadow_composite == 0, 1,
      #        ifel(shadow_composite == 1, 0, 2))
      # )
      
      # shade diff
      shade <- shadow_composite > 0
      base_shade <- base_shade > 0
      
      diff_shade <- shade - base_shade
      
      # Shade distance
      shade_dist <- distance(shade %>% subst(0, NA))
      
      # utci diff
      utci_diff <- utci_composite - base_utci
      
      # utci category
      utci_cat <- utci_risk_cat(utci_composite)
      
      # Write to output
      write_s3(utci_composite, 
               glue("{bucket}/{scenario_folder}/{t}/ccl_layers/utci-{time}__{infra}__{scenario}.tif"))
      write_s3(utci_cat, 
               glue("{bucket}/{scenario_folder}/{t}/ccl_layers/utci-cat-{time}__{infra}__{scenario}.tif"))
      write_s3(utci_diff, 
               glue("{bucket}/{scenario_folder}/{t}/ccl_layers/utci-{time}__{infra}__{scenario}__vs-baseline.tif"))
      
      write_s3(shade, 
               glue("{bucket}/{scenario_folder}/{t}/ccl_layers/shade-{time}__{infra}__{scenario}.tif"))
      write_s3(diff_shade, 
               glue("{bucket}/{scenario_folder}/{t}/ccl_layers/shade-{time}__{infra}__{scenario}__vs-baseline.tif"))
      write_s3(shade_dist, 
               glue("{bucket}/{scenario_folder}/{t}/ccl_layers/shade-distance-{time}__{infra}__{scenario}.tif"))
      
      message("Composite rasters written for time: ", time)
    }
    
  }
  
  
}

  