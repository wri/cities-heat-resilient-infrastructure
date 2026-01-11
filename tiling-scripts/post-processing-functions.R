process_tcm_layers <- function(baseline_folder, infra, scenario, scenario_folder){
  
  aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  tiles <- list_tiles(glue("s3://wri-cities-tcm/{scenario_folder}"))
  
  for (t in tiles){
    
    files <- list_s3_keys("wri-cities-tcm", glue("{scenario_folder}/{t}/tcm_results/met_era5_hottest_days"))
    
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
        diff_shade <- crop(shade, base_shade) - base_shade
        
        write_s3(shade, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/shade-{h}__{infra}__{scenario}.tif"))
        write_s3(diff_shade, glue("wri-cities-tcm/{scenario_folder}/{t}/ccl_layers/shade-{h}__{infra}__{scenario}__vs-baseline.tif"))
      }
    }
    
  }
  
}

  