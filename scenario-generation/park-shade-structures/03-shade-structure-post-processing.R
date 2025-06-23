shade_structure_post_processing <- function(city){
  
  library(terra)


  times <- c("1200", "1500", "1800")
  
  # Input paths
  baseline_dir <- here("data", city, "scenarios", "baseline")
  t0_dir <- here("data", city, "scenarios", "park-shade-structures", "program-potential", "t0")
  t3_dir <- here("data", city, "scenarios", "park-shade-structures", "program-potential", "t3")
  
  # Output path
  output_dir <- here("data", city, "scenarios", "park-shade-structures", "program-potential")
  
  for (time in times) {
    # Input file paths
    shadow_file <- list.files(t3_dir, pattern = str_c("shade.*", time)) %>%
      str_subset("aux", negate = TRUE)
    
    tmrt_file <- list.files(t3_dir, pattern = str_c("Tmrt.*", time)) %>%
      str_subset("aux", negate = TRUE)
    
    # Read rasters
    baseline_tmrt <- rast(here(baseline_dir, tmrt_file)) 
    
    struct_t3_mask <- rast(here(t3_dir, shadow_file)) %>% 
      crop(baseline_tmrt, mask = TRUE)
   
    struct_t0_tmrt <- rast(here(t0_dir, tmrt_file)) %>% 
      crop(baseline_tmrt, mask = TRUE)
    
    baseline_shadow <- rast(here(baseline_dir, shadow_file))
    struct_t0_shadow <- rast(here(t0_dir, shadow_file)) %>% 
      crop(baseline_tmrt, mask = TRUE)
    
    # Create mask: only keep areas where t3 mask is between 0 and 1
    tree_shade_mask <- struct_t3_mask > 0 & struct_t3_mask < 1
    
    # Apply conditional replacement
    tmrt_composite <- ifel(tree_shade_mask, struct_t0_tmrt, baseline_tmrt)
    shadow_composite <- ifel(tree_shade_mask, struct_t0_shadow, baseline_shadow)
    
    # Write to output
    writeRaster(tmrt_composite, 
                file.path(output_dir, tmrt_file), 
                overwrite = TRUE)
    writeRaster(shadow_composite, 
                file.path(output_dir, shadow_file), 
    overwrite = TRUE)
    
    message("Composite rasters written for time: ", time)
  }
}