shade_structure_post_processing <- function(){
  
  library(terra)


  times <- c("1200D", "1500D", "1800D")
  
  # Input paths
  baseline_dir <- here("data", city, "scenarios", "baseline")
  t0_dir <- here("data", city, "scenarios", "park-shade-structures", "program-potential", "t0")
  t3_dir <- here("data", city, "scenarios", "park-shade-structures", "program-potential", "t3")
  
  # Output path
  output_dir <- here("data", city, "scenarios", "park-shade-structures", "program-potential")
  
  for (time in times) {
    # Input file paths
    shadow_file <- list.files(t3_dir, pattern = str_c("Shadow.*", time))
    tmrt_file <- list.files(t3_dir, pattern = str_c("Tmrt.*", time))
    
    # Read rasters
    struct_t3_mask <- rast(here(t3_dir, shadow_file))
    
    baseline_tmrt <- rast(here(baseline_dir, tmrt_file))
    struct_t0_tmrt <- rast(here(t0_dir, tmrt_file))
    
    baseline_shadow <- rast(here(baseline_dir, shadow_file))
    struct_t0_shadow <- rast(here(t0_dir, shadow_file))
    
    # Create mask: only keep areas where t3 mask is between 0 and 1
    tree_shade_mask <- struct_t3_mask >= 0 & struct_t3_mask <= 1
    
    # Apply conditional replacement
    tmrt_composite <- ifel(tree_shade_mask, struct_t0_tmrt, baseline_tmrt)
    shadow_composite <- ifel(tree_shade_mask, struct_t0_shadow, baseline_shadow)
    
    # Write to output
    writeRaster(tmrt_composite, file.path(output_dir, tmrt_file), overwrite = TRUE)
    writeRaster(shadow_composite, file.path(output_dir, shadow_file), overwrite = TRUE)
    
    message("Composite rasters written for time: ", time)
  }
}