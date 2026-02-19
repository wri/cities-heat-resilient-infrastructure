for (c in c("BRA-Florianopolis", "BRA-Teresina", "BRA-Fortaleza", "BRA-Recife")){
  
  city <- c
  aoi_name <- "accelerator_area"
  city_folder <- glue("city_projects/{city}/{aoi_name}")
  baseline_folder <- glue("{city_folder}/scenarios/baseline/baseline")
  
  # Parks
  from_park <- glue("OpenUrban/{city}/open_space/open_space_all.geojson")
  to_park <- glue("{baseline_folder}/parks-polygons__baseline__baseline.geojson")
  s3_copy_vec(from_park, to_park, 
              from_bucket = "wri-cities-heat", to_bucket = "wri-cities-tcm",
              overwrite = TRUE) 
  
  # Building polygons
  from_build <- glue("OpenUrban/{city}/buildings/buildings_all.geojson")
  to_build <- glue("{baseline_folder}/building-polygons__baseline__baseline.geojson")
  s3_copy_vec(from_build, to_build, 
              from_bucket = "wri-cities-heat", to_bucket = "wri-cities-tcm",
              overwrite = TRUE) 
}

