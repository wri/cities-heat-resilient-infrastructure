cities <- c("BRA-Teresina", "BRA-Recife", "BRA-Fortaleza", "BRA-Florianopolis", "BRA-Campinas")
source(here("tiling-scripts", "metrics-functions.R"))

for (city in cities){
  
  city <- "ZAF-Durban"
  aoi_name <- "corridors_of_excellence"
  
  bucket   <- "wri-cities-tcm"
  aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
  s3 <- paws::s3()
  
  city_folder     <- file.path("city_projects", city, aoi_name)
  baseline_folder <- file.path(city_folder, "scenarios", "baseline", "baseline")
  
  aoi_path <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson")
  
  tiles_s3 <- list_tiles(paste0("s3://", bucket, "/", baseline_folder))
  
  tile_grid <- st_read(
    paste0(aws_http, "/", baseline_folder, "/metadata/.qgis_data/unbuffered_tile_grid.geojson"),
    quiet = TRUE
  ) |>
    filter(tile_name %in% tiles_s3)
  
  utm <- st_crs(tile_grid)
  
  aoi <- st_read(aoi_path, quiet = TRUE) |>
    st_transform(st_crs(utm))
  
  tile_grid_aoi <- tile_grid |> 
    st_filter(aoi) 
  tiles_aoi <- tile_grid_aoi$tile_name
  
  ###########################
  
  baseline_metrics(city, aoi_name, tiles_aoi)
  calc_street_tree_metrics(city, aoi_name, tiles_aoi, scenario = "custom-n_trees_uniform")
  
  
}
