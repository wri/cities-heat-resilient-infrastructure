library(geoarrow)
library(sfarrow)
library(exactextractr)


source(here("tiling-scripts", "utils.R"))

update_albedo <- function(area_threshold = 2000){
  
  list2env(
    list(
      city = city,
      aoi = aoi,
      bucket = bucket,
      aws_http = aws_http,
      city_folder = city_folder,
      country = country,
      baseline_folder = baseline_folder,
      infra = infra,
      scenario = scenario,
      tiles_aoi = tiles_aoi
    ),
    envir = .GlobalEnv
  )
  
  # Get buildings
  open_urban_aws_http <- glue("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}")
  buildings_path <- glue("{open_urban_aws_http}/buildings/buildings_all.parquet")
  buildings <- st_read_parquet(buildings_path, quiet = TRUE) %>% 
    # Filter to only buildings within AOI
    st_filter(aoi) %>% 
    mutate(area_m2 = as.numeric(units::set_units(st_area(.), m^2)))
  
  for (t in tiles_aoi){
    
    tile <- tile_grid %>% 
      filter(tile_name == t)
    
    # Filter to buildings in tile
    tile_buildings <- buildings %>% 
      st_filter(tile)
    
    if (nrow(tile_buildings) == 0) {
      print(glue("{t} has no buildings"))
      next
    }
    
    # Load data
    albedo <- tryCatch(
      rast(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_albedo_cloud_masked.tif")),
      error = function(e) NULL
    )
    
    if (is.null(albedo)) {
      print(glue("{t} is missing albedo data"))
      next
    }
    
    med_build_alb <- exactextractr::exact_extract(albedo, tile_buildings, 'median', force_df = TRUE) 
    tile_buildings <- tile_buildings %>% 
      add_column(median_alb = med_build_alb$median)
    
    # if city is in the U.S. use the slope classification
    if (country == "USA"){
      
      open_urban <- rast(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_open_urban.tif"))
      build_slope <- exactextractr::exact_extract(open_urban, tile_buildings, 'mode', force_df = TRUE) 
      
      tile_buildings <- tile_buildings %>% 
        add_column(lulc = open_urban$mode) %>% 
        mutate(slope = case_when(lulc %in% c(600, 601, 611, 620, 621) ~ "low",
                                 lulc %in% c(602, 610, 612, 622) ~ "high",
                                 .default = NA),
               cool_roof_alb = if_else(slope == "low", 0.62, 0.28))
    } else {
      # Buildings are considered high-slope if they are less than 821 m2 in size
      tile_buildings <- tile_buildings %>% 
        mutate(slope = if_else(area_m2 > 821, "low", "high"),
               cool_roof_alb   = if_else(slope == "low", 0.62, 0.28))
    }
    
    write_s3(tile_buildings, glue("{bucket}/{city_folder}/scenarios/{infra}/all-buildings.geojson"))
    
    for (s in c("all-buildings", "large-buildings")){
      scenario <- s
      
      # Filter based on building size
      if (scenario == "all-buildings"){
        tile_buildings_s <- tile_buildings
      } else if (scenario == "large-buildings"){
        tile_buildings_s <- tile_buildings %>% 
          filter(area_m2 >= area_threshold)
      }
      
      # Filter to only buildings with medians below cool roof albedos
      tile_buildings_s <- tile_buildings_s %>% 
        filter(cool_roof_alb > median_alb)
      
      if (nrow(tile_buildings_s) == 0) {
        print(glue("{t} has no buildings that get cool roofs"))
        next
      }
      
      # Rasterize buildings
      build_rast <- rasterize(tile_buildings_s, albedo, field = "cool_roof_alb",
                              touches = TRUE, background = NA)
      
      # Mask albedo
      updated_albedo <- terra::ifel(
        is.na(build_rast),
        albedo,
        build_rast
      )
      
      # Albedo difference
      diff_albedo <- updated_albedo - albedo
      
      # Ensure prefix
      scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
      ensure_s3_prefix(bucket, glue("{scenario_folder}/{t}/ccl_layers"))
      
      # Write raster
      write_s3(updated_albedo, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/albedo__cool-roofs__{scenario}.tif"))
      write_s3(diff_albedo, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/albedo__cool-roofs__{scenario}__vs-baseline.tif"))
      
      print(glue("{t} albedo rasters saved for {scenario}"))
    }
    
  }
  
  
}


