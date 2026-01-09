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
      tiles_s3 = tiles_s3
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
  
  for (t in tiles_s3){
    
    tile <- buffered_tile_grid %>% 
      filter(tile_name == t)
    
    # Filter to buildings in tile
    tile_buildings <- buildings %>% 
      st_filter(tile)
    
    # Load data
    albedo <- tryCatch(
      rast(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_albedo_cloud_masked.tif")),
      error = function(e) NULL
    )
    
    if (is.null(albedo)) {
      print(glue("{t} is missing albedo data"))
      next
    }
    
    if (nrow(tile_buildings) == 0) {
      
      for (s in c("all-buildings", "large-buildings")){
        # Ensure prefix
        scenario_folder <- glue("{city_folder}/scenarios/{infra}/{s}")
        ensure_s3_prefix(bucket, glue("{scenario_folder}/{t}/ccl_layers"))
        
        # Write raster
        write_s3(albedo, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/albedo__cool-roofs__{s}.tif"))
        
        diff_albedo <- albedo
        values(diff_albedo) <- 0
        write_s3(diff_albedo, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/albedo__cool-roofs__{s}__vs-baseline.tif"))
      }
      
      
      print(glue("{t} has no buildings"))
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
    
    for (s in c("all-buildings", "large-buildings")){
      scenario <- s
      
      # Ensure prefix
      scenario_folder <- glue("{city_folder}/scenarios/{infra}/{scenario}")
      ensure_s3_prefix(bucket, glue("{scenario_folder}/{t}/ccl_layers"))
      
      # Filter based on building size
      if (scenario == "all-buildings"){
        tile_buildings_s <- tile_buildings
      } else if (scenario == "large-buildings"){
        tile_buildings_s <- tile_buildings %>% 
          filter(area_m2 >= area_threshold)
      }
      
      # Filter to only buildings with medians below cool roof albedos
      tile_buildings_s <- tile_buildings_s %>% 
        filter(median_alb < cool_roof_alb)
      
      if (nrow(tile_buildings_s) == 0) {
        # Write raster
        write_s3(albedo, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/albedo__cool-roofs__{scenario}.tif"))
        
        diff_albedo <- albedo
        values(diff_albedo) <- 0
        write_s3(diff_albedo, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/albedo__cool-roofs__{scenario}__vs-baseline.tif"))
        
        print(glue("{t} albedo rasters saved for {scenario}"))
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
      
      # Write raster
      write_s3(updated_albedo, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/albedo__cool-roofs__{scenario}.tif"))
      write_s3(diff_albedo, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/albedo__cool-roofs__{scenario}__vs-baseline.tif"))
      
      print(glue("{t} albedo rasters saved for {scenario}"))
    }
    
  }
  
  
}

calc_air_temp_delta <- function(city, scenario, aoi){
  
  # list tiles
  scenario_path <- glue("city_projects/{city}/{aoi_name}/scenarios/cool-roofs/{scenario}")
  tiles <- list_tiles(glue("s3://wri-cities-tcm/{scenario_path}"))
                      
  # load and merge scenario
  scenario_alb_diff <- load_and_merge(glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/{scenario_path}/{tiles}/ccl_layers/albedo__cool-roofs__{scenario}__vs-baseline.tif"))
  
  # Calculate albedo delta
  alb_delta <- global(mask(scenario_alb_diff, aoi), fun = "mean", na.rm = TRUE)$mean
  
  ###### CALCULATE TEMP CHAGE ######
  # from Krayenhoff et al. 2021 DOI 10.1088/1748-9326/abdcf1
  
  # 0.1 increase in albedo results in a 0.6 C reduction in air temp for midday clear-sky conditiosn
  
  ###### ADJUST TO 12:00, 15:00, 18:00 TEMP CHANGE ######
  
  dT_12 <- round(alb_delta * 6, 2)
  dT_15 <- round(0.935315 * dT_12, 2)
  dT_18 <- round(0.646853 * dT_12, 2)
  
  air_temp_reductions <- tribble(
    ~ Hour, ~ reduction,
    12, dT_12,
    15, dT_15,
    18, dT_18
  )
  
  write_s3(air_temp_reductions, 
           glue("wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/cool-roofs/{scenario}/air_temp_reductions.csv"))
  
  # Modify the met file with the updated air temperatures
  met <- read_delim(glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/accelerator_area/scenarios/baseline/baseline/metadata/met_files/met_era5_hottest_days.csv"),
                    skip = 2) %>% 
    left_join(air_temp_reductions, by = "Hour") %>%
    mutate(Temperature = Temperature - reduction) %>%
    select(-reduction)
  
  write_s3(met,
           glue("wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/cool-roofs/{scenario}/reduced_temps.txt"))
  
}
