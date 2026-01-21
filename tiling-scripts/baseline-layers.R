# Create baseline data layers: 
##############################

# aoi__baseline__baseline.geojson
# albedo__baseline__baseline.tif
# building-areas__baseline__baseline.tif
# non-building-areas__baseline__baseline.tif
# shade-1500__baseline__baseline.tif
# tree-cover__baseline__baseline.tif
# tree-points__baseline__baseline.tif
# pedestrian-areas__baseline__baseline.tif
# parks__baseline__baseline.tif
# parks-polygons__baseline__baseline.geojson
# building-polygons__baseline__baseline.geosjon



library(terra)
library(sf)
library(glue)

source(here("tiling-scripts", "utils.R"))
source(here("tiling-scripts", "trees-functions.R"))
source(here("utils", "utci.R"))

save_baseline_layers <- function(utm){
  
  list2env(
    list(
      city = city,
      aoi_name = aoi_name,
      bucket = bucket,
      aws_http = aws_http,
      baseline_folder = baseline_folder,
      aoi_path = aoi_path,
      tiles_s3 = tiles_s3
    ),
    envir = .GlobalEnv
  )
  
  # AOI
  aoi <- st_read(aoi_path) %>%
    st_transform(utm)
  write_s3(aoi, glue("{bucket}/{baseline_folder}/aoi__baseline__baseline.geojson"))

  # Parks
  from_park <- glue("OpenUrban/{city}/open_space/open_space_all.parquet")
  to_park <- glue("{baseline_folder}/parks-polygons__baseline__baseline.parquet")
  s3_copy_vec(from_park, to_park,
              from_bucket = "wri-cities-heat", to_bucket = "wri-cities-tcm",
              overwrite = TRUE)

  # Building polygons
  from_build <- glue("OpenUrban/{city}/buildings/buildings_all.parquet")
  to_build <- glue("{baseline_folder}/building-polygons__baseline__baseline.parquet")
  s3_copy_vec(from_build, to_build,
              from_bucket = "wri-cities-heat", to_bucket = "wri-cities-tcm",
              overwrite = TRUE)
  
  # Get date stamp
  stamp <- find_shadow_stamp(bucket, baseline_folder, tiles_s3[[1]])
  
  # per tile
  for (t in tiles_s3){
    print(t)
    
    # Albedo
    alb <- rast(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_albedo_cloud_masked.tif"))
    write_s3(alb, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/albedo__baseline__baseline.tif"))
    
    # Building areas
    lulc <- rast(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_open_urban.tif"))
    
    build_areas <- terra::ifel(lulc >= 600 & lulc < 700, 1, 0)
    write_s3(build_areas, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/building-areas__baseline__baseline.tif"))
    
    non_build_areas <- abs(build_areas - 1)
    write_s3(non_build_areas, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/non-building-areas__baseline__baseline.tif"))
    
    # Pedestrian areas
    roads_distance <- distance(subst(lulc == 500, 0, NA))
    ped_area <- (roads_distance > 0) & (roads_distance <= 5) & (lulc != 500) & (lulc != 400)
    write_s3(ped_area, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/pedestrian-areas__baseline__baseline.tif"))
    
    # Parks
    parks <- lulc == 200
    write_s3(ped_area, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/parks__baseline__baseline.tif"))
    
    # Shade 
    shade_1200 <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/Shadow_{stamp}_1200D.tif"))
    shade_1200 <- ifel(
      is.na(shade_1200), NA,
      ifel(shade_1200 == 0, 1,
           ifel(shade_1200 == 1, 0, 2))
    )
    write_s3(shade_1200, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/shade-1200__baseline__baseline.tif"))
    
    shade_1500 <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/Shadow_{stamp}_1500D.tif"))
    shade_1500 <- ifel(
      is.na(shade_1500), NA,
      ifel(shade_1500 == 0, 1,
           ifel(shade_1500 == 1, 0, 2))
    )
    write_s3(shade_1500, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/shade-1500__baseline__baseline.tif"))
    
    shade_1800 <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/Shadow_{stamp}_1800D.tif"))
    shade_1800 <- ifel(
      is.na(shade_1800), NA,
      ifel(shade_1800 == 0, 1,
           ifel(shade_1800 == 1, 0, 2))
    )
    
    write_s3(shade_1800, glue("{bucket}/{baseline_folder}/{t}/ccl_layers/shade-1800__baseline__baseline.tif"))
    
    if (! s3_exists(bucket, glue("{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCI_{stamp}_1200D.tif"))) {
      
      met <- read_csv(
        glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/metadata/met_files/met_era5_hottest_days.csv"),
        skip = 2)
      
      for (time in c("1200", "1500", "1800")) {
        
        mrt <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/Tmrt_{stamp}_{time}D.tif"))
        utci <- create_utci(mrt, time, met)
        utci_class <- utci_risk_cat(utci)
        
        out_path <- glue("wri-cities-tcm/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCI_{stamp}_{time}D.tif")
        write_s3(utci, out_path)
        
        out_path2 <- glue("wri-cities-tcm/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCIcat_{stamp}_{time}D.tif")
        write_s3(utci_class, out_path2)
      }
      

      
      
      
      
    }
    
    # UTCI
    utci_1200 <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCI_{stamp}_1200D.tif"))
    utci_1200cat <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCIcat_{stamp}_1200D.tif"))
    
    write_s3(utci_1200, glue("wri-cities-tcm/{baseline_folder}/{t}/ccl_layers/utci-1200__baseline__baseline.tif"))
    write_s3(utci_1200cat, glue("wri-cities-tcm/{baseline_folder}/{t}/ccl_layers/utci-cat-1200__baseline__baseline.tif"))
    
    utci_1500 <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCI_{stamp}_1500D.tif"))
    utci_1500cat <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCIcat_{stamp}_1500D.tif"))
    
    write_s3(utci_1500, glue("wri-cities-tcm/{baseline_folder}/{t}/ccl_layers/utci-1500__baseline__baseline.tif"))
    write_s3(utci_1500cat, glue("wri-cities-tcm/{baseline_folder}/{t}/ccl_layers/utci-cat-1500__baseline__baseline.tif"))
    
    utci_1800 <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCI_{stamp}_1800D.tif"))
    utci_1800cat <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCIcat_{stamp}_1800D.tif"))
    
    write_s3(utci_1800, glue("wri-cities-tcm/{baseline_folder}/{t}/ccl_layers/utci-1800__baseline__baseline.tif"))
    write_s3(utci_1800cat, glue("wri-cities-tcm/{baseline_folder}/{t}/ccl_layers/utci-cat-1800__baseline__baseline.tif"))
    
    # Trees
    tree_canopy <- rast(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_tree_canopy.tif"))
    city_folder <- glue("city_projects/{city}/{aoi_name}")
    process_trees(tree_canopy, city_folder, baseline_folder, t_id = t)
  }
}
