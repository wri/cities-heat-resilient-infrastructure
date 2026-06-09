library(terra)
library(sf)
library(tidyverse)
library(here)
library(glue)
library(geojsonsf)
library(exactextractr)

aoi <- st_read("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ARG-Buenos_Aires/barrio_20/scenarios/baseline/baseline/aoi__baseline__baseline.geojson")

ba_alb_overture <- read_csv("/Users/elizabeth.wesley/Library/CloudStorage/OneDrive-WorldResourcesInstitute/Documents/Research/Albedo/manuscript/dataverse_files/buenos-aires_overture_aggregation.csv")
ba_alb_overture_sf <- ba_alb_overture |>
  mutate(geometry = geojson_sfc(geometry)) |>
  st_as_sf(crs = 4326) |> 
  st_transform(st_crs(aoi)) |> 
  st_make_valid() |>
  filter(st_is_valid(geometry), !st_is_empty(geometry)) |> 
  st_filter(aoi)

source(here("tiling-scripts", "utils.R"))  

s3 <- paws::s3()

city_folder     <- file.path("city_projects", "ARG-Buenos_Aires", "barrio_20")
baseline_folder <- file.path(city_folder, "scenarios", "baseline", "baseline")


tiles <- list_tiles(paste0("s3://wri-cities-tcm/", baseline_folder))
ba_paths <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ARG-Buenos_Aires/barrio_20/scenarios/baseline/baseline/{tiles}/raster_files/cif_open_urban.tif")
ba_lulc <- load_and_merge(ba_paths)

alb_paths <- glue("https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/ARG-Buenos_Aires/barrio_20/scenarios/baseline/baseline/{tiles}/raster_files/cif_albedo_cloud_masked.tif")
alb <- load_and_merge(alb_paths)  

overture_rast <- rasterize(ba_alb_overture_sf, alb, field = "PNeo", background = NA)

ov_alb <- ifel(ba_lulc %in% 600:622 & !is.na(overture_rast), overture_rast, alb)

ba_lulc <- ba_lulc |> 
  crop(aoi) |> 
  mask(aoi)

alb <- alb |> 
  crop(aoi) |> 
  mask(aoi)

ov_alb <- ov_alb |> 
  crop(aoi) |> 
  mask(aoi)

mean(values(alb), na.rm = TRUE)
mean(values(ov_alb), na.rm = TRUE)

alb2 <- ifel(ba_lulc %in% 600:622 & !is.na(alb), 0.62, alb)
ov_alb2 <- ifel(ba_lulc %in% 600:622 & !is.na(ov_alb), 0.62, ov_alb)

diff_alb <- alb2 - alb
diff_ov_alb <- ov_alb2 - ov_alb

mean(values(diff_alb), na.rm = TRUE)
mean(values(diff_ov_alb), na.rm = TRUE)

ba_alb_overture_sf <- ba_alb_overture_sf |>
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) |>
  st_cast("MULTIPOLYGON")

ba_alb_overture_sf$median_diff_alb <- exact_extract(diff_alb, ba_alb_overture_sf, 'median')
ba_alb_overture_sf$median_diff_ov_alb <- exact_extract(diff_ov_alb, ba_alb_overture_sf, 'median')

ba_alb_overture_sf |> 
  rename(S2_albedo_diff = median_diff_alb,
         PNeo_albedo_diff = median_diff_ov_alb) |> 
  pivot_longer(cols = c(S2_albedo_diff, PNeo_albedo_diff)) |> 
ggplot() +
  geom_density(aes(x = value, color = name)) 

library(tidyterra)
library(patchwork)

lims <- c(0, 1)

(ggplot() + geom_spatraster(data = alb) + scale_fill_viridis_c(limits = lims)) +
  (ggplot() + geom_spatraster(data = ov_alb) + scale_fill_viridis_c(limits = lims)) +
  plot_layout(guides = 'collect')

writeRaster(alb, "~/Downloads/alb.tif")  
writeRaster(ov_alb, "~/Downloads/ov_alb.tif")  

mean(values(mask(alb, ba_lulc %in% 600:622, maskvalue = FALSE)), na.rm = T)
mean(values(mask(ov_alb, ba_lulc %in% 600:622, maskvalue = FALSE)), na.rm = T)
