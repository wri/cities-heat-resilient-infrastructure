library(terra)
library(sf)
library(dplyr)
library(glue)
library(purrr)
library(stringr)

aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"

library(tibble)

dat <- tribble(
  ~city, ~baseline_aoi, ~aoi_name,
  "ZAF-Cape_Town", "urban_extent", "business_district",
  "BRA-Rio_de_Janeiro", "low_emission_zone", "low_emission_zone",
  "MEX-Monterrey", "mitras_centro", "mitras_centro",
  "BRA-Teresina", "accelerator_area_big", "accelerator_area_big",
  "ARG-Buenos_Aires", "cildenez_padre_rodolfo_ricciardelli", "cildenez_padre_rodolfo_ricciardelli",
  "ZAF-Johannesburg", "jukskei-river", "jukskei-river",
  "IND-Bhopal", "tt_nagar", "tt_nagar",
  "BRA-Campinas", "accelerator", "accelerator_area",
  "BRA-Florianopolis", "accelerator", "accelerator_area",
  "BRA-Fortaleza", "accelerator", "accelerator_area",
  "BRA-Recife", "accelerator", "accelerator_area"
)

rasterize_building_albedo_tile <- function(city, aoi_name, tile) {
  print(glue("{city} tile {tile}"))
  
  scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/cool-roofs/all-buildings/{tile}/ccl_layers")
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/{tile}/ccl_layers")
  
  geojson_url <- glue("{aws_http}/{scenario_folder}/buildings__cool-roofs__all-buildings.geojson")
  template_url <- glue("{aws_http}/{scenario_folder}/albedo__cool-roofs__all-buildings__vs-baseline.tif")
  
  tryCatch({
    
    # check if geojson exists
    if (!s3_exists("wri-cities-tcm",
                   glue("{scenario_folder}/buildings__cool-roofs__all-buildings.geojson"))) {
      
      return(tibble(
        city = city,
        aoi_name = aoi_name,
        tile = tile,
        status = "missing_geojson"
      ))
    }
    
    template <- terra::rast(template_url)
    
    bldg <- terra::vect(geojson_url)
    
    if (!terra::same.crs(bldg, template)) {
      bldg <- terra::project(bldg, terra::crs(template))
    }
    
    r_median_alb <- terra::rasterize(
      bldg, template,
      field = "median_alb",
      background = NA_real_
    )
    
    r_cool_roof_alb <- terra::rasterize(
      bldg, template,
      field = "cool_roof_alb",
      background = NA_real_
    )
    
    out_baseline <- glue("wri-cities-tcm/{baseline_folder}/building-albedo-median__baseline__baseline.tif")
    out_scenario <- glue("wri-cities-tcm/{scenario_folder}/building-albedo-median__cool-roofs__all-buildings.tif")
    
    write_s3(r_median_alb, out_baseline)
    write_s3(r_cool_roof_alb, out_scenario)
    
    tibble(
      city = city,
      aoi_name = aoi_name,
      tile = tile,
      status = "success"
    )
    
  }, error = function(e) {
    
    tibble(
      city = city,
      aoi_name = aoi_name,
      tile = tile,
      status = "error",
      message = conditionMessage(e)
    )
    
  })
}

results <- purrr::map_dfr(seq_len(nrow(dat)), function(i) {
  
  city <- dat$city[i]
  aoi_name <- dat$aoi_name[i]
  
  tile_prefix <- glue("s3://wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/cool-roofs/all-buildings/")
  tiles <- list_tiles(tile_prefix)
  
  purrr::map_dfr(tiles, ~rasterize_building_albedo_tile(city, aoi_name, .x))
})

results
