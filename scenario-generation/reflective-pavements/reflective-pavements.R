reflective_pavements <- function(aoi, city_folder, potential, achievable_target = NULL){
  
  baseline_path <- here("data", city_folder, "scenarios", "baseline")
  scenario_path <- here("data", city_folder, "scenarios", "reflective-pavements", glue("{potential}-potential"))
  
  # Create directory
  dir.create(scenario_path, showWarnings = FALSE)
  
  # albedo
  albedo <- rast(here("data", city_folder, "cif_albedo_cloud_masked.tif"))
  lulc <- rast(here("data", city_folder, "open-urban.tif"))
  
  road_vectors <- st_read(here("data", city_folder, "scenarios", "baseline", "roads.geojson"))
  lanes <- read_csv(here("data", city_folder, "average-lanes.csv"))
  
  high <- c("primary", "primary_link", "motorway", "motorway_junction", "motorway_link")
  
  if (st_crs(aoi)$units == "us-ft"){
    width = 10
  } else if (st_crs(aoi)$units == "ft"){
    width = 10
  } else if (st_crs(aoi)$units == "m"){
    width = 3.048
  } 
  
  road_vectors <- road_vectors %>% 
    select(highway, lanes) %>% 
    left_join(lanes, by = "highway") %>% 
    mutate(lanes = coalesce(lanes, avg.lanes)) 
  
  road_vectors <- road_vectors %>% 
    st_buffer(dist = road_vectors$lanes * (width / 2),
              endCapStyle = "FLAT",
              joinStyle = "MITRE") %>% 
    st_crop(albedo)
  
  
  if (potential == "technical"){
    high_traff <- road_vectors %>% 
      filter(highway %in% high) %>% 
      rasterize(albedo, field = 1, background = 0)
    high_area <- global(high_traff, fun = function(x, ...) sum(x == 1, na.rm = TRUE))[,1] 
    
    low_traff <- road_vectors %>% 
      filter(!(highway %in% high)) %>% 
      rasterize(albedo, field = 1, background = 0)
    low_traff <- mask(low_traff, high_traff, maskvalues = 1)
    low_area <- global(low_traff, fun = function(x, ...) sum(x == 1, na.rm = TRUE))[,1] 
    
    updated_alb <- ifel(low_traff, 0.195, albedo)
    updated_alb <- ifel(high_traff, 0.305, updated_alb)
    
    area <- c("high_area" = high_area, "low_area" = low_area)
    
  } else if (potential == "achievable"){
    roads <- road_vectors %>% 
      rasterize(albedo, field = 1, background = 0)
    area = c("all_area" = global(roads, fun = function(x, ...) sum(x == 1, na.rm = TRUE))[,1]) 
    
    updated_alb <- ifel(roads, achievable_target, albedo)
  } else if (potential == "program"){
    high_traff <- road_vectors %>% 
      filter(highway %in% high) %>% 
      rasterize(albedo, field = 1, background = 0)
    high_area <- global(high_traff, fun = function(x, ...) sum(x == 1, na.rm = TRUE))[,1] 
    
    low_traff <- road_vectors %>% 
      filter(!(highway %in% high)) %>% 
      rasterize(albedo, field = 1, background = 0)
    low_traff <- mask(low_traff, high_traff, maskvalues = 1)
    low_area <- global(low_traff, fun = function(x, ...) sum(x == 1, na.rm = TRUE))[,1] 
    
    updated_alb <- ifel(low_traff, 0.195, albedo)
    area <- c("low_area" = low_area)
  }
  
  
  writeRaster(updated_alb, here(scenario_path, glue("albedo__reflective-pavements__{potential}-potential.tif")), 
              overwrite = TRUE)
  return(area)
}

achievable_target = 0.17

x = reflective_pavements(aoi = aoi, city_folder = city_folder, potential = "program")
