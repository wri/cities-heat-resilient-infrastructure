# aoi <- st_read(here(inputs_path, "aoi.geojson"))
get_aoi_utm <- function(aoi) {
  city_centroid <- aoi %>%
    st_transform(4326) %>%
    st_centroid() %>% 
    st_coordinates()
  
  utm_epsg <- gfcanalysis::utm_zone(y = city_centroid[2], x = city_centroid[1], proj4string = TRUE) %>% 
    str_sub(12) %>% 
    as.numeric()
  
  utm_ee <- paste0("EPSG:", utm_epsg)
  
  rm(city_centroid)
  
  return(list(epsg = utm_epsg,
              ee = utm_ee))
}

  