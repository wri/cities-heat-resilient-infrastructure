generate_plantable_street <- function(aoi, lulc_rast, existing_trees, road_vectors, lanes, city, utm, save_files = FALSE) {
  
  # Roads -------------------------------------------------------------------
  
  roads_raster <- lulc_rast == 500
  roads_raster <- roads_raster %>% 
    subst(0, NA)
  
  
  # Load roads and filter to bbox of AOI
  road_vectors <- road_vectors %>% 
    st_transform(utm$epsg) %>% 
    filter(sapply(geometry, function(geom) st_intersects(geom, st_as_sfc(st_bbox(aoi)), sparse = FALSE))) 
  
  ped_roads_list <- c("tertiary",
                      "tertiary_link",
                      "residential",
                      "living_street")
  
  
  road_vectors <- road_vectors %>% 
    left_join(lanes, by = "highway") %>% 
    mutate(lanes = coalesce(lanes, avg.lanes))
  
  ped_road_vectors <- road_vectors %>% 
    filter(highway %in% ped_roads_list)
  
  if (save_files){
    
    st_write(road_vectors, here("data", city, "scenarios", "street-trees", "roads_lines.geojson"))
    st_write(ped_road_vectors, here("data", city, "scenarios", "street-trees", "ped_roads_lines.geojson"))
    
  }
  
  # Buffer roads by lanes * 10 ft (3.048 m) 
  # https://nacto.org/publication/urban-street-design-guide/street-design-elements/lane-width/#:~:text=wider%20lane%20widths.-,Lane%20widths%20of%2010%20feet%20are%20appropriate%20in%20urban%20areas,be%20used%20in%20each%20direction
  # cap is flat to the terminus of the road
  # join style is mitred so intersections are squared
  if (st_crs(aoi)$units == "us-ft"){
    width = 10
  } else if (st_crs(aoi)$units == "ft"){
    width = 10
  } else if (st_crs(aoi)$units == "m"){
    width = 3.048
  } 
  
  road_buff <- road_vectors %>% 
    st_buffer(dist = road_vectors$lanes * (width / 2),
              endCapStyle = "FLAT",
              joinStyle = "MITRE") 
  
  if (save_files){
    st_write(road_buff, here("data", city, "scenarios", "street-trees", "road_areas.geojson"))
  }
  
  ped_roads <- road_buff %>% 
    filter(highway %in% ped_roads_list)  
  
  if (save_files){
    st_write(ped_roads, here("data", city, "scenarios", "street-trees", "ped_roads_areas.geojson"))
  }
  
  ped_roads_raster <- rasterize(ped_roads, lulc_rast, field = 1, background = 0)
  
  if (save_files){
    writeRaster(ped_roads_raster, here("data", city, "scenarios", "street-trees", "ped-roads-raster.tif"))
  }
  
  
  # Plantable area ----------------------------------------------------------
  
  
  
  ## Pixels adjacent to streets ####
  # set 0's to NA to create a buffer of only roads
  # streets_NA <- subst(streets, 0, NA)
  
  # 5-meter buffer 
  ped_road_adjacent <- ped_roads_raster %>% 
    subst(0, NA) %>% 
    buffer(5) %>% 
    as.numeric()
  
  ped_road_adjacent <- ped_road_adjacent - ped_roads_raster  
  
  ## Buildings buffer ####
  # buildings buffer 5-m
  builds <- floor(lulc_rast / 100) == 6
  
  builds_buff <- builds %>% 
    subst(0, NA) %>% 
    buffer(5)
  
  builds_buff <- abs(builds_buff - builds - 1) 
  
  ## intersections buffer ####
  # no trees within 9-m of intersection
  
  # Dissolve road segments by a unique road identifier (e.g., 'road_id' or 'name')
  dissolved_roads <- st_union(road_vectors)
  
  # Convert the dissolved result back into an sf object
  dissolved_roads_sf <- st_as_sf(data.frame(geometry = dissolved_roads)) %>% 
    st_cast("LINESTRING")
  
  # Find intersections of the dissolved roads
  intersections <- st_intersection(dissolved_roads_sf)
  
  # Keep only the points where roads intersect
  intersection_points <- intersections[st_geometry_type(intersections) == "POINT", ] %>% 
    st_union() %>% 
    st_as_sf() %>% 
    st_cast("POINT")
  
  intersection_buffer <- intersection_points %>% 
    st_buffer(dist = 9) %>% 
    rasterize(lulc_rast, field = 0, background = 1)
  
  ## Plantable area ####
  # green space, built up other, barren, open space can be planted
  # water, roads, building, parking cannot
  plantable_lulc <- floor(lulc_rast / 100) %in% c(1, 2)
  
  # remove building buffer ####
  plantable_lulc <- plantable_lulc * builds_buff
  
  # remove intersections buffer ####
  plantable_lulc <- plantable_lulc * intersection_buffer
  
  # Street plantable area
  plantable_street <- plantable_lulc * ped_road_adjacent
  
  # Remove areas of existing tree cover
  plantable_street <- plantable_street * (existing_trees < 1)
  
  # Pedstrian area (not building, road, water)
  ped_area <- ped_road_adjacent * abs((floor(lulc_rast / 100) %in% c(3, 5, 6)) - 1)
  
  # Save raster as Cloud-Optimized GeoTIFF (COG)
  if (save_files){
    writeRaster(plantable_street, here("data", city, "scenarios", "street-trees", "plantable-street.tif"), overwrite = TRUE)
    writeRaster(ped_area, here("data", city, "scenarios", "street-trees", "ped_area.tif"))
  }
  
  return(list(plantable_street = plantable_street,
              ped_area = ped_area))
  
}

