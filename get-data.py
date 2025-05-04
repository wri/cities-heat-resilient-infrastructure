######################
# Setup
######################

## Inputs
# city = "ZAF-Cape_Town"
# aoi_file = "https://wri-cities-heat.s3.us-east-1.amazonaws.com/ZAF-Cape_Town/processed/citycentre_roi.geojson"
# year = "2024"


def get_data(city, aoi_file, year, output_base="."):
  # Create city folder
  import os
  city_dir = os.path.join(output_base, "data", city)
  os.makedirs(city_dir, exist_ok=True)
    
  # path = f"./data/{city}"
  # if not os.path.isdir(path):
  #    os.makedirs(path)
  
  ## Get the area of interest
  import geopandas as gpd
  aoi = gpd.read_file(aoi_file).to_crs(4326)
  
  ## Create bounding box
  from city_metrix.layers.layer_geometry import GeoExtent
  bbox = GeoExtent(aoi.total_bounds, aoi.crs.srs).as_utm_bbox()
  bbox.to_file(os.path.join(city_dir, "bbox.geojson"))
  
  ## save aoi with UTM crs
  aoi.to_crs(bbox.crs).to_file(os.path.join(city_dir, "aoi.geojson"))
  
  
  ## Setup Earth Engine
  import ee
  ee.Authenticate(auth_mode="notebook")
  ee.Initialize(project='wri-earthengine')
  
  #TODD: Get UTM from OpenUrban and resample others
  #TODO: add projection to all downloads
  
  # ######################
  # # Get the tree canopy height
  # ######################
  # from city_metrix.layers import TreeCanopyHeight
  # city_TreeCanopyHeight = TreeCanopyHeight().get_data(bbox)
  # 
  # ## Write raster to tif file
  # city_TreeCanopyHeight.rio.to_raster(raster_path=os.path.join(city_dir, "tree-canopy-height.tif"))
  
  ######################
  # Get OpenUrban
  ######################
  import open_urban
  
  from open_urban import OpenUrban
  lulc = OpenUrban().get_data(bbox)
  
  ## Save data to file
  lulc.rio.to_raster(raster_path=os.path.join(city_dir, "open-urban.tif"))
  
  
  ######################
  # Get roads
  ######################
  lulc_collection = ee.ImageCollection('projects/wri-datalab/cities/OpenUrban/OpenUrban_LULC') \
      .filterBounds(bbox.to_ee_rectangle()['ee_geometry'])
  
  tiles = lulc_collection.aggregate_array("grid_cell").getInfo()
  
  import geopandas as gpd
  import pandas as pd
  
  road_paths = [
      f"https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}/roads/roads_{tile}.geojson"
      for tile in tiles
  ]
  
  road_vectors = pd.concat([gpd.read_file(url) for url in road_paths], ignore_index=True)
  
  # Save to json file
  road_vectors.to_file(os.path.join(city_dir, "roads.geojson"), driver='GeoJSON')
  
  
  
  ######################
  # Get lanes
  ######################
  lanes = pd.read_csv(f"https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}/roads/average_lanes.csv")
  
  # Save to csv file
  lanes.to_csv(os.path.join(city_dir, "average-lanes.csv"), index=False)
  
  
  
  # Get buildings
  ######################
  build_paths = [
      f"https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}/buildings/buildings_{tile}.geojson"
      for tile in tiles
  ]
  
  build_vectors = pd.concat([gpd.read_file(url) for url in build_paths], ignore_index=True)
  
  # Save to json file
  build_vectors.to_file(os.path.join(city_dir, "buildings.geojson"), driver='GeoJSON')
  
  ######################
  # Get albedo
  ######################
  centroid_lat = aoi.geometry.union_all().centroid.y

  if centroid_lat < 0:
      # Southern Hemisphere
      summer_start = f"{year}-12-01"
      summer_end = f"{int(year) + 1}-02-28"
  else:
      # Northern Hemisphere
      summer_start = f"{year}-06-01"
      summer_end = f"{int(year) + 1}-08-31"

    
  from city_metrix.layers import Albedo
  city_Albedo = Albedo(start_date=summer_start, end_date=summer_end).get_data(bbox)
  
  ## Write raster to tif file
  city_Albedo.rio.to_raster(raster_path=os.path.join(city_dir, "albedo.tif"))
  
  
  # Get parks
  ######################
  openspace_paths = [
      f"https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/{city}/openspace/openspace_{tile}.geojson"
      for tile in tiles
  ]
  
  openspace_vectors = pd.concat([gpd.read_file(url) for url in openspace_paths], ignore_index=True)
  
  # Save to json file
  openspace_vectors.to_file(os.path.join(city_dir, "openspaces.geojson"), driver='GeoJSON')
  
  
  
  
  
  ######################
  # Create scenario folder
  ######################
  import os
  os.makedirs(os.path.join(city_dir, "scenarios"), exist_ok=True)
