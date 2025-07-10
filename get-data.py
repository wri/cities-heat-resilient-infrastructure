######################
# Setup
######################

## Inputs
# city = "BRA-Rio_de_Janeiro"
# aoi_file = "https://wri-cities-heat.s3.us-east-1.amazonaws.com/BRA-Rio_de_janeiro/raw/boundaries/BRA-Rio_de_janeiro-DBE_low_emission_zone.geojson"
# year = "2024"
# buffer = 100

def get_data(city, city_folder, aoi_file, buffer, year, output_base="."):
  # Create city folder
  import os
  city_dir = os.path.join(output_base, "data", city_folder)
  os.makedirs(city_dir, exist_ok=True)
    
  # path = f"./data/{city}"
  # if not os.path.isdir(path):
  #    os.makedirs(path)
  
  ## Get the area of interest
  import geopandas as gpd
  import pandas as pd
  
  aoi = gpd.read_file(aoi_file).to_crs(4326)
  
  ## Create bounding box
  from city_metrix.metrix_model import GeoExtent
  from city_metrix.metrix_tools import reproject_units
  import rioxarray
  from shapely.geometry import Polygon
  
  raster = rioxarray.open_rasterio(os.path.join(city_dir, "cif_lulc.tif"), masked=True)
  bounds = raster.rio.bounds()
  crs = raster.rio.crs.to_string()
  
  from src.worker_manager.tools import construct_polygon_from_bounds
  bbox_poly = construct_polygon_from_bounds(bounds[0], bounds[1], bounds[2], bounds[3])
  tile_gpd = gpd.GeoDataFrame(index=[0], crs=crs, geometry=[bbox_poly])
  bbox = GeoExtent(bbox=tile_gpd, crs=crs)
  
  ## save aoi with UTM crs
  aoi.to_crs(crs).to_file(os.path.join(city_dir, "boundaries.geojson"))
  aoi.to_crs(crs).to_file(os.path.join(city_dir, "scenarios", "baseline", "boundaries.geojson"))
  
  
  ## Setup Earth Engine
  import ee
  ee.Authenticate(auth_mode="notebook")
  ee.Initialize(project='wri-earthengine')
  
  ######################
  # Get roads
  ######################
  lulc_collection = ee.ImageCollection('projects/wri-datalab/cities/OpenUrban/OpenUrban_LULC') \
      .filterBounds(bbox.to_ee_rectangle()['ee_geometry'])
  
  tiles = lulc_collection.aggregate_array("grid_cell").getInfo()
  
  
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

    
  from city_metrix.layers import AlbedoCloudMasked
  city_Albedo = AlbedoCloudMasked(start_date=summer_start, end_date=summer_end).get_data(bbox)
  
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
