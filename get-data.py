######################
# Setup
######################

## Inputs
city = "BRA-Rio_de_janeiro"
aoi_file = "https://wri-cities-heat.s3.us-east-1.amazonaws.com/BRA-Rio_de_janeiro/raw/boundaries/BRA-Rio_de_janeiro-DBE_low_emission_zone.geojson"

# Create city folder
import os
path = f"./data/{city}"
if not os.path.isdir(path):
   os.makedirs(path)

## Get the area of interest
import geopandas as gpd
aoi = gpd.read_file(aoi_file)

## Create bounding box
from city_metrix.layers.layer_geometry import GeoExtent
bbox = GeoExtent(aoi.total_bounds, aoi.crs.srs).as_utm_bbox()


## save aoi with UTM crs
aoi.to_crs(bbox.crs).to_file(f"./data/{city}/aoi.geojson")


## Setup Earth Engine
import ee
ee.Authenticate(auth_mode="notebook")
ee.Initialize(project='wri-earthengine')

#TODD: Get UTM from OpenUrban and resample others
#TODO: add projection to all downloads

######################
# Get the tree canopy height
######################
from city_metrix.layers import TreeCanopyHeight
city_TreeCanopyHeight = TreeCanopyHeight().get_data(bbox)

## Write raster to tif file
city_TreeCanopyHeight.rio.to_raster(raster_path=f"./data/{city}/tree-canopy-height.tif")

######################
# Get OpenUrban
######################
import open_urban

from open_urban import OpenUrban
lulc = OpenUrban().get_data(bbox)

## Save data to file
lulc.rio.to_raster(raster_path=f"./data/{city}/open-urban.tif")


######################
# Get roads
######################
lulc_collection = ee.ImageCollection('projects/wri-datalab/cities/OpenUrban/OpenUrban_LULC') \
    .filterBounds(bbox.to_ee_rectangle()['ee_geometry'])

tiles = lulc_collection.aggregate_array("grid_cell").getInfo()

import geopandas as gpd
import pandas as pd

road_paths = [
    f"https://wri-cities-heat.s3.us-east-1.amazonaws.com/{city}/vector-data/roads/roads_{tile}.geojson"
    for tile in tiles
]

road_vectors = pd.concat([gpd.read_file(url) for url in road_paths], ignore_index=True)

# Save to json file
road_vectors.to_file(f"./data/{city}/roads.geojson", driver='GeoJSON')



######################
# Get lanes
######################
lanes = pd.read_csv("https://wri-cities-heat.s3.us-east-1.amazonaws.com/MEX-Monterrey/vector-data/roads/average_lanes.csv")

# Save to csv file
lanes.to_csv(f"./data/{city}/average-lanes.csv", index=False)



######################
# Create scenario folder
######################
import os
os.makedirs(f"./data/{city}/scenarios", exist_ok=True)
