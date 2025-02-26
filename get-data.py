# Setup
## Inputs
city = "BRA-Rio_de_janeiro"
aoi_file = "https://wri-cities-heat.s3.us-east-1.amazonaws.com/BRA-Rio_de_janeiro/raw/boundaries/BRA-Rio_de_janeiro-DBE_low_emission_zone.geojson"

## Get the area of interest
import geopandas as gpd
aoi = gpd.read_file(aoi_file)


## Create bounding box
from city_metrix.layers.layer_geometry import GeoExtent
bbox = GeoExtent(aoi.total_bounds, aoi.crs.srs).as_utm_bbox()


## save aoi with UTM crs
aoi.to_crs(bbox.crs).to_file(f"./data/{city}/{city}-aoi.geojson")


## Setup Earth Engine
import ee
ee.Authenticate()
ee.Initialize(project='wri-earthengine')


# Get the tree canopy height
from city_metrix.layers import TreeCanopyHeight
city_TreeCanopyHeight = TreeCanopyHeight().get_data(bbox)

## Write raster to tif file
city_TreeCanopyHeight.rio.to_raster(raster_path=f"./data/{city}/{city}_TreeCanopyHeight.tif")



# Get OpenUrban
from importlib import reload
import open_urban
importlib.reload(open_urban)

from open_urban import OpenUrban, reclass_map
lulc = OpenUrban().get_data(bbox)

# Reclassify
from xrspatial.classify import reclassify
lulc_to_solweig_class = reclassify(lulc, bins=list(reclass_map.keys()), new_values=list(reclass_map.values()), name='lulc')

# Remove zeros
def count_occurrences(data, value):
    return data.where(data == value).count().item()
  
remove_value = 0
count = count_occurrences(lulc_to_solweig_class, remove_value)
if count > 0:
    print(f'Found {count} occurrences of the value {remove_value}. Removing...')
    lulc_to_solweig_class = lulc_to_solweig_class.where(lulc_to_solweig_class != remove_value, drop=True)
    count = _count_occurrences(lulc_to_solweig_class, remove_value)
    print(f'There are {count} occurrences of the value {remove_value} after removing.')
else:
    print(f'There were no occurrences of the value {remove_value} found in data.')

# TODO Can we specify resolution through GEE and avoid below?
if output_resolution != DEFAULT_LULC_RESOLUTION:
    lulc_to_solweig_class = _resample_categorical_raster(lulc_to_solweig_class, output_resolution)

# reverse y direction, if y values increase in NS direction from LL corner
from city_metrix.layers.layer_tools import standardize_y_dimension_direction
was_reversed, lulc_to_solweig_class = standardize_y_dimension_direction(lulc_to_solweig_class)

# Save data to file
lulc_to_solweig_class.rio.to_raster(raster_path=f"./data/{city}/{city}_OpenUrban.tif")
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
lanes.to_csv(f"./data/{city}/lanes.csv", index=False)



######################
# Create scenario folder
######################
import os
os.makedirs(f"./data/{city}/scenarios", exist_ok=True)
