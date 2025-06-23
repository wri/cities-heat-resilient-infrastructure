
import rasterio

import os

output_base="."
city = "ZAF-Cape_Town"
city_dir = os.path.join(output_base, "data", city)

ctcm_setup_path = os.path.join("C:\\", "CTCM_data_setup")
scenario_name = "achievable-90pctl"
run_setup_folder = os.path.join(ctcm_setup_path, f"{city}-street-trees-{scenario_name}")
tile_folder = os.path.join(run_setup_folder, "primary_data", "raster_files", "tile_001")

trees = os.path.join(tile_folder, "tree_canopy.tif")
open_urban = os.path.join(tile_folder, "open-urban.tif")
dem = os.path.join(tile_folder, "cif_dem.tif")
dsm = os.path.join(tile_folder, "cif_dsm_ground_build.tif")
lulc = os.path.join(tile_folder, "cif_lulc.tif")
flip = os.path.join(tile_folder, "open-urban-flipped.tif")

dataset = rasterio.open(flip)

# crs = dataset.crs.to_string()
# width = dataset.profile["width"]
# height = dataset.profile["height"]
# no_data = dataset.nodata if dataset.nodata is not None else ~sys.maxsize
bounds = dataset.bounds

# band1 = dataset.read(1)
# band_min = band1.min()
# band_max = band1.max()

print(bounds)


            
