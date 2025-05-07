import ee
#from city_metrix.metrix_model import Layer
from city_metrix.metrix_model import Layer, GeoExtent, get_image_collection

class OpenUrban():
    def __init__(self, band='b1', **kwargs):
        super().__init__(**kwargs)
        self.band = band
    # TODO: Reproject https://github.com/wri/cities-heat-resilient-infrastructure/blob/main/scenarios/street-trees/street-trees-pct.R#L55C3-L55C12
    def get_data(self, bbox: GeoExtent, spatial_resolution:int=None,
                 resampling_method:str=None):
        dataset = ee.ImageCollection("projects/wri-datalab/cities/OpenUrban/OpenUrban_LULC")
        ## It is important if the cif code is pulling data from GEE to take the maximum value where the image tiles overlap

        # Check for data
        data = None
        ee_rectangle = bbox.to_ee_rectangle()
        if dataset.filterBounds(ee_rectangle['ee_geometry']).size().getInfo() == 0:
            print("No OpenUrban Data Available")
        else:
            open_urban = ee.ImageCollection(dataset
                                     .filterBounds(ee_rectangle['ee_geometry'])
                                     .select(self.band)
                                     .max()
                                     .reduce(ee.Reducer.firstNonNull())
                                     #.reproject(crs = utm_ee, scale = 1)
                                     .rename('lulc')
                                     )

            data = get_image_collection(
                open_urban,
                ee_rectangle,
                1,
                "Open Urban"
            )

        return data

