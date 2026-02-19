# Load data for entire city
  # aoi.geojson
  # roads.geojson
  # average_lanes.csv

# For each tile
  # Create binary tree cover
  # Process to individual trees
    # tree points, crown vectors, crowns raster
  # Create pedestrian area
  # Create plantable area
    # What about intersections that are in within 9-meters of tile boundary?
    # What about the buildings buffer?

# Combine individual trees into population for entire city
  # crown vectors
  # crown rasters
  # create tree_structure (small-25th, med-50th, large-75th percentile)
  # filter crown vectors to small, med, and large?
  # mask crown rasters with filtered vectors?

# Create 100-m grid over entire city and intersect with boundary

  # For each gridcell:
    # intersect with tile grid to get tile numbers
  
    # Load, combine, and crop data:
      # lulc.tif
      # canopy_height.tif
      # ped_roads.geojon
      # plantable_area.tif
      # pedestrian_area.tif
      # tree points
    
    # generate new trees
      # new tree points
      # updated tree raster
      # tree diff raster

# Combine gridcell outputs for tiles

  # For each tile:
    # intersect with 100-m grid to get gridcell numbers

    # Load, combine, and crop data:
      # updated tree raster

    # Create updated binary tree cover

# Combine all new tree points