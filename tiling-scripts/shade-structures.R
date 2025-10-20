# Shade structures

# Load data for entire city
  # aoi
  # parks
    # process areas
      # Remove areas classified as "pitch"
      # dissolve to combine contiguous areas
      # cast to polygon to seperate
      # erase pitches
    # add park id


# For each tile:
  # create binary shade at noon and save

# For each park:
  # intersect with tile grid to get tile ids
  # Load, combine, and clip noon shade
  
  # if park is small
    # Calculate percent of shade
    # add structures until 25% is reached
    # save structures as park_id_structures.geojson
  
  # if park is large
    # calculate max or mean distance to shade
    # add structures until distance is <= 50-m
    # save structures as park_id_structures.geojson

# Combine shade structure geojsons (structures__shade-structures__small-parks)
# Create shade structure centroids (structure-points__shade-structures__small-parks)

# For each tile:
  # Filter shade structures to those within tile boundary
  # rasterize to structures-as-trees.tif
  # Only rerun CTCM for tiles that include shade structures*


