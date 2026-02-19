library(terra)

# Load raster
r <- rast(here("data", city, "open-urban.tif"))

# Optional: check if y is increasing or decreasing
yres(r)  # should be positive if y increases upward

# Save with explicit settings
writeRaster(
  r,
  "open-urban_fixed.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "TFW=YES")  # Optional: georeferencing + compression
)
