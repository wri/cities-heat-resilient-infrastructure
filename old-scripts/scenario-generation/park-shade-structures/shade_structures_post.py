import rasterio
import numpy as np

# Times of analysis: year, day of year, times

year = '2022'
day = '20'
times = ['1200', '1500', '1800']

# Input folders

no_struct_in_dir = "/directory/to/input/files/baseline/no/structures/"
t0_in_dir = "/directory/to/input/files/transmissivity0/"
t3_in_dir = "/directory/to/input/files/transmissivity3/"

output_dir = "/directory/to/output/files/"

# Loop through 3 times of day
for i in range(0,2):

    time = times[i]

    struct_t3_file = (t3_in_dir+"Sahdow_"+year+"_"+day+"_"+time+"D.tif")
    no_struct_tmrt_file = (no_struct_in_dir+"Tmrt_"+year+"_"+day+"_"+time+"D.tif")
    struct_t0_tmrt_file = (t0_in_dir+"Tmrt_"+year+"_"+day+"_"+time+"D.tif")
    no_struct_shadow_file = (no_struct_in_dir+"Shadow_"+year+"_"+day+"_"+time+"D.tif")
    struct_t0_shadow_file = (t0_in_dir+"Shadow_"+year+"_"+day+"_"+time+"D.tif")

    with rasterio.open(struct_t3_file) as struct_t3_mask_in, \
         rasterio.open(no_struct_tmrt_file) as no_struct_tmrt_in, \
         rasterio.open(struct_t0_tmrt_file) as struct_t0_tmrt_in, \
         rasterio.open(no_struct_shadow_file) as no_struct_shadow_in, \
         rasterio.open(struct_t0_shadow_file) as struct_t0_shadow_in:

        # Read the data
        struct_t3_mask = struct_t3_mask_in.read(1)
        no_struct_tmrt = no_struct_tmrt_in.read(1)
        struct_t0_tmrt = struct_t0_tmrt_in.read(1)
        no_struct_shadow = no_struct_shadow_in.read(1)
        struct_t0_shadow = struct_t0_shadow_in.read(1)

        # Get metadata from one of the inputs for output
        meta = no_struct_tmrt.meta.copy()

        # Create a mask where transmissivity = 3 shadow layer has values between 0 and 1
        # Which means it's isolating the "tree shade" where trees are shade structures
        mask = (struct_t3_mask >= 0) & (struct_t3_mask <= 1)

        # Create composite output files where the shade structure Tmrt and shadows
        # are merged with the baselie data
        output_trmt_data = np.where(mask, struct_t0_tmrt, no_struct_tmrt)
        output_shadow_data = np.where(mask, struct_t0_shadow, no_struct_shadow)

        # Write the result to a new file
        with rasterio.open(output_dir+"/"+time+"/", 'w', **meta) as dst:
            dst.write(output_tmrt_data, 1)
            dst.write(output_shadow_data, 1)

    print("Composite rasters written to:", output_dir)
