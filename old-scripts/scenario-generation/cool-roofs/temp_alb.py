# temp ~ alb
# Santamouris and Fiorito (2021) https://www.sciencedirect.com/science/article/pii/S0038092X21000475#e0015
# delta_T = -0.261 + 0.935(delta_albedo) + 0.01(perc_green) + 0.013(perc_streets) + -0.000014(pop_density)
# valid for starting albedo between 0.12 and 0.2, and finishing albedo between 0.25 and 0.7, and albedo increase between 0.1 and 0.5

# temporal scaling: model value is for 17:00
# scaling from Kreyenhof et al (2021) https://iopscience.iop.org/article/10.1088/1748-9326/abdcf1/meta
# values from fig 8, reflective roof WRF-BEP (better for 2m height temperature, per section 5.1.3)

###### PACKAGE IMPORTS ######
import numpy as np
import rasterio
import matplotlib.pyplot as plt

###### IMPORTS & EXPORTS TO CHANGE ######

# before albedo
with rasterio.open('/path/to/large-building-cool-roof.tif') as src:
    start_alb = src.read()

# after albedo
with rasterio.open('/path/to/large-building-cool-roof.tif') as src:
    end_alb = src.read()

# # land use
# with rasterio.open('/path/to/landcover_nozero.tif') as src:
#     lulc = src.read()

# # tree canopy
# with rasterio.open('/path/to/canopyheight.tif') as src:
#     canopyheight = src.read()

# # worldpop (people/100m)
# with rasterio.open('/path/to/worldpop.tif') as src:
#     worldpop = src.read()

# export location
out_dir = '/path/to/export/folder/'

###### CALCULATE INDEPENDENT VARS ######

# albedo change
start_mean_alb = start_alb.mean()
end_mean_alb = end_alb.mean()
delta_alb = end_mean_alb - start_mean_alb
print(start_mean_alb)
print(end_mean_alb)
print(delta_alb)

# # green space
# green = (lulc*(lulc==5))+canopyheight
# perc_green = ((np.count_nonzero(green))/(green.size))*100

# # streets
# streets = (lulc)*(lulc==1)
# perc_streets = ((np.count_nonzero(streets))/(streets.size))*100

# # pop density (people/km2)
# pop_density = (worldpop.mean())*100


###### CALCULATE 17:00 TEMP CHANGE ######
# original formula calculates decrease in temp
# added negative value to signal temp reduction (overall change in temp)

# dT_17 = -(-0.261 + 0.935*(delta_alb) + 0.01*(perc_green) + 0.013*(perc_streets) + -0.000014*(pop_density))


###### CALCULATE TEMP CHAGE ######
# from Krayenhoff et al. 2021 DOI 10.1088/1748-9326/abdcf1

# 0.1 increase in albedo results in a 0.6 C reduction in air temp for midday clear-sky conditiosn

#noon dT
dT = 6(delta_alb)


###### ADJUST TO 12:00, 15:00, 18:00 TEMP CHANGE ######

# dT_12 = 1.297052154*(dT_17)
# dT_15 = 1.213151927*(dT_17)
# dT_18 = 0.839002268*(dT_17)

dT_12 = dT
dT_15 = 0.935315*(dT)
dT_18 = 0.646853*(dT)

out_arr = np.array([[12, dT_12], 
					[15, dT_15], 
					[18, dT_18]])

###### EXPORT ######
np.savetxt(out_dir+'Ta_change.txt', out_arr, delimiter=',',fmt='%1.2f')
