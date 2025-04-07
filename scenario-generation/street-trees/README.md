# Code for generating street tree scenarios.

## Get Data

See the main project readme

## Run

1.  Open `street-tree-potentials.R`
2.  Update `city` variable
3.  Choose a descriptive name for your scenario.
4.  Run the street trees scenario function with your desired parameters.
    -   To generate the technical potential (maximum coverage of plantable area):

        -   scenario = "technical", percentile = NULL, target_coverage = NULL

    -   To generate the achievable potential, choose the percentile value of the distribution of existing tree cover in pedestrian areas:

        -   scenario = "achievable", percentile = 0.90, target_coverage = NULL

    -   To run a program scenario, choose the target percent of cover for pedestrian areas:

        -   scenario = "program", percentile = NULL, target_coverage = 0.5
5.  Select the code in lines 1-48 and run. The output data will be saved in a folder
named for the scenario; the new tree height raster is named "scenario-tree-canopy-height.tif".

## Metrics

To calculate metrics after having run the thermal comfort model, add the resulting 
Tmrt and Shadow rasters to the scenario folder making sure not to change the filenames. 
The met data file should be in the "scenarios" folder of the city. This will also
convert mrt to utci and save the rasters if this hasn't already been done.
