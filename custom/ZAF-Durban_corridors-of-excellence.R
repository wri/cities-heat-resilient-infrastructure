library(here)
library(terra)
library(sf)
library(tidyverse)
library(glue)

source(here("tiling-scripts", "utils.R"))
source(here("tiling-scripts", "n-trees-uniform.R"))
source(here("tiling-scripts", "trees-functions.R"))

city <- "ZAF-Durban"
aoi_name <- "inner_city_lap"
project_name <- "corridors_of_excellence"

infra <- "trees"
scenario <- "custom-n_trees_uniform"
scenario_folder <- glue("city_projects/{city}/{project_name}/scenarios/{infra}/{scenario}")
baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")

bucket   <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
open_urban_aws_http <- paste0("https://wri-cities-heat.s3.us-east-1.amazonaws.com/OpenUrban/", city)
s3 <- paws::s3()

buffered_tile_grid <- st_read(
  paste0(aws_http, "/", glue("{baseline_folder}/metadata/.qgis_data/tile_grid.geojson")),
  quiet = TRUE
) 

utm <- st_crs(buffered_tile_grid)

aoi <- st_read(glue("https://wri-cities-heat.s3.us-east-1.amazonaws.com/{city}/{city}__{aoi_name}.geojson")) %>% 
  st_transform(utm)

roads <- st_read(glue("{aws_http}/city_projects/{city}/{project_name}/scenarios/baseline/baseline/corridors_of_excellence.geojson")) %>%
  st_transform(utm)

roads_buff <- roads %>% 
  st_buffer(dist = 5, endCapStyle = 'SQUARE') %>% 
  st_union()

write_s3(roads_buff, glue("{bucket}/city_projects/{city}/{project_name}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson"))

buffered_tile_grid <- buffered_tile_grid %>% 
  st_filter(roads_buff)

tile_grid <- st_read(
  paste0(aws_http, "/", glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline/metadata/.qgis_data/unbuffered_tile_grid.geojson")),
  quiet = TRUE
) |>
  st_filter(roads_buff)

tiles <- buffered_tile_grid$tile_name

# Copy existing tree canopy tiles into scenario folder to be updated
tree_canopy_paths <- glue("{baseline_folder}/{tiles}/raster_files/cif_tree_canopy.tif")
scenario_tree_canopy_paths <- glue("{scenario_folder}/{tiles}/raster_files/tree_canopy.tif")

map(glue("{scenario_folder}/{tiles}/raster_files/"), ~ ensure_s3_prefix(bucket, .x))
s3_copy_vec(from = tree_canopy_paths, to = scenario_tree_canopy_paths, 
            from_bucket = bucket, to_bucket = bucket, overwrite = TRUE)

# Create plantable area
for (t in tiles){
  lulc <- rast(glue("{aws_http}/{baseline_folder}/{t}/raster_files/cif_open_urban.tif"))
  pedestrian_area <- rast(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/pedestrian-areas__baseline__baseline.tif"))
  binary_tree_cover <- rast(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/tree-cover__baseline__baseline.tif"))
  plantable_area <- create_plantable_area_all_roads(lulc, pedestrian_area, binary_tree_cover, 
                                                    open_urban_aws_http, t,
                                                    intersection_buff = 9)
  
  ensure_s3_prefix(bucket, glue("{scenario_folder}/{t}/ccl_layers/"))
  write_s3(plantable_area, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/plantable-areas__trees__{scenario}.tif"))
}


# Load tree population data

trees <- st_read(glue("{aws_http}/city_projects/{city}/{aoi_name}/scenarios/trees/tree-population/trees.geojson")) %>% 
  st_transform(utm)
crowns <- st_read(glue("{aws_http}/city_projects/{city}/{aoi_name}/scenarios/trees/tree-population/crowns.geojson")) %>% 
  st_transform(utm)
tree_structure <- read_csv(glue("{aws_http}/city_projects/{city}/{aoi_name}/scenarios/trees/tree-population/tree-structure.csv"))

aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"
bucket   <- "wri-cities-tcm"

# pg <- roads %>%
#   filter(name == "Prince/Gillepsie Street") %>%
#   st_buffer(dist = 5, endCapStyle = 'SQUARE')
# ab <- roads %>% filter(name == "AB Xuma/Monty Naicker Street") %>%
#   st_buffer(dist = 5, endCapStyle = 'SQUARE') %>%
#   st_difference(pg)
# dr <- roads %>% filter(name == "Dr. Pixley KaSeme Street") %>%
#   st_buffer(dist = 5, endCapStyle = 'SQUARE') %>%
#   st_difference(st_union(pg, ab))


trees_aug <- trees
all_new <- list()

for (c in roads$name){
  
  aoi <- roads %>% 
    filter(name == c) %>% 
    st_buffer(dist = 5, endCapStyle = 'SQUARE')
  
  n_trees <- aoi$n_trees
  
  aoi_grid <- aoi %>% 
    st_make_grid(cellsize = c(100, 100), square = TRUE, what = "polygons") %>% 
    st_sf() %>% 
    st_intersection(aoi) %>% 
    st_filter(tile_grid) %>% 
    select(geometry) %>% 
    mutate(ID = row_number())
  
  new_trees <- plant_trees_uniform_over_ped_area(
    n_total         = n_trees,   
    aoi_grid        = aoi_grid,
    min_dist        = 5,
    trees           = trees_aug,
    crowns          = crowns,
    tree_structure  = tree_structure
  )
  
  if (!is.null(new_trees) && nrow(new_trees) > 0) {
    
    # Ensure CRS match
    new_trees <- sf::st_transform(new_trees, sf::st_crs(trees_aug))
    
    # ✅ accumulate ONLY new trees
    all_new[[length(all_new) + 1L]] <- new_trees
    
    # 🚫 augment blocker layer (not saved)
    trees_aug <- bind_rows(trees_aug, new_trees)
  }
  
  
}

updated_trees <- bind_rows(all_new)

write_s3(
  updated_trees,
  glue::glue("{bucket}/{scenario_folder}/new-tree-points__trees__{scenario}.geojson")
)

# Copy final canopy to scenario folder
for (t in tiles){
  # updated tree cover
  updated_tree <- rast_retry(glue("{aws_http}/{scenario_folder}/{t}/raster_files/tree_canopy.tif"))
  updated_tree_cover <- updated_tree >= 3
  write_s3(updated_tree_cover, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/tree-cover__trees__{scenario}.tif"))
  
  # new tree cover
  existing_tree <- rast_retry(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/tree-cover__baseline__baseline.tif"))
  new_tree <- updated_tree_cover - existing_tree
  write_s3(new_tree, glue("{bucket}/{scenario_folder}/{t}/ccl_layers/new-tree-cover__trees__{scenario}.tif"))
}


x <- load_and_merge(glue(
  "{aws_http}/{scenario_folder}/{tiles}/ccl_layers/new-tree-cover__trees__{scenario}.tif"
))
plot(x)

y <- load_and_merge(glue(
  "{aws_http}/{scenario_folder}/{tiles}/raster_files/tree_canopy.tif"))
plot(y)

z <- load_and_merge(glue(
  "{aws_http}/{scenario_folder}/{tiles}/ccl_layers/tree-cover__trees__{scenario}.tif"))

q <- st_read(glue("{aws_http}/{scenario_folder}/new-tree-points__trees__{scenario}.geojson"))
plot(q$geometry)
