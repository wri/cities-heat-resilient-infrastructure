load(here("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/scenarios/street-trees", "tree-vars.RData"))
crowns <- rast(here("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/scenarios/street-trees", "existing-tree-crowns.tif"))
infrastructure_path <- here("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/scenarios/park-trees")


baseline_tree <- (rast(here("data", city_folder, "cif_tree_canopy.tif")) ) %>% 
  # subst(0, NA) %>% 
  crop(aoi)
parks_rast <- parks %>% rasterize(baseline_tree)

source(here("scenario-generation", "street-trees", "tree-generating-functions.R"))
existing_tree_points = st_read("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/scenarios/baseline/tree_points_baseline.geojson")

updated <- generate_trees(
  plantable_area = parks_rast,
  ped_area = parks_rast,
  existing_tree_cover = baseline_tree,
  existing_tree_points = existing_tree_points,
  tree_structure = tree_structure,
  tree_height = NULL,
  target_coverage = 0.295,
  min_dist = 2,
  crown_vectors = crown_vectors,
  crown_raster = crowns,
  infrastructure_path = infrastructure_path,
  city_folder = city_folder
)

writeRaster(updated$updated_tree_cover, here(infrastructure_path, "achievable-90pctl", "updated-tree-canopy.tif"), overwrite = TRUE)
st_write(updated$new_tree_pts, here(infrastructure_path, "achievable-90pctl", "new-tree-points.geojson"))
